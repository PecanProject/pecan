#    PEcAn Parameter Data Assimilation workflow via MCMC
# 
#
#--------------------------------------------------------------------------------#
# functions used to write STATUS used by history
#--------------------------------------------------------------------------------#
options(warn = 1, keep.source = TRUE, error = quote({
  status.end("ERROR")
}))

status.start <- function(name) {
  cat(paste(name, format(Sys.time(), "%F %T"), sep="\t"), file=file.path(settings$outdir, "STATUS"), append=TRUE)      
}

status.end <- function(status="DONE") {
  cat(paste("", format(Sys.time(), "%F %T"), status, "\n", sep="\t"), file=file.path(settings$outdir, "STATUS"), append=TRUE)      
}

#---------------- Load libraries. -----------------------------------------------------------------#
require(PEcAn.all)
#--------------------------------------------------------------------------------------------------#
#
# Code to copy an existing settings file:
# clean.settings("~/demo/demo.xml","~/demo.pda/demo.xml" )
#
#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings("~/demo.pda/demo.xml")
#--------------------------------------------------------------------------------------------------#

# start with a clean status
unlink(file.path(settings$outdir, "STATUS"))

#---------------- Open Database Connection -------------------------------------------------------#
if(settings$database$bety$write){
  con <- try(db.open(settings$database$bety), silent=TRUE)
  if(is.character(con)){
    con <- NULL
  }
} else {
  con <- NULL
}

#---------------- Load Priors ----------------------------------------------------------------------#
status.start("PRIORS")
if(is.null(settings$assim.batch$prior)){
  pft.id =  db.query(paste0("SELECT id from pfts where name = '",settings$pfts$pft$name,"'"),con)
  priors =  db.query(paste0("SELECT * from posteriors where pft_id = ",pft.id),con)
  ## by default, use the most recent posterior as the prior
  settings$assim.batch$prior = priors$id[which.max(priors$updated_at)]
}
prior.db = db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id = ",
                                 settings$assim.batch$prior),con)
prior.db = prior.db[grep("post.distns.Rdata",prior.db$file_name),]
load(file.path(prior.db$file_path,"post.distns.Rdata"))
prior = post.distns
status.end()

#---------------- Define Variables -------------------------------------------------------#
var.names = "Amax"           ## variables to be fit
var.ids = db.query(paste0("SELECT id from variables where name = '",var.names,"'"),con)
var.rows = which(row.names(prior) %in% var.names)
jvar = rep(0.5,22)  ## jump variance
params = NULL       ## MCMC matrix


#--------------- Assimilation -------------------------------------------------------#
status.start("MCMC")
params <- pda.mcmc(settings,prior,var.names=var.names,jvar=jvar,params=params)
status.end()


#--------------- MCMC Post Process -------------------------------------------------------#
status.start("POSTERIOR")
pdf(file.path(settings$pfts$pft$outdir,"mcmc.diagnostics.pdf"))
sink(file.path(settings$pfts$pft$outdir,"mcmc.log"))
## Assess MCMC output
burnin = min(2000,0.2*nrow(params))
params.subset = as.data.frame(params[burnin:nrow(params),var.rows])
names(params.subset) <- rownames(prior)[var.rows]
dm <- as.mcmc(params.subset)

plot(dm)
summary(dm)
if(length(var.rows)>1){
  crosscorr(dm)
  pairs(params.subset)
}

a = 1-rejectionRate(dm)
print("Acceptance Rates")
print(a)
sink()
dev.off()
## update jump variance
# a = 1-rejectionRate(as.mcmc(params[nrow(params)-49:0,var.rows]))
# jvar[var.rows] = jvar[var.rows]*(a/0.4)


#---------------- Write final estimate to Posteriors table. ---------------------------------------#

# create a new Posteriors DB entry
now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
db.query(paste0("INSERT INTO posteriors (pft_id, created_at, updated_at) VALUES (", pftid, ", '", now, "', '", now, "')"), dbcon)
posteriorid <- db.query(paste0("SELECT id FROM posteriors WHERE pft_id=", pftid, " AND created_at='", now, "'"), dbcon)[['id']]

## Save raw MCMC
filename = file.path(settings$outdir,"pda.mcmc.Rdata")
save(params,file=filename)
dbfile.insert(filename, 'Posterior', posteriorid, con)

## save named distributions
filename = file.path(settings$pfts$pft$outdir, 'post.distns.Rdata')
post.distns <- approx.posterior(params.subset, prior, outdir=settings$pfts$pft$outdir)
save(post.distns, file = filename)
dbfile.insert(filename, 'Posterior', posteriorid, con)

## coerce parameter output into the same format as trait.mcmc
pname <- rownames(post.distns)
trait.mcmc <- list()
for(i in var.rows){
  beta.o <- array(params[,i],c(nrow(params),1))
  colnames(beta.o) = "beta.o"
  if(pname[i] %in% names(trait.mcmc)){
    trait.mcmc[[pname[i]]] <- mcmc.list(as.mcmc(beta.o))
  } else {
    k = length(trait.mcmc)+1
    trait.mcmc[[k]] <- mcmc.list(as.mcmc(beta.o))
    names(trait.mcmc)[k] <- pname[i]      
  }
}
## save updated parameter distributions as trait.mcmc
## so that they can be read by the ensemble code
filename = file.path(settings$pfts$pft$outdir, 'trait.mcmc.Rdata')
save(trait.mcmc,file=filename)
dbfile.insert(filename, 'Posterior', pft$posteriorid, con)
status.end()
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### PEcAn workflow run complete
status.start("FINISHED")
if (settings$workflow$id != 'NA') {
  query.base(paste("UPDATE workflows SET finished_at=NOW() WHERE id=", settings$workflow$id, "AND finished_at IS NULL"),con)
}
status.end()
db.close(con)
##close any open database connections
for(i in dbListConnections(PostgreSQL())) db.close(i)
print("---------- PEcAn Workflow Complete ----------")
#--------------------------------------------------------------------------------------------------#

