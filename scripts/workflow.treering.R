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
library(PEcAn.assim.sequential)
library(PEcAn.visualization)
#--------------------------------------------------------------------------------------------------#
#
#  dir.create("~/demo.sda")
#  clean.settings("~/demo.pda/demo.xml","~/demo.sda/demo.xml")

#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings("~/demo.sda/demo.xml")
#--------------------------------------------------------------------------------------------------#

#---------------- Load plot and tree ring data. -------------------------------------------------------#
status.start("LOAD DATA")
## Read tree data
trees <- read.csv("/home/carya/Camp2014/ForestPlots/treecores2014.csv")

## Read tree ring data
rings <- Read_Tuscon("/home/carya/Camp2014/ForestPlots/Tucson/")

## Match observations & format for JAGS
combined <- matchInventoryRings(trees,rings,extractor="Tag",nyears=36,coredOnly=FALSE)
data <- buildJAGSdata_InventoryRings(combined)
status.end()

#---------------- Load plot and tree ring data. -------------------------------------------------------#
status.start("TREE RING MODEL")
## Tree Ring model
n.iter = 3000
jags.out = InventoryGrowthFusion(data,n.iter=n.iter)
save(trees,rings,combined,data,jags.out,
     file=file.path(settings$outdir,"treering.Rdata"))

pdf(file.path(settings$outdir,"treering.Diagnostics.pdf"))
InventoryGrowthFusionDiagnostics(jags.out,combined)
dev.off()
status.end()

#-------------- Allometry Model -------------------------------#
status.start("ALLOMETRY")
library(PEcAn.allometry)
con <- db.open(settings$database$bety)
pft.data = list()
for(ipft in 1:length(settings$pfts)){  ## loop over PFTs
  pft_name = settings$pfts[[ipft]]$name
  query <- paste0("SELECT s.spcd,",'s."Symbol"'," as acronym from pfts as p join pfts_species on p.id = pfts_species.pft_id join species as s on pfts_species.specie_id = s.id where p.name like '%",pft_name,"%'")  
  pft.data[[pft_name]] <- db.query(query, con)
}
allom.stats = AllomAve(pft.data,outdir = settings$outdir,ngibbs=n.iter/10)
save(allom.stats,file=file.path(settings$outdir,"allom.stats.Rdata"))
status.end()

#-------------- Convert tree-level growth & diamter to stand-level NPP & AGB -------------------------------#
status.start("PLOT2AGB")
out = as.matrix(jags.out)
sel = grep('x[',colnames(out),fixed=TRUE)
state = plot2AGB(combined,out[,sel],settings$outdir,allom.stats,unit.conv=0.01)
obs = data.frame(mean = apply(state$NPP[1,,],2,mean,na.rm=TRUE),
                 sd = apply(state$NPP[1,,],2,sd,na.rm=TRUE))
status.end()

#---------------- Build Initial Conditions ----------------------------------------------------------------------#
status.start("IC")
ne = as.numeric(settings$assim.sequential$n.ensemble)
IC = sample.IC.SIPNET(ne,state)
status.end()

#---------------- Load Priors ----------------------------------------------------------------------#
status.start("PRIORS")
prior = sample.parameters(ne,settings,con)
status.end()

#--------------- Assimilation -------------------------------------------------------#
status.start("MCMC")
sda.enkf(settings,IC,prior,obs)
status.end()

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
