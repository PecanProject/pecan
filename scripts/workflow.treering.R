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
      #  dir.create("~/demo.sda")
      #  clean.settings("~/demo.pda/demo.xml","~/demo.sda/demo.xml")
      
      #---------------- Load PEcAn settings file. -------------------------------------------------------#
      # Open and read in settings file for PEcAn run.
      settings <- read.settings("~/demo.sda/demo.xml")
      #--------------------------------------------------------------------------------------------------#
      
      ## Read tree data
      trees <- read.csv("/home/carya/Camp2014/PecanInputs/H 2012 Adult Field Data.csv")
      
      ## Read tree ring data
      rings <- Read_Tuscon("/home/carya/Camp2014/PecanInputs/Revised 2/")
      
      ## Match observations & format for JAGS
      combined <- matchInventoryRings(trees,rings,nyears=30,coredOnly=FALSE)
      data <- buildJAGSdata_InventoryRings(combined)
      
      ## Tree Ring model
      n.iter = 300
      jags.out = InventoryGrowthFusion(data,n.iter=n.iter)
      save(trees,rings,combined,data,jags.out,
           file=file.path(settings$outdir,"treering.Rdata"))

pdf(file.path(settings$outdir,"treering.Diagnostics.pdf"))
InventoryGrowthFusionDiagnostics(jags.out,combined)
dev.off()

## Allometry Model
library(PEcAn.allometry)
con <- db.open(settings$database$bety)
pft.data = list()
for(ipft in 1:length(settings$pfts)){  ## loop over PFTs
  pft_name = settings$pfts[[ipft]]$name
  query <- paste0("SELECT s.spcd,",'s."Symbol"'," as acronym from pfts as p join pfts_species on p.id = pfts_species.pft_id join species as s on pfts_species.specie_id = s.id where p.name like '%",pft_name,"%'")  
  pft.data[[pft_name]] <- db.query(query, con)
}
allom.stats = AllomAve(pft.data,outdir = settings$outdir,ngibbs=n.iter)
save(allom.stats,file=file.path(settings$outdir,"allom.stats.Rdata"))

## Convert tree-level growth & diamter to stand-level NPP & AGB
out = as.matrix(jags.out)
sel = grep('x[',colnames(out),fixed=TRUE)
state = plot2AGB(combined,out[,sel],settings$outdir,allom.stats,unit.conv=0.02)
obs = data.frame(mean = apply(state$NPP[1,,],2,mean,na.rm=TRUE),
                 sd = apply(state$NPP[1,,],2,sd,na.rm=TRUE))

## Build Initial Conditions
#### write INITAL CONDITIONS here ####
# settings$assim.sequential$n.ensemble = 10
ne = settings$assim.sequential$n.ensemble
plantWood = ifelse(rep("AGB" %in% names(state),ne),
                   state$AGB[1,sample.int(ncol(state$AGB),ne),1]*0.1,
                   runif(ne,0,300)) ## prior
lai = ifelse(rep("LAI" %in% names(state),ne),
             state$LAI[1,sample.int(ncol(state$LAI),ne),1],
             runif(ne,0,7)) ## prior
litter = ifelse(rep("litter" %in% names(state),ne),
             state$litter[1,sample.int(ncol(state$litter),ne),1],
             runif(ne,0,4)) ## prior
soil   = ifelse(rep("soil" %in% names(state),ne),
                state$soil[1,sample.int(ncol(state$soil),ne),1],
                runif(ne,0,10)) ## prior
litterWFrac = ifelse(rep("litterW" %in% names(state),ne),
                state$litterW[1,sample.int(ncol(state$litterW),ne),1],
                runif(ne)) ## prior
soilWFrac = ifelse(rep("soilW" %in% names(state),ne),
                state$soilW[1,sample.int(ncol(state$soilW),ne),1],
                runif(ne)) ## prior
snow = ifelse(rep("snow" %in% names(state),ne),
                state$snow[1,sample.int(ncol(state$snow),ne),1],
                runif(ne,0,1)) ## prior
microbe = ifelse(rep("microbe" %in% names(state),ne),
                state$microbe[1,sample.int(ncol(state$microbe),ne),1],
                runif(ne,0,1)) ## prior                  
IC = data.frame(plantWood,lai,litter,soil,litterWFrac,soilWFrac,snow,microbe)

#---------------- Load Priors ----------------------------------------------------------------------#
status.start("PRIORS")
## grab posteriors from database
if(is.null(settings$assim.sequential$prior)){
  pft.id =  db.query(paste0("SELECT id from pfts where name = '",settings$pfts$pft$name,"'"),con)
  priors =  db.query(paste0("SELECT * from posteriors where pft_id = ",pft.id),con)
  ## by default, use the most recent posterior as the prior
  settings$assim.sequential$prior = priors$id[which.max(priors$updated_at)]
}
## load prior
prior.db = db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id = ",
                           settings$assim.sequential$prior),con)
prior.db = prior.db[grep("post.distns.Rdata",prior.db$file_name),]
load(file.path(prior.db$file_path,"post.distns.Rdata"))
## sample from priors
nvar <- nrow(post.distns)
prior = as.data.frame(matrix(numeric(),ne,nvar))
for(i in 1:nvar){
  if(post.distns$distn[i] == 'exp'){
    prior[,i] <- eval(parse(text=paste0("rexp(",ne,",",post.distns$parama[i],")")))
  }else{
    prior[,i] <- eval(parse(text=paste0("r",post.distns$distn[i],"(",ne,",",post.distns$parama[i],",",post.distns$paramb[i],")")))
  }
}
colnames(prior) = rownames(post.distns)
status.end()

## run ensemble
# Query the trait database for data and priors
#settings$pfts <- get.trait.data(settings$pfts, settings$run$dbfiles, settings$database$bety, settings$meta.analysis$update)

# Run the PEcAn meta.analysis
#run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$run$dbfiles, settings$database$bety)

# Calls model specific write.configs e.g. write.config.ed.R
run.write.configs(settings, settings$database$bety$write)

# Start ecosystem model runs
start.model.runs(settings, settings$database$bety$write)

# Get results of model runs
get.results(settings)

sda.particle(settings$model$name)

sda.enkf(settings,IC,prior,obs)

db.close(con)
#--------------------------------------------------------------------------------------------------#
