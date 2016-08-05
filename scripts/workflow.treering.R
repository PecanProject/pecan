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
library(mvtnorm)
library(rjags)
library(reshape2)
#--------------------------------------------------------------------------------------------------#
#
#  dir.create("~/demo.sda")
#  clean.settings("~/demo.pda/demo.xml","~/demo.sda/demo.xml")

#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings() 
#--------------------------------------------------------------------------------------------------#

#---------------- Load plot and tree ring data. -------------------------------------------------------#
status.start("LOAD DATA")
## Read tree data
trees <- read.csv("~/Camp2016/ForestPlots/treecores2014.csv")

## Read tree ring data
rings <- Read_Tucson("~/Camp2016/ForestPlots/Tucson/")

## Match observations & format for JAGS
combined <- matchInventoryRings(trees,rings,extractor="Tag",nyears=36,coredOnly=FALSE) #WARNINGS
data <- buildJAGSdata_InventoryRings(combined) #WARNINGS
status.end()

#---------------- Load plot and tree ring data. -------------------------------------------------------#
status.start("TREE RING MODEL")
## Tree Ring model
n.iter = 3000
jags.out = InventoryGrowthFusion(data,n.iter=n.iter) #WARNINGS
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
allom.stats = AllomAve(pft.data,outdir = settings$outdir,ngibbs=n.iter/10) #WARNINGS
save(allom.stats,file=file.path(settings$outdir,"allom.stats.Rdata"))
status.end()

#-------------- Convert tree-level growth & diameter to stand-level NPP & AGB -------------------------------#
status.start("PLOT2AGB")
out = as.matrix(jags.out)
sel = grep('x[',colnames(out),fixed=TRUE)
state = plot2AGB(combined,out[,sel],settings$outdir,list(allom.stats[[2]],allom.stats[[3]]),unit.conv=0.02) #WARNINGS

NPP.conv <- (1/10000)*(1000/1)*(1/(3.154*10^7))*.48 #mg/ha/yr -> kgC/m2/s
AGB.conv <- (1/10000)*(1000/1)*.48 #mg/ha >-kgC/m2

NPP = apply(state$NPP[1,,],2,mean,na.rm=TRUE)*NPP.conv 
AGB = apply(state$AGB[1,,],2,mean,na.rm=TRUE)*AGB.conv

obs.mean <- list()
for(i in 1:length(NPP)) {
  obs.mean[[i]]<-c(NPP[i],AGB[i])
  names(obs.mean[[i]])<-c("NPP",'plantWood')
}

obs.cov <- list()
for(i in 1:length(NPP)){
  obs.cov[[i]]<- cov(cbind(state$AGB[,,i],state$NPP[,,i]))
  colnames(obs.cov[[i]]) <- c("AGB","NPP")
  rownames(obs.cov[[i]]) <- c("AGB","NPP")
}


if(settings$model$type==LINKAGES){
  obs_tsca = data.frame(mean = apply(state$biomass_tsca[1,,],2,mean,na.rm=TRUE),
                        sd = apply(state$biomass_tsca[1,,],2,sd,na.rm=TRUE))
  obs_acsa3 = data.frame(mean = apply(state$biomass_acsa3[1,,],2,mean,na.rm=TRUE),
                         sd = apply(state$biomass_acsa3[1,,],2,sd,na.rm=TRUE))
  obs_beal2 = data.frame(mean = apply(state$biomass_beal2[1,,],2,mean,na.rm=TRUE),
                         sd = apply(state$biomass_beal2[1,,],2,sd,na.rm=TRUE))
  obs_thoc2 = data.frame(mean = apply(state$biomass_thoc2[1,,],2,mean,na.rm=TRUE),
                         sd = apply(state$biomass_thoc2[1,,],2,sd,na.rm=TRUE))
  
  obs = cbind(obs_tsca,obs_acsa3,obs_beal2,obs_thoc2)
  colnames(obs)<-c("mean_tsca","sd_tsca","mean_acsa3","sd_acsa3","mean_beal2",
                   "sd_beal2","mean_thoc2","sd_thoc2")
}


status.end()

#---------------- Build Initial Conditions ----------------------------------------------------------------------#
status.start("IC")
ne = as.numeric(settings$ensemble$size) # do we want this to point somewhere else?
IC = sample.IC.SIPNET(ne,state)
source("/pecan/modules/assim.sequential/R/sample.IC.LINKAGES.R")
IC = sample.IC.LINKAGES(ne,state)
status.end()

#---------------- Load Priors ----------------------------------------------------------------------#
status.start("PRIORS")
prior = sample.parameters(ne,settings,con)
prior = NA

load(file.path(settings$outdir, "samples.Rdata"))
pick.trait.params <- c(names(ensemble.samples[[1]]),names(ensemble.samples[[2]]))

status.end()

#--------------- Assimilation -------------------------------------------------------#
status.start("MCMC")
settings$ensemble$size <- 50
settings$state.data.assimilation$n.ensemble<- 50
settings$state.data.assimilation$processvar<-TRUE
sda.enkf(settings=settings, obs.mean = obs.mean,
         obs.cov = obs.cov, pick.trait.params = pick.trait.prams, #c('G') for linkages
         given.process.variance = NULL)
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
