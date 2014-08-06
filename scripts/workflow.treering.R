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
InventoryGrowthFusionDiagnostics(combined,jags.out,combined)
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

sda.enkf(settings$model$name)

db.close(con)
#--------------------------------------------------------------------------------------------------#
