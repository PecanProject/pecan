#--------------------------------------------------------------------------------#
# functions used to write STATUS used by history
#--------------------------------------------------------------------------------#
options(warn = 1, keep.source = TRUE, error = quote({
  status.end("ERROR")
}))

status.start <- function(name) {
  cat(paste(name, format(Sys.time(), "%F %T"), sep = "\t"), file = file.path(settings$outdir, "STATUS"), append = TRUE)
}

status.end <- function(status = "DONE") {
  cat(paste("", format(Sys.time(), "%F %T"), status, "\n", sep = "\t"), file = file.path(settings$outdir, "STATUS"), append = TRUE)
}

#---------------- Load libraries. -----------------------------------------------------------------#
library(PEcAn.all)
library(PEcAn.assim.sequential)
library(PEcAn.visualization)
library(PEcAn.allometry)
library(mvtnorm)
library(rjags)
library(reshape2)
#--------------------------------------------------------------------------------------------------#
#

#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings("pecan.SDA.xml")
#--------------------------------------------------------------------------------------------------#

#---------------- Load plot and tree ring data. -------------------------------------------------------#
status.start("LOAD DATA")
## Read tree data
trees <- read.csv("~/Camp2016/ForestPlots/2016/TenderfootBog_2016_Cleaned.csv")

## Read tree ring data
rings <- Read_Tucson("~/Camp2016/ForestPlots/2016/TucsonCombined/")

## Match observations & format for JAGS
combined <- matchInventoryRings(trees, rings, extractor = "Tag", nyears = 39, coredOnly = FALSE)  #WARNINGS
data <- buildJAGSdata_InventoryRings(combined)  #WARNINGS
status.end()

#---------------- Load plot and tree ring data. -------------------------------------------------------#
status.start("TREE RING MODEL")
## Tree Ring model
n.iter <- 5000
jags.out <- InventoryGrowthFusion(data, n.iter = n.iter)
save(trees, rings, combined, data, jags.out, file = file.path(settings$outdir, "treering.Rdata"))

pdf(file.path(settings$outdir, "treering.Diagnostics.pdf"))
InventoryGrowthFusionDiagnostics(jags.out, combined)
### Error in prec[, i] : subscript out of bounds
dev.off()
status.end()

#-------------- Allometry Model -------------------------------#
status.start("ALLOMETRY")
con <- db.open(settings$database$bety)
pft.data <- list()
for (ipft in seq_along(settings$pfts)) {
  ## loop over PFTs
  pft_name <- settings$pfts[[ipft]]$name
  query <- paste0("SELECT s.spcd,", "s.\"Symbol\"", " as acronym from pfts as p join pfts_species on p.id = pfts_species.pft_id join species as s on pfts_species.specie_id = s.id where p.name like '%",
                  pft_name, "%'")
  pft.data[[pft_name]] <- db.query(query, con)
}
allom.stats <- AllomAve(pft.data, outdir = settings$outdir, ngibbs = n.iter/10)
save(allom.stats, file = file.path(settings$outdir, "allom.stats.Rdata"))
status.end()

#-------------- Convert tree-level growth & diameter to stand-level NPP & AGB -------------------------------#
status.start("PLOT2AGB")
out <- as.matrix(jags.out)
sel <- grep("x[", colnames(out), fixed = TRUE)
unit.conv <- pi * 10^2/10000
state <- plot2AGB(combined, out[, sel], settings$outdir, list(allom.stats[[2]]), unit.conv = unit.conv)

biomass2carbon <- 0.48

state$NPP <- PEcAn.utils::ud_convert(state$NPP,'Mg/ha/yr','kg/m^2/s') * biomass2carbon# kgC/m^2/s 
state$AGB <- PEcAn.utils::ud_convert(state$AGB,'Mg/ha','kg/m^2') * biomass2carbon# kgC/m2

NPP <- apply(state$NPP[1, , ], 2, mean, na.rm = TRUE)
AGB <- apply(state$AGB[1, , ], 2, mean, na.rm = TRUE)
        
obs.mean <- list()
for (i in seq_along(NPP)) {
  obs.mean[[i]] <- c(NPP[i], AGB[i])
  names(obs.mean[[i]]) <- c("NPP", "AbvGrndWood")
}

obs.cov <- list()
for (i in seq_along(NPP)) {
  obs.cov[[i]] <- cov(cbind(state$NPP[, , i], state$AGB[, , i]))
  colnames(obs.cov[[i]]) <- c("NPP", "AbvGrndWood")
  rownames(obs.cov[[i]]) <- c("NPP", "AbvGrndWood")
}

obs.times <- seq(as.Date(settings$state.data.assimilation$start.date), as.Date(settings$state.data.assimilation$end.date), by = settings$state.data.assimilation$forecast.time.step)
obs.times <- lubridate::year(obs.times)

names(obs.mean) <- paste0(obs.times,'/12/31')
names(obs.cov) <- paste0(obs.times,'/12/31')
status.end()

#---------------- Build Initial Conditions ----------------------------------------------------------------------#
status.start("IC")
ne <- as.numeric(settings$state.data.assimilation$n.ensemble)
IC <- sample.IC.SIPNET(ne, state)
status.end()

#--------------- Assimilation -------------------------------------------------------#
status.start("EnKF")
sda.enkf(settings = settings, obs.mean = obs.mean, obs.cov = obs.cov, IC = IC, Q = NULL)
status.end()
#--------------------------------------------------------------------------------------------------#
### PEcAn workflow run complete
status.start("FINISHED")
if (settings$workflow$id != "NA") {
  db.query(paste("UPDATE workflows SET finished_at=NOW() WHERE id=", settings$workflow$id, "AND finished_at IS NULL"), con)
}
status.end()
db.close(con)
## close any open database connections
for (i in dbListConnections(PostgreSQL())) {
  db.close(i)
}
print("---------- PEcAn Workflow Complete ----------")
#--------------------------------------------------------------------------------------------------#
