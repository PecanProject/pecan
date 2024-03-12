library(dplyr)
library(xts)
library(PEcAn.all)
library(purrr)
library(furrr)
library(lubridate)
library(nimble)
library(ncdf4)
library(PEcAnAssimSequential)
library(dplyr)
library(sp)
library(raster)
library(zoo)
library(ggplot2)
library(mnormt)
library(sjmisc)
library(stringr)
library(doParallel)
library(doSNOW)
library(Kendall)
library(lgarch)
library(parallel)
if (future::supportsMulticore()) {
  future::plan(future::multicore)
} else {
  future::plan(future::multisession)
}

setwd("/projectnb/dietzelab/dongchen/anchorSites/")
settings <- PEcAn.settings::read.settings("/projectnb/dietzelab/dongchen/anchorSites/SDA/pecan.xml")

in.path <- "/projectnb/dietzelab/hamzed/ERA5/Data/Ensemble"
out.path <- "/projectnb/dietzelab/dongchen/anchorSites//ERA5_2012_2021"

# prepare ERA5
paths <- ERA5_met_process(settings = settings, in.path = in.path, out.path = out.path, write.db = F, write = T)

#prepare observations
settings <- PEcAn.settings::read.settings("/projectnb/dietzelab/dongchen/anchorSites/SDA/pecan.xml")
settings$state.data.assimilation$Obs_Prep$outdir <- "/projectnb/dietzelab/dongchen/anchorSites/Obs"
obs <- PEcAnAssimSequential::SDA_OBS_Assembler(settings)

#prepare LC and PFTs
LC <- PEcAn.data.remote::MODIS_LC_prep(site_info = site_info, time_points = settings$state.data.assimilation$start.date, qc.filter = T)

#tweak LC to actual pfts
LC[which(LC[,2] %in% c("Deciduous Broadleaf Forests", 
                       "Deciduous Needleleaf Forests", 
                       "Mixed Forests"))] <- "temperate.deciduous.HPDA"
LC[which(LC[,2] %in% c("Evergreen Broadleaf Forests", 
                       "Evergreen Needleleaf Forests"))] <- "boreal.coniferous"
LC[which(LC[,2] %in% c("Closed Shrublands", 
                       "Open Shrublands", 
                       "Woody Savannas", 
                       "Savannas", 
                       "Grasslands",
                       "Permanent Wetlands",
                       "Croplands",
                       "Cropland/Natural Vegetation Mosaics",
                       ))] <- "semiarid.grassland_HPDA"
LC[which(LC[,2] %in% c("Urban and Built-up Lands", 
                       "Permanent Snow and Ice", 
                       "Barren",
                       "Water Bodies"))] <- NA
#delete settings and LC table with NAs.
del_ind <- which(is.na(LC[,2]))
LC <- LC[-del_ind,]
settings <- settings[-del_ind]
#write settings and PFT file.
PEcAn.settings::write.settings(settings, outputfile = "pecan.xml")
write.csv(LC, file = settings$run$settings.1$inputs$pft.site[[1]], row.names = F)
# settings <- PEcAn.settings::read.settings("/projectnb/dietzelab/dongchen/anchorSites/SDA/pecan.xml")
# settings <- PEcAn.settings::prepare.settings(settings)

