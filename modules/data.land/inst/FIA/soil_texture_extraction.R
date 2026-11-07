library(purrr)
library(foreach)
library(PEcAn.all)
source("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/soilgrids_texture_extract.R")
outdir <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soil_texture_output"
dir.create(outdir)
setwd(outdir)
settings <- PEcAn.settings::read.settings("/projectnb/dietzelab/cwebb16/SDA/AZ_test.xml")
data_paths <- list(
  list(
    url = NULL,
    local = "/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soil_texture/sand/",
    save_path = paste0(outdir, "sand_percent.rds")),
  list(
    url = NULL,
    local = "/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soil_texture/clay/",
    save_path = paste0(outdir, "clay_percent.rds")),
  list(
    url = NULL,
    local = "/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soil_texture/silt/",
    save_path = paste0(outdir, "silt_percent.rds")))

site_info <- settings %>%
  purrr::map(~.x[['run']] ) %>%
  purrr::map('site') %>%
  purrr::map(function(site.list){
    #conversion from string to number
    site.list$lat <- as.numeric(site.list$lat)
    site.list$lon <- as.numeric(site.list$lon)
    list(site_id=site.list$id, lat=site.list$lat, lon=site.list$lon, site_name=site.list$name)
  })%>%
  dplyr::bind_rows() %>%
  as.list()

out <- soilgrids_texture_extraction(site_info, outdir, "/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soil_texture")
dir.create("/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soil_texture_output/soil_texture_ensemble")

sand <- readRDS("/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soil_texture_output/sand_percent.rds") #sand fraction in percentage
clay <- readRDS("/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soil_texture_output/clay_percent.rds") #clay fraction in percentage
silt <- readRDS("/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soil_texture_output/silt_percent.rds") #silt fraction in percentage
dat <- PEcAn.data.land:::soil_params_ensemble_soilgrids(settings,
                                                        sand,
                                                        clay,
                                                        silt,
                                                        "/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soil_texture_output/soil_texture_ensemble")
