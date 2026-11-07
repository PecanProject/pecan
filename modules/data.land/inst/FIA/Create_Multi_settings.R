############################################################################
############################################################################
###                                                                      ###
###                      Setup Environmental Paths                       ###
###                                                                      ###
############################################################################
############################################################################
start_date <- "2012/01/01"
end_date <- "2024/12/31"

#setup working space
outdir <- "/projectnb/dietzelab/cwebb16/SDA"
SDA_run_dir <- "/projectnb/dietzelab/cwebb16/SDA/run"
SDA_out_dir <- "/projectnb/dietzelab/cwebb16/SDA/out"

ERA5_dir <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/ERA5_2012_2024"
xml_out_dir <- "/projectnb/dietzelab/cwebb16/SDA"
XML_out_dir <- file.path(xml_out_dir, "pecan.xml")

pft_csv_dir <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/site_pft.csv"
modis_phenology_dir <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/Phenology/leaf_phenology.csv"
model_binary <- "/projectnb/dietzelab/cwebb16/sipnet/sipnet"
n_ensemble <- 100
n_era5_ens <- 10
site_info_path <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/site_info.RData"
soil_base <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soil_texture_output/soil_texture_ensemble"
ic_base <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/IC"

#Obs_prep part
#AGB
AGB_indir     <- "/projectnb/dietzelab/dongchen/Multi-site/download_500_sites/AGB"
rf_model_path <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/Obs/landtrendr_rf.rds"
lt_cov_dir    <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/covariates"
lc_dir        <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/glance"
allow_download <- TRUE
AGB_export_csv <- TRUE
AGB_timestep   <- list(unit="year", num=1)

#LAI
LAI_search_window <- 30
LAI_timestep <- list(unit="year", num=1)
LAI_export_csv <- TRUE
run_parallel <- TRUE
sd_threshold <- 20

#SMP
SMP_search_window <- 30
SMP_timestep <- list(unit="year", num=1)
SMP_export_csv <- TRUE
update_csv <- FALSE

#SoilC
SoilC_timestep <- list(unit="year", num=1)
SoilC_export_csv <- TRUE

#Obs Date
obs_start_date <- "2012-07-15"
obs_end_date <- "2024-07-15"
obs_outdir <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/Obs"
timestep <- list(unit="year", num=1)

#specify host section
host.flag <- "local"
if (host.flag == "remote") {
  #if we submit jobs through tunnel remotely.
  host = structure(list(
    name = "geo.bu.edu",
    usr = "zhangdc",
    folder = SDA_out_dir,
    prerun = "module load R/4.4.0",
    cdosetup = "module load cdo/2.0.6",
    qsub = "qsub -l h_rt=24:00:00 -q &apos;geo*&apos; -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash",
    qsub.jobid = "Your job ([0-9]+) .*",
    qstat = "qstat -j @JOBID@",
    tunnel = "~/Tunnel/Tunnel",
    outdir = SDA_out_dir,
    rundir = SDA_run_dir
  ))
} else if (host.flag == "local") {
  host = structure(list(
    name = "localhost",
    prerun = "module load R/4.4.0",
    cdosetup = "module load cdo/2.0.6",
    folder = SDA_out_dir,
    outdir = SDA_out_dir,
    rundir = SDA_run_dir
  ))
}
#Start building template
template <- PEcAn.settings::Settings(list(
  ############################################################################
  ############################################################################
  ###                                                                      ###
  ###                     STATE DATA ASSIMILATION PART                     ###
  ###                                                                      ###
  ############################################################################
  ############################################################################
  state.data.assimilation = structure(list(
    process.variance = TRUE,
    aqq.Init = 1,
    bqq.Init = 1,
    adjustment = FALSE,
    censored.data = FALSE,
    free.run = FALSE,
    FullYearNC = TRUE,
    NC.Overwrite = FALSE,
    NC.Prefix = "sipnet.out",
    q.type = "vector",
    by.site = FALSE,
    Localization.FUN = "Local.support",
    scalef = 0,
    chains = 1,
    data = structure(list(format_id = 1000000040, input.id = 1000013298)),
    state.variables = structure(list(
      #you could add more state variables here
      variable = structure(list(variable.name = "AbvGrndWood", unit = "MgC/ha", min_value = 0, max_value = 9999)),
      variable = structure(list(variable.name = "LAI", unit = "", min_value = 0, max_value = 9999)),
      variable = structure(list(variable.name = "SoilMoistFrac", unit = "", min_value = 0, max_value = 1)),
      variable = structure(list(variable.name = "TotSoilCarb", unit = "kg/m^2", min_value = 0, max_value = 9999))
    )),
    forecast.time.step = "year",
    start.date = start_date,
    end.date = end_date,

    Obs_Prep = structure(list(
      variables = structure(list(
        LandTrendr = structure(list(var_name = "AbvGrndWood", AGB_indir = AGB_indir, rf_model_path = rf_model_path, lt_cov_dir = lt_cov_dir, lc_dir = lc_dir, timestep = AGB_timestep, allow_download = allow_download, export_csv = AGB_export_csv)),
        MODIS = structure(list(var_name = "LAI", search_window = LAI_search_window, timestep = LAI_timestep, export_csv = LAI_export_csv, run_parallel = run_parallel, sd_threshold = sd_threshold)),
        SMAP = structure(list(var_name = "SoilMoistFrac", search_window = SMP_search_window, timestep = SMP_timestep, export_csv = SMP_export_csv, update_csv = update_csv)),
        SoilGrids = structure(list(var_name = "TotSoilCarb", timestep = SoilC_timestep, export_csv = SoilC_export_csv))
      )),
      start.date = obs_start_date,
      end.date = obs_end_date,
      outdir = obs_outdir,
      timestep = timestep
    )),

    batch.settings = structure(list(
      general.job = structure(list(cores = 32, folder.num = 80)),
      qsub.cmd = "qsub -l h_rt=24:00:00 -l mem_per_core=4G -l buyin -pe omp 16 -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
    ))
  )),

  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                              INFO PART                              ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  info = structure(list(
    notes = NULL, userid = "-1", username = "",
    date = "2017/12/06 21:19:33 +0000"
  )),

  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                               DIR PART                              ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  outdir = outdir,
  rundir = SDA_run_dir,
  modeloutdir = SDA_out_dir,

  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                              BETY PART                              ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  database = structure(list(
    bety = structure(
      list(user = "bety", password = "bety", host = "10.241.76.27",
           dbname = "bety", driver = "PostgreSQL", write = "FALSE"
      ))
  )),

  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                              PFT PARTS                              ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  pfts = structure(list(
    #you could add more pfts you needed and make sure to modify the outdir of each pft!!!
    pft = structure(
      list(name = "temperate.deciduous.HPDA",
           constants = structure(list(num = "1")),
           posteriorid = "1000022311",
           outdir = "/fs/data2/output//PEcAn_1000010530/pft/temperate.deciduous.HPDA"
      )),

    pft = structure(
      list(name = "boreal.coniferous",
           outdir = "/projectnb/dietzelab/hamzed/SDA/ProductionRun/50Sites/SDA_50Sites_1000008768/pft/Conifer/boreal.coniferous"
      )),

    pft = structure(
      list(name = "semiarid.grassland_HPDA",
           constants = structure(list(num = "1")),
           posteriorid = "1000016525",
           outdir = "/projectnb/dietzelab/hamzed/SDA/ProductionRun/50Sites/SDA_50Sites_1000008768/pft/GrassA/semiarid.grassland"
      ))
  )),

  ############################################################################
  ############################################################################
  ###                                                                      ###
  ###                          META ANALYSIS PART                          ###
  ###                                                                      ###
  ############################################################################
  ############################################################################
  meta.analysis = structure(list(
    iter = "3000", random.effects = FALSE
  )),

  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                            ENSEMBLE PART                            ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  ensemble = structure(list(size = n_ensemble, variable = "NPP",
                            samplingspace = structure(list(
                              parameters = structure(list(method = "lhc")),
                              soil_physics = structure(list(method = "sampling")),
                              met = structure(list(method = "sampling")),
                              poolinitcond = structure(list(method = "sampling"))
                            ))
  )),

  ############################################################################
  ############################################################################
  ###                                                                      ###
  ###                              MODEL PART                              ###
  ###                                                                      ###
  ############################################################################
  ############################################################################
  model = structure(list(id = "1000000022",
                         type = "SIPNET",
                         revision = "ssr",
                         delete.raw = FALSE,
                         binary = model_binary,
                         jobtemplate = "~/sipnet_geo.job"
  )),

  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                              HOST PART                              ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  #be carefull of the host section, you need to specify the host of your own!!!
  host = host,

  ############################################################################
  ############################################################################
  ###                                                                      ###
  ###                          SETTINGS INFO PART                          ###
  ###                                                                      ###
  ############################################################################
  ############################################################################
  settings.info = structure(list(
    deprecated.settings.fixed = TRUE,
    settings.updated = TRUE,
    checked = TRUE
  )),

  ############################################################################
  ############################################################################
  ###                                                                      ###
  ###                               RUN PART                               ###
  ###                                                                      ###
  ############################################################################
  ############################################################################
  run = structure(list(
    inputs = structure(list(
      met = structure(list(
        source = "ERA5",
        output = "SIPNET",
        id = "",
        path = ERA5_dir
      )),
      pft.site = structure(list(path = pft_csv_dir)),
      leaf_phenology = structure(list(path = modis_phenology_dir))
    ))
  ))
))

############################################################################
############################################################################
###                                                                      ###
###                Using Site Info to create the Settings                ###
###                                                                      ###
############################################################################
############################################################################

load(site_info_path)  # loads site_info with site_id, lat, lon
site_info <- as.data.frame(site_info)
site_ids  <- sort(as.integer(site_info$site_id))

run_list <- vector("list", length(site_ids))
names(run_list) <- paste0("site.", site_ids)

for (i in seq_along(site_ids)) {
  sid <- site_ids[i]
  lat <- site_info$lat[site_info$site_id == sid]
  lon <- site_info$lon[site_info$site_id == sid]

  era5_paths <- setNames(lapply(seq_len(n_era5_ens), function(e)
    file.path(ERA5_dir, paste0("ERA5_", sid, "_", e),
              paste0("ERA5.", e, ".", gsub("/", "-", start_date), ".", gsub("/", "-", end_date), ".clim"))),
    rep("path", n_era5_ens))
  soil_paths <- setNames(lapply(n_ensemble:1, function(e)
    file.path(soil_base, sid, paste0("Soil_params_", sid, "_", e, ".nc"))),
    rep("path", n_ensemble))
  ic_paths   <- setNames(lapply(n_ensemble:1, function(e)
    file.path(ic_base, sid, paste0("IC_site_", sid, "_", e, ".nc"))),
    rep("path", n_ensemble))

  run_list[[i]] <- list(
    site = list(
      id        = as.character(sid),
      lat       = lat,
      lon       = lon,
      name      = "weighted_sample",
      met.start = start_date,
      met.end   = end_date
    ),
    start.date = start_date,
    end.date   = end_date,
    inputs = list(
      met = list(
        source = "ERA5", output = "SIPNET", id = "",
        path   = era5_paths
      ),
      leaf_phenology = list(path = modis_phenology_dir),
      pft.site       = list(path = pft_csv_dir),
      soil_physics   = list(
        source   = "SoilGrids", output = "soil_physics",
        ensemble = n_ensemble,  path   = soil_paths
      ),
      poolinitcond   = list(
        source   = "RS_veg", output = "poolinitcond",
        ensemble = n_ensemble, path  = ic_paths
      )
    )
  )
}

template$run <- run_list
settings <- PEcAn.settings::MultiSettings(template)

dir.create(xml_out_dir, recursive = TRUE, showWarnings = FALSE)
if (file.exists(XML_out_dir)) unlink(XML_out_dir)
PEcAn.settings::write.settings(settings, outputdir = xml_out_dir, outputfile = "pecan.xml")

#here we re-read the xml file to fix issues of some special character within the Host section.
tmp = readChar(XML_out_dir, file.info(XML_out_dir)$size)
tmp = gsub("&amp;","&",tmp)
writeChar(tmp, XML_out_dir, eos = NULL)

settings <- PEcAn.settings::read.settings(XML_out_dir)
