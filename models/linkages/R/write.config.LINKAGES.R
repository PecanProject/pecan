#' Writes a LINKAGES config file.
#'
#' Requires a pft xml object, a list of trait values for a single model run,
#' and the name of the file to create
#'
#' @param defaults list of defaults to process
#' @param trait.values vector of samples for a given trait
#' @param settings list of settings from pecan settings file
#' @param run.id id of run
#' @param restart logical: Write a restart file?
#'  If NULL (default), treated as FALSE
#' @param spinup logical: perform spinup using `spinup.LINKAGES()`?
#'  If NULL (default), treated as FALSE
#' @param inputs inputs section of a PEcAn settings object.
#'  Currently only used for climate file (inputs$met$path),
#'  which is taken from `settings$input$met$path` if `inputs` is NULL.
#' @param IC TODO currently ignored
#'
#' @return configuration file for LINKAGES for given run
#' @export
#' @author Ann Raiho, Betsy Cowdery
#'
write.config.LINKAGES <- function(defaults = NULL, trait.values, settings, run.id, 
                                  restart = NULL, spinup = FALSE, inputs = NULL, IC = NULL) {

  # 850-869 repeated to fill 1000 years
  if (is.null(restart)) {
    restart <- FALSE # why not have restart default to FALSE above?
  }
  if (is.null(spinup)) {
    spinup <- FALSE # why not have spinup default to FALSE above?
  }
  
  ##TO DO add restart file as IC for HF

  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  if (!file.exists(rundir)) {  # why not use `dir.exists`?
    dir.create(rundir)
  }
  outdir <- file.path(settings$host$outdir, run.id)
  if (!file.exists(outdir)) {  # why not use `dir.exists`?
    dir.create(outdir)
  }
  
  #-----------------------------------------------------------------------
  #TO DO: need to change to date because sometimes this runs two years when it shouldn't
  start.year <- as.numeric(strftime(settings$run$start.date, "%Y"))
  end.year <- as.numeric(strftime(settings$run$end.date, "%Y"))
  year <- seq(start.year, end.year, 1)
  
  iplot <- 1
  nyear <- length(year)
  max.ind <- 1500
  plat <- abs(as.numeric(settings$run$site$lat))
  
  bgs <- 120
  egs <- 273
  
  texture <- utils::read.csv(system.file("texture.csv", package = "PEcAn.LINKAGES"))
  
  dbcon <- PEcAn.DB::db.open(settings$database$bety)
  on.exit(PEcAn.DB::db.close(dbcon), add = TRUE)
  
  if("soil" %in% names(settings$run$inputs)){
    ## open soil file
    soil <- settings$run$inputs$soil
    nc.soil <- ncdf4::nc_open(soil$path)
    
    ## extract LINKAGES variables
    fc      <- ncdf4::ncvar_get(nc.soil,"volume_fraction_of_water_in_soil_at_field_capacity") * 100
    dry     <- ncdf4::ncvar_get(nc.soil,"volume_fraction_of_condensed_water_in_soil_at_wilting_point") * 100
    if(length(fc) > 1) fc <- mean(fc)
    if(length(dry) > 1) dry <- mean(dry)
    ncdf4::nc_close(nc.soil)

  }else{
    soils <- PEcAn.DB::db.query(paste("SELECT soil,som,sand_pct,clay_pct,soilnotes FROM sites WHERE id =", settings$run$site$id),
                      con = dbcon)

    soil.dat <- PEcAn.data.land::soil_params(sand = soils$sand_pct/100, clay = soils$clay_pct/100, silt = 100 - soils$sand_pct - soils$clay_pct)
    
    fc <- soil.dat$volume_fraction_of_water_in_soil_at_field_capacity * 100
    dry <- soil.dat$volume_fraction_of_condensed_water_in_soil_at_wilting_point * 100
    
    if(is.na(fc)) fc = 5
    if(is.na(dry)) dry = 5
  }

  fdat <- utils::read.csv(system.file("fdat.csv", package = "linkages"), header = FALSE)  #litter quality parameters
  clat <- utils::read.csv(system.file("clat.csv", package = "linkages"), header = FALSE)
  load(system.file("switch.mat.Rdata", package = "linkages"))

  if(!is.null(inputs)){
    climate_file <- inputs$met$path
    load(climate_file)
  }else{
    climate_file <- settings$run$inputs$met$path
    load(climate_file) 
  }
  
  temp.mat <- matrix(temp.mat[which(rownames(temp.mat)%in%start.year:end.year),],ncol=12,byrow=F)
  precip.mat <- matrix(precip.mat[which(rownames(precip.mat)%in%start.year:end.year),],ncol=12,byrow=F)
  
  basesc <- 74
  basesn <- 1.64

  spp.params.default <- utils::read.csv(system.file("spp_matrix.csv", package = "linkages"))  # default spp.params
  nspec <- length(settings$pfts)
  spp.params.save <- numeric(nspec)
  for (i in seq_len(nspec)) {
    spp.params.save[i] <- which(spp.params.default[, 1] %in% settings$pfts[i]$pft$name)
  }
  spp.params <- spp.params.default[spp.params.save, ]
  
  ### Create species parameter matrix with correct PFTs trait.values$`Hemlock(Tsuga Canadensis)`$
  ### group will be each spp.
  if (!is.null(trait.values)) {
    for (group in names(trait.values)) {
      if (group == "env" | any(settings$run$inputs$met$source == 'PalEONregional')) {
        
        ## leave defaults
        
      } else {
        ## copy values
          # IF: not sure what's going on here but I had to have this hack to overwrite params below
          # should come back to this
          if(is.null(dim(trait.values[[group]]))){
            vals <- as.data.frame(t(trait.values[[group]]))
          }else{
            vals <- as.data.frame(trait.values[[group]])
          }
          
          if ("SLA" %in% names(vals)) {
            sla_use <- (1/vals$SLA)*1000
            sla_use[sla_use>5000] <- stats::rnorm(1,4000,100)
            spp.params[spp.params$Spp_Name == group, ]$FWT <- sla_use
            ## If change here need to change in write_restart as well
            }
          
          # replace defaults with traits
          #new.params.locs <- which(names(spp.params) %in% names(vals))
          #new.vals.locs <- which(names(vals) %in% names(spp.params))
          #spp.params[which(spp.params$Spp_Name == group), new.params.locs] <- vals[new.vals.locs]
          
          # conversion of some traits to match what LINKAGES needs Going to have to look up this paper
          # Botkin 1972 Some Ecological Consequences of a computer model of forest growth
          if ("HTMAX" %in% names(vals) & "DBHMAX" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$B2 <- 2 * (((vals$HTMAX * 100) - 137) / 
                                                                    (vals$DBHMAX * 100))
            spp.params[spp.params$Spp_Name == group, ]$B3 <- (vals$HTMAX * 100 - 137) / (vals$DBHMAX * 100^2)
          }
          
          if ("root2shoot" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$RTST <- vals$root2shoot
          }
          
          # if ("leaf_longevity" %in% names(vals)) {
          #   spp.params[spp.params$Spp_Name == group, ]$FRT <- vals$leaf_longevity
          # }
          
          if ("DMAX" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$DMAX <- vals$DMAX
          }
          if ("DMIN" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$DMIN <- vals$DMIN
          }
          if ("AGEMX" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$AGEMX <- vals$AGEMX
          }

          if ("Gmax" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$G <- vals$Gmax
          }
          if ("SPRTND" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$SPRTND <- vals$SPRTND
          }
          if ("SPRTMN" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$SPRTMN <- vals$SPRTMN
          }
          if ("SPRTMX" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$SPRTMX <- vals$SPRTMX
          }
          if ("MPLANT" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$MPLANT <- vals$MPLANT
          }
          if ("D3" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$D3 <- vals$D3
          }
          if ("FROST" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$FROST <- vals$FROST
          }
          if ("CM1" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$CM1 <- vals$CM1
          }
          if ("CM2" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$CM2 <- vals$CM2
          }
          if ("CM3" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$CM3 <- vals$CM3
          }
          if ("CM4" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$CM4 <- vals$CM4
          }
          if ("CM5" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$CM5 <- vals$CM5
          }
          
          if ("SLTA" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$SLTA <- vals$SLTA
          }
          if ("SLTB" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$SLTB <- vals$SLTB
          }
          if ("FRT" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$FRT <- vals$FRT
          }
          if ("TL" %in% names(vals)) {
            spp.params[spp.params$Spp_Name == group, ]$TL <- ceiling(vals$TL)
          }

        }
      }
    }
  
  switch.mat <- switch.mat[spp.params.save, ]
  
  if (spinup) {
    spinup.out <- spinup.LINKAGES(start.year, end.year, temp.mat, precip.mat)
    start.year <- spinup.out$start.year
    end.year <- spinup.out$end.year
    nyear <- spinup.out$nyear
    temp.mat <- spinup.out$temp.mat
    precip.mat <- spinup.out$precip.mat
    settings$run$start.date <- paste0(spinup.out$start.year, 
                                      strftime(settings$run$start.date, "/%m/%d"))
  }
  
  input <- file.path(settings$rundir, run.id, "linkages.input.Rdata")
  
  save(iplot, nyear, nspec, fc, dry, bgs, egs, max.ind, plat, temp.mat, 
       precip.mat, spp.params, switch.mat, fdat, clat, basesc, basesn, 
       start.year, end.year, file = input)
  
  if (restart) {
    restartfile <- file.path(settings$rundir, run.id, "linkages.restart.Rdata")
  } else {
    restartfile <- NULL
  }
  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.LINKAGES"), n = -1)
  }
  
  # create host specific setttings
  hostsetup <- ""
  if (!is.null(settings$model$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$model$prerun, collapse = "\n"))
  }
  if (!is.null(settings$host$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$host$prerun, collapse = "\n"))
  }
  
  hostteardown <- ""
  if (!is.null(settings$model$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$model$postrun, collapse = "\n"))
  }
  if (!is.null(settings$host$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$host$postrun, collapse = "\n"))
  }
  
  # create job.sh
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)
  
  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  jobsh <- gsub("@SITE_MET@", settings$run$inputs$met$path, jobsh)
  
  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
  
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  
  jobsh <- gsub("@INPUT@", input, jobsh)
  jobsh <- gsub("@RESTART@", restart, jobsh)
  if (restart) {
    jobsh <- gsub("@RESTARTFILE@", restartfile, jobsh)
  }
  
  pft_names <- unlist(sapply(settings$pfts, `[[`, "name"))
  pft_names <- paste0("pft_names = c('", paste(pft_names, collapse = "','"), "')")
  jobsh <- gsub("@PFT_NAMES@", pft_names, jobsh)
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
} # write.config.LINKAGES
