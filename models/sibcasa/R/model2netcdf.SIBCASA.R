
#-------------------------------------------------------------------------------------------------#
#' Convert SIBCASA output into the NACP Intercomparison format (ALMA using netCDF)
#'
#' @param outdir Location of model output
#' @param sitelat Latitude of the site
#' @param sitelon Longitude of the site
#' @param start_date Start time of the simulation
#' @param end_date End time of the simulation
#' @export
#'
#' @author Tony Gardella
model2netcdf.SIBCASA <- function(outdir, sitelat, sitelon, start_date, end_date) {

  ## Get Files
  file_list <- dir(outdir, "hsib", all.files = TRUE)
  years <- sub(".qp2.nc", "", sub("hsib.", "", file_list))

  ## Loop over years
  for (year in unique(years)) {
    ysel <- which(years == year) ## subselect files for selected year
    if (length(ysel) > 1) {
      PEcAn.logger::logger.warn("PEcAn.SIBCASA::model2netcdf.SIBCASA does not currently support multiple files per year")
    }

    fname <- paste0(outdir, "hsib_", year, ".qp2.nc")
    nc_file <- ncdf4::nc_open(fname)

    # Check lat and lon
    lat_ind <- ncdf4::ncvar_get(nc_file, "latindex")
    lon_ind <- ncdf4::ncvar_get(nc_file, "lonindex")

    lat <- nc_file$dim$latitude$vals[lat_ind]
    lon <- nc_file$dim$longitude$vals[lon_ind]

    ## Read in variables

    gpp <- ncdf4::ncvar_get(nc_file, "gpp") # micromoles/m^2/s
    nee1 <- ncdf4::ncvar_get(nc_file, "NEE_1") # micromoles/m^2/s (resp_tot - GPP)
    # nee2 <- ncdf4::ncvar_get(nc_file,"NEE_2") #micromoles/m^2/s (conductance-based carbon flux)
    npp <- ncdf4::ncvar_get(nc_file, "npp") # micromol/m^2/s

    # Unit Conversions

    mole2kg_C <- 0.01201071000000 # mole of carbon to kilogram of carbon
    micromole2kg_C <- 0.0000120107 # micromoles of carbon to kilogram of carbon

    output <- list()
    output[[1]] <- gpp * micromole2kg_C
    output[[2]] <- nee1 * micromole2kg_C
    output[[3]] <- npp * micromole2kg_C

    ####
    ## SIBCASA Time
    month <- ncdf4::ncvar_get(nc_file, "month")
    day_in_month <- ncdf4::ncvar_get(nc_file, "DOM") # Days in each month
    hour <- ncdf4::ncvar_get(nc_file, "HOD")
    second <- ncdf4::ncvar_get(nc_file, "seconds")

    month_days <- cumsum(day_in_month)
    # time <- day + sec / 86400
    # nt <- length(time)
    # nc.time <- ncin$dim$time$vals             # days since "start_date"


    ## Build Standard netCDF files

    t <- ncdf4::ncdim_def(
      name = "time",
      units = paste0("days since ", year, "-01-01 00:00:00"),
      month_days,
      calendar = "standard",
      unlim = TRUE
    )
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")


    for (i in seq_along(output)) {
      if (length(output[[i]]) == 0) {
        output[[i]] <- rep(-999, length(t$vals))
      }
    }

    dims <- list(lon = lon, lat = lat, time = t)

    nc_var <- list()
    nc_var[[1]] <- ncdf4::ncvar_def("GPP",
      units = "kg C m-2 s-1", dim = dims, missval = -999,
      longname = "Gross Primary Production"
    )
    nc_var[[2]] <- ncdf4::ncvar_def("NEE",
      units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999,
      longname = "Net Ecosystem Exchange"
    )
    nc_var[[3]] <- ncdf4::ncvar_def("NPP",
      units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999,
      longname = "Net Primary Production"
    )



    ## Write out File
    ### Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(year, "nc", sep = ".")), nc_var)
    varfile <- file(file.path(outdir, paste(year, "nc", "var", sep = ".")), "w")
    for (i in seq_along(nc_var)) {
      # print(i)
      ncdf4::ncvar_put(nc, nc_var[[i]], output[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile, sep = "\n")
    }
    close(varfile)
    ncdf4::nc_close(nc)
  }
} # model2netcdf.SIBCASA

# All Variables in qp2 Output Files
#  [1] "lonindex"    "latindex"    "year"        "month"       "DOM"         "HOD"         "DOY"         "seconds"
#  [9] "ventmf"      "ustar"       "gt"          "tcan"        "ev"          "snow_depth"  "snow_can"    "runoff"
#  [17] "gl"          "testvar1"    "testvar2"    "testvar3"    "gs"          "ra"          "rb"          "rc"
#  [25] "rd"          "snow_bulk"   "d_active"    "d_freeze"    "snow_area"   "snow_mass"   "evt"         "hfss"
#  [33] "fws"         "chf"         "shf"         "hfc"         "hfg"         "hfect"       "hfeci"       "hfegs"
#  [41] "hfegi"       "ea"          "ta"          "em"          "hura"        "leaf_frac"   "root_frac"   "wood_frac"
#  [49] "snow_nsl"    "grnd_liq"    "canopy_liq"  "mrtsoil"     "pco2ap"      "pco2s"       "pco2i"       "pco2c"
#  [57] "spdmsib"     "pssib"       "dlsprsib"    "dcuprsib"    "tssib"       "tsib3"       "sh_sib"      "radvbc"
#  [65] "radvdc"      "radnbc"      "radndc"      "sw_dwn"      "dlwbotsib"   "carb_live"   "carb_dead"   "carb_litter"
#  [73] "carb_soil"   "carb_awood"  "carb_tot"    "c_flux"      "assimn"      "gpp"         "NEE_1"       "NEE_2"
#  [81] "npp"         "resp_grnd"   "resp_tot"    "resp_auto"   "resp_het"    "rstfac1"     "rstfac2"     "rstfac3"
#  [89] "carb_above"  "pco2m"       "zlt"         "LAI"         "lai_chi_sqr" "lai_err"     "aparc"       "fpar"
#  [97] "carb_al"     "dperm_top"   "OLT"         "resp_perm"   "resp_al"     "carb_perm"   "carb_froz"   "carb_thaw"
#  [105] "resp_meth"   "soil_frz"
