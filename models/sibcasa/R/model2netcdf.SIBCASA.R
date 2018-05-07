#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Convert SIBCASA output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.SIBCASA
##' @title Code to convert SIBCASA output into netCDF format
##'
##' @param outdir Location of model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##'
##' @author Tony Gardella
model2netcdf.SIBCASA <- function(outdir, sitelat, sitelon, start_date, end_date) {
    
  PEcAn.logger::logger.severe("NOT IMPLEMENTED") 

    ## Loop over years  
        ## read in and rename variables and units
    [1] "lonindex"    "latindex"    "year"        "month"       "DOM"         "HOD"         "DOY"         "seconds"    
    [9] "ventmf"      "ustar"       "gt"          "tcan"        "ev"          "snow_depth"  "snow_can"    "runoff"     
    [17] "gl"          "testvar1"    "testvar2"    "testvar3"    "gs"          "ra"          "rb"          "rc"         
    [25] "rd"          "snow_bulk"   "d_active"    "d_freeze"    "snow_area"   "snow_mass"   "evt"         "hfss"       
    [33] "fws"         "chf"         "shf"         "hfc"         "hfg"         "hfect"       "hfeci"       "hfegs"      
    [41] "hfegi"       "ea"          "ta"          "em"          "hura"        "leaf_frac"   "root_frac"   "wood_frac"  
    [49] "snow_nsl"    "grnd_liq"    "canopy_liq"  "mrtsoil"     "pco2ap"      "pco2s"       "pco2i"       "pco2c"      
    [57] "spdmsib"     "pssib"       "dlsprsib"    "dcuprsib"    "tssib"       "tsib3"       "sh_sib"      "radvbc"     
    [65] "radvdc"      "radnbc"      "radndc"      "sw_dwn"      "dlwbotsib"   "carb_live"   "carb_dead"   "carb_litter"
    [73] "carb_soil"   "carb_awood"  "carb_tot"    "c_flux"      "assimn"      "gpp"         "NEE_1"       "NEE_2"      
    [81] "npp"         "resp_grnd"   "resp_tot"    "resp_auto"   "resp_het"    "rstfac1"     "rstfac2"     "rstfac3"    
    [89] "carb_above"  "pco2m"       "zlt"         "LAI"         "lai_chi_sqr" "lai_err"     "aparc"       "fpar"       
    [97] "carb_al"     "dperm_top"   "OLT"         "resp_perm"   "resp_al"     "carb_perm"   "carb_froz"   "carb_thaw"  
    [105] "resp_meth"   "soil_frz"    
  
  
} # model2netcdf.SIBCASA
