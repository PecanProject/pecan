####################################################################################################
#
#   Generate MAAT drivers using NGEE-Tropics QA/QC PA-Bar (BCI) met drivers.  Source format
#   differs from PA-PNM and PA-SLZ, so for now using two separate met prep scripts
#   Goal: Get NGEE to provide met drivers in a standard format like AMF to allow for
#         one met script
#
#
#    --- Last updated: 10.03.2018 By Shawn P. Serbin <sserbin@bnl.gov>
####################################################################################################


#---------------- Close all devices and delete all variables. -------------------------------------#
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files

library(lubridate)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Load met driver data
met_path <- file.path('/Volumes/data/Model_Data/sites/PA-Bar/NGEETropics_source/')
met_drivers <- read.csv(file = file.path(met_path,'BCI_met_drivers_2003_2016.csv'), header=T)
met_output_path <- file.path('/Volumes/data/Model_Data/sites/PA-Bar/MAAT_drivers/')

## Options
site_name <- "PA-Bar"
pressure <- TRUE
wind <- TRUE
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Subset met
names(met_drivers)
head(met_drivers)

MAAT_Time <- lubridate::mdy_hm(as.character(met_drivers$Date_UTC_start), tz="UTC")
date_range <- unique(lubridate::year(MAAT_Time))
head(MAAT_Time)

met_yr_subset <- c(2015,2016)
met_years <- lubridate::year(MAAT_Time)
met_drivers$Time <- MAAT_Time
met_drivers$PAR_umols_m2_s <- met_drivers$SR_W_m2.*2.114
met_drivers$Tair_degC <- met_drivers$Temp_o_C.
met_drivers$RH_perc <- met_drivers$RH_.
met_drivers$VPD_kPa <- PEcAn.data.atmosphere::get.vpd(met_drivers$RH_perc, met_drivers$Tair_degC) / 10
met_drivers$Prec_mm <- met_drivers$RA_mm_d/24  # converting to mm per 1 hour period (met timestep hourly)

# get additional variables
if (pressure){
  met_drivers$Press_Pa <- PEcAn.utils::ud_convert(met_drivers$BP_hPa, "mmHg", "Pa")  # need to match this with source, when availible 
} else {
  met_drivers$Press_Pa <- rep(101325,length(met_drivers$Time))  # just use standard atmospheric pressure at sea level
}

if (wind) {
  met_drivers$Windspeed_m_s <- met_drivers$WS_m_s
}

# subset
if (met_yr_subset[2]-met_yr_subset[1] != 0 ) {
  met_driver_subset <- subset(met_drivers, met_years %in% seq(met_yr_subset[1], met_yr_subset[2],1))
} else {
  met_driver_subset <- subset(met_drivers, met_years == met_yr_subset[1])
}
met_years <- lubridate::year(met_driver_subset$Time)


# finalize
if (wind) {
  output_met_driver <- cbind.data.frame(Time = met_driver_subset$Time,
                                        Year = met_years,
                                        DOY = lubridate::yday(met_driver_subset$Time),
                                        Hour = strftime(met_driver_subset$Time,"%H:%M:%S", tz="UTC"),
                                        Tair_degC = met_driver_subset$Tair_degC,
                                        Prec_mm = met_driver_subset$Prec_mm,
                                        Atm_press_Pa = met_driver_subset$Press_Pa,
                                        RH_perc = met_driver_subset$RH_perc,
                                        VPD_kPa = met_driver_subset$VPD_kPa,
                                        PAR_umols_m2_s = met_driver_subset$PAR_umols_m2_s,
                                        Windspeed_m_s = met_driver_subset$Windspeed_m_s
  )
  
  leaf_user_met_list <- list(leaf = list(env = list(time = "'Time'", temp = "'Tair_degC'", par = "'PAR_umols_m2_s'",vpd="'VPD_kPa'",
                                                    atm_press="'Atm_press_Pa'",wind="'Windspeed_m_s'")))
  
} else {
  output_met_driver <- cbind.data.frame(Time = met_driver_subset$Time,
                                        Year = met_years,
                                        DOY = lubridate::yday(met_driver_subset$Time),
                                        Hour = strftime(met_driver_subset$Time,"%H:%M:%S", tz="UTC"),
                                        Tair_degC = met_driver_subset$Tair_degC,
                                        Prec_mm = met_driver_subset$Prec_mm,
                                        Atm_press_Pa = met_driver_subset$Press_Pa,
                                        RH_perc = met_driver_subset$RH_perc,
                                        VPD_kPa = met_driver_subset$VPD_kPa,
                                        PAR_umols_m2_s = met_driver_subset$PAR_umols_m2_s
  )
  
  leaf_user_met_list <- list(leaf = list(env = list(time = "'Time'", temp = "'Tair_degC'", 
                                                    par = "'PAR_umols_m2_s'",vpd="'VPD_kPa'",
                                                    atm_press="'Atm_press_Pa'")))
}

leaf_user_met_xml <- PEcAn.settings::listToXml(leaf_user_met_list, "met_data_translator")

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Create output met for MAAT
write.csv(output_met_driver, 
          file = file.path(met_output_path,paste0(site_name,"_NGEETropics_",met_yr_subset[1],"_",
                                                  met_yr_subset[2],"_UTC.csv")),row.names = F)
### output XML file
PREFIX_XML <- "<?xml version=\"1.0\"?>\n"
XML::saveXML(leaf_user_met_xml,
             file = file.path(met_output_path, "leaf_user_met.xml"),
             indent = TRUE,
             prefix = PREFIX_XML)
#--------------------------------------------------------------------------------------------------#
### EOF