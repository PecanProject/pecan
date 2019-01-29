####################################################################################################
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
met_path <- file.path('/Volumes/data/Model_Data/sites/PA-SLZ/NGEETropics_source/')
met_drivers <- read.csv(file = file.path(met_path,'SanLorenzo_Sherman_met_drivers_2008-2017.csv'), header=T)
met_output_path <- file.path('/Volumes/data/Model_Data/sites/PA-SLZ/MAAT_drivers/')

## Options
site_name <- "PA-SLZ"
pressure <- FALSE
wind <- TRUE
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Subset met
names(met_drivers)
head(met_drivers)
date_range <- unique(lubridate::year(met_drivers$Date_UTC_start))

met_yr_subset <- c(2015,2016)
MAAT_Time <- lubridate::as_datetime(met_drivers$Date_UTC_start, tz = "UTC")
head(MAAT_Time)

met_years <- lubridate::year(MAAT_Time)
met_drivers$Time <- MAAT_Time
met_drivers$PAR_umols_m2_s <- met_drivers$SR..W.m2.*2.114
met_drivers$Tair_degC <- met_drivers$Ta..oC.
met_drivers$RH_perc <- met_drivers$RH....
met_drivers$VPD_kPa <- PEcAn.data.atmosphere::get.vpd(met_drivers$RH...., met_drivers$Tair_degC) / 10
met_drivers$Prec_mm <- met_drivers$Rain..mm.min.*30  # converting to mm per 30 min period

# get additional variables
if (pressure){
  met_drivers$Press_Pa <- udunits2::ud.convert(met_drivers$BP_hPa, "mmHg", "Pa")  # need to match this with source, when availible 
} else {
  met_drivers$Press_Pa <- rep(101325,length(met_drivers$Time))
}

if (wind) {
  met_drivers$Windspeed_m_s <- udunits2::ud.convert(met_drivers$WS..km.h., "km/h", "m/s")
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
          file = file.path(met_output_path,paste0(site_name,"_NGEET_",met_yr_subset[1],"_",
                                                  met_yr_subset[2],"_UTC.csv")),row.names = F)
### output XML file
PREFIX_XML <- "<?xml version=\"1.0\"?>\n"
XML::saveXML(leaf_user_met_xml,
             file = file.path(met_output_path, "leaf_user_met.xml"),
             indent = TRUE,
             prefix = PREFIX_XML)
#--------------------------------------------------------------------------------------------------#
### EOF