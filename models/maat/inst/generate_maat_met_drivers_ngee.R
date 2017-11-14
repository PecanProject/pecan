####################################################################################################
#
#    --- Last updated: 11.13.2017 By Shawn P. Serbin <sserbin@bnl.gov>
####################################################################################################


#---------------- Close all devices and delete all variables. -------------------------------------#
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files

library(lubridate)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Load met driver data
met_path <- file.path('/Volumes/data/Model_Data/sites/PA-Bar/MAAT_drivers/QAQC_2017_04_07/')
met_drivers <- read.csv(file = file.path(met_path,'BCI_met_drivers_2003_2016_edit.csv'), header=T)
met_output_path <- file.path('/Volumes/data/Model_Data/sites/PA-Bar/MAAT_drivers/')
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Subset met
met_yr_subset <- c(2016,2016)
Time <- mdy_hm(as.character(met_drivers$Date_UTC_start))
met_years <- year(Time)
met_drivers$Time <- Time
met_drivers$PAR_umols_m2_s <- met_drivers$SR_W_m2*2.114
met_drivers$Tair_degC <- met_drivers$Temp_o_C
met_drivers$VPD_kPa <- PEcAn.data.atmosphere::get.vpd(met_drivers$RH_perc, met_drivers$Temp_o_C) / 10
met_drivers$Press_Pa <- udunits2::ud.convert(met_drivers$BP_hPa, "mmHg", "Pa")

# subset
if (met_yr_subset[2]-met_yr_subset[1] != 0 ) {
  met_driver_subset <- subset(met_drivers, met_years == c(met_yr_subset[1], met_yr_subset[2]))
} else {
  met_driver_subset <- subset(met_drivers, met_years == met_yr_subset[1])
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Create output met for MAAT
write.csv(met_driver_subset, 
          file = file.path(met_output_path,paste0("PA-Bar_NGEET_",met_yr_subset[1],"_",
                                                  met_yr_subset[2],"_1hr_UTC.csv")),row.names = F)
#--------------------------------------------------------------------------------------------------#
### EOF