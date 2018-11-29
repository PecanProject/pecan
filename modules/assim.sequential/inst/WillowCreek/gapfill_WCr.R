##'@title gapfill_WCr
##'@section purpose:
##'This function uses the REddyProc package to gap fill half-hourly data 
##'from the Willow Creek Ameriflux tower
##'
##'@param start_date First date of data download
##'@param end_date End date of data download
##'@param var variable (NEE or LE) that you want to do data assimilation on 
##'@return data frame of gapfilled data
##'@export
##'@author Luke Dramko and K. Zarada

gapfill_WCr <- function(start_date, end_date,
                        var, nsample=10,
                        FUN.met=download_US_WCr_met,
                        FUN.flux=download_US_WCr_flux){


start_date <- as.Date(start_date)
end_date <- as.Date(end_date)

#download WCr flux and met date 
flux <- FUN.flux(start_date, end_date) 
met <- FUN.met(start_date, end_date) 
# converting NEE and LE 
#change -999 to NA's 
flux[flux == -999] <- NA
flux$NEE<-PEcAn.utils::misc.convert(flux$NEE, "umol C m-2 s-1", "kg C m-2 s-1")
flux$LE<-flux$LE*1e-6
#join met and flux data by date (which includes time and day)
met <- met %>% dplyr::select(date, Tair, Rg, Tsoil)
flux <- left_join(flux, met, by = "date") %>%
          dplyr::select(-FjDay, -SC, -FC) 
#print(str(flux))


#Start REddyProc gapfilling
suppressWarnings({

  EddyDataWithPosix.F <-
    fConvertTimeToPosix(flux,
                        'YDH',
                        Year.s = 'Year'
                        ,
                        Day.s = 'DoY',
                        Hour.s = 'Hour') %>%
    dplyr::select(-date,-Month,-Day)
})


EddyProc.C <- sEddyProc$new('WCr', EddyDataWithPosix.F, 
                            c(var,'Rg','Tair', 'Ustar'))

tryCatch(
  {
    EddyProc.C$sMDSGapFill(var)
  },
  error = function(e) {
    PEcAn.logger::logger.warn(e)
  }
)

#Merging the output
FilledEddyData.F <- EddyProc.C$sExportResults()
CombinedData.F <- cbind(flux, FilledEddyData.F)

return(CombinedData.F)

}


#LE.g <- gapfill_WCr(start_date = "2017-01-01", end_date = "2017-12-31", var = "NEE")










