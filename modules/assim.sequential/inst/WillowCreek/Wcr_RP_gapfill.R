library(REddyProc)
library(dplyr)

start_date <- as.Date("2017-01-01")
end_date <- as.Date("2017-12-31")
# 
download_US_WCr(start_date, end_date) ->flux
download_US_WCr_met(start_date, end_date) ->met
# 

colnames(flux) <- c("Year", "Month", "Day", "Hour", "DoY", "FjDay", "SC", "FC", "NEE", "LE", "H", "Ustar", "Flag", "date")

colnames(met) <- c("Year", "Month", "Day", "Hour", "DoY", "FjDay", "Tair", "rH", "Tsoil", "Rg", "date")

met <- met %>% select(date, Tair, Rg, Tsoil)
flux <- left_join(flux, met, by = "date") %>% select(-FjDay, -SC, -FC, -Flag) 
flux[flux == -999] <- NA

EddyDataWithPosix.F <- fConvertTimeToPosix(flux, 'YDH',Year.s = 'Year'
                                           ,Day.s = 'DoY',Hour.s = 'Hour') %>% select(-date, -Month, -Day)

EddyProc.C <- sEddyProc$new('WCr', EddyDataWithPosix.F, 
                            c('NEE','Rg','Tair', 'Ustar'))

EddyProc.C$sPlotFingerprintY('NEE', Year = 2017)

uStarTh <- EddyProc.C$sEstUstarThresholdDistribution(UstarColName = "Ustar", NEEColName = "NEE", 
                                                     TempColName = "Tair", RgColName = "Rg", 
                                                     nSample = 100L, probs = c(0.05, 0.5, 0.95)) 

select(uStarTh, -seasonYear)

uStarThAnnual <- usGetAnnualSeasonUStarMap(uStarTh)[-2]
uStarSuffixes <- colnames(uStarThAnnual)[-1]
print(uStarThAnnual)

EddyProc.C$sMDSGapFillAfterUStarDistr('NEE',
                                      UstarThres.df= uStarThAnnual,
                                      UstarSuffix.V.s = uStarSuffixes,
                                      FillAll = TRUE
)

grep("NEE_.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)


EddyProc.C$sPlotFingerprintY('NEE_U05_f', Year = 2017)


EddyProc.C$sPlotFingerprintY('NEE_U50_f', Year = 2017)

EddyProc.C$sPlotFingerprintY('NEE_U95_f', Year = 2017)


FilledEddyData.F <- EddyProc.C$sExportResults()
CombinedData.F <- cbind(flux, FilledEddyData.F)

library(highcharter)
library(timetk)

CombinedData.F %>%
  mutate(Date=as.Date(DoY, origin = paste0(Year,"-01-01"))) %>%
  dplyr::select(NEE_U05_f,NEE_U50_f,NEE_U95_f,Date)%>%
  timetk::tk_xts()->readytoplot

highchart(type = "stock") %>% 
  hc_add_theme(hc_theme_538())%>% 
  hc_title(text = "NEE FLUX uncertainty") %>% 
  hc_add_series(readytoplot[,1], name="NEE_U05_f") %>% 
  hc_add_series(readytoplot[,2], name="NEE_U50_f") %>%
  hc_add_series(readytoplot[,3], name="NEE_U95_f")











