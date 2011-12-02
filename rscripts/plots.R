library(XML)
library(ggplot2)
library(hdf5)

# ----------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------
settings.file <- Sys.getenv('PECANSETTINGS')
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)

# ----------------------------------------------------------------------
# CREATE PLOTS
# ----------------------------------------------------------------------
start_date <- as.Date(settings$run$start.date, format="%Y/%m/%d")
start_year <- format(start_date, "%Y")
end_date <- as.Date(settings$run$end.date, format="%Y/%m/%d")
end_year <- format(end_date, "%Y")

# This depends on NL%FRQFAST in template and should reflect the number of
# data points written per day.
values_day <- 24

umol2gc <- 1.0368

for (year in start_year:end_year) {
  if (year == start_year) {
    start_day <- as.numeric(format(start_date, "%j"))
  } else {
    start_day <- 1
  }
  if (year == end_year) {
    end_day <- as.numeric(format(end_date, "%j"))
  } else {
    end_day <- as.numeric(format(as.Date(sprintf("%s-12-31", year)), "%j"))
  }

  filename <- list.files(settings$run$host$outdir, full.names=TRUE,pattern=paste('.*-T-', year, '-.*.h5', sep=''))[1]
  data <- hdf5load(filename, load = FALSE)

  GPP         <- data$AVG_GPP[1:(values_day*(1+end_day-start_day))]
  PLANT_RESP  <- data$AVG_PLANT_RESP[1:(values_day*(1+end_day-start_day))]
  HTROPH_RESP <- data$AVG_HTROPH_RESP[1:(values_day*(1+end_day-start_day))]
  Reco        <- (PLANT_RESP + HTROPH_RESP) * umol2gc
  NPP         <- (GPP - PLANT_RESP)  * umol2gc
  NEE         <- (GPP - (PLANT_RESP + HTROPH_RESP))  * umol2gc

  gpp.year <- qplot(start_day:end_day, Reco, geom=c('smooth','point'), span=0.2, xlab='doy', ylab='Reco')
  png(filename=sprintf("%d-Reco-year.png", year))
  print(gpp.year)
  dev.off()

  gpp.year <- qplot(start_day:end_day, NPP, geom=c('smooth','point'), span=0.2, xlab='doy', ylab='NPP')
  png(filename=sprintf("%d-NPP-year.png", year))
  print(gpp.year)
  dev.off()

  gpp.year <- qplot(start_day:end_day, NEE, geom=c('smooth','point'), span=0.2, xlab='doy', ylab='NEE')
  png(filename=sprintf("%d-NEE-year.png", year))
  print(gpp.year)
  dev.off()
}

#  gpp.week <- qplot(600:624, data$AVG_GPP[600:624], geom = c('line','point'), xlab = 'doy', ylab = 'GPP')
#  png(filename=format("%d-week.png", year))
#  png(filename="week.png")
#  print(gpp.week)
#  dev.off()

