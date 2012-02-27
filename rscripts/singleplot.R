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
start_date <- as.Date(settings$run$start.date)
start_year <- format(start_date, "%Y")
end_date <- as.Date(settings$run$end.date)
end_year <- format(end_date, "%Y")

# arguments are --args year variable
args <- commandArgs(trailingOnly = TRUE)
year <- args[1]
var  <- args[2]
png  <- args[3]

# This depends on NL%FRQFAST in template and should reflect the number of
# data points written per day.
values_day <- 24

umol2gc <- 1.0368

if (year == start_year) {
	start_day <- as.numeric(format(start_date, "%j")) - 1
} else {
	start_day <- 0
}
if (year == end_year) {
	end_day <- as.numeric(format(end_date, "%j")) - 1
} else {
	end_day <- as.numeric(format(as.Date(sprintf("%s-12-31", year)), "%j")) - 1
}

filename <- list.files(settings$run$host$outdir, full.names=TRUE,pattern=paste('.*-T-', year, '-.*.h5', sep=''))[1]
data <- hdf5load(filename, load = FALSE)

lastval    <- (values_day*(1+end_day-start_day))
aggrlist   <- list(rep(start_day:end_day, each=values_day))

if (var == "Reco") {
	PLANT_RESP  <- subset(aggregate(data$AVG_PLANT_RESP[1:lastval], by=aggrlist, FUN=mean), select=-Group.1)
	HTROPH_RESP <- subset(aggregate(data$AVG_HTROPH_RESP[1:lastval], by=aggrlist, FUN=mean), select=-Group.1)
	val         <- (PLANT_RESP + HTROPH_RESP) * umol2gc
	units       <- "unknown"
	title       <- var
} else if (var == "NPP") {
	GPP         <- subset(aggregate(data$AVG_GPP[1:lastval], by=aggrlist, FUN=mean), select=-Group.1)
	PLANT_RESP  <- subset(aggregate(data$AVG_PLANT_RESP[1:lastval], by=aggrlist, FUN=mean), select=-Group.1)
	val         <- (GPP - PLANT_RESP)  * umol2gc
	units       <- "unknown"
	title       <- var
} else if (var == "NEE") {
	GPP         <- subset(aggregate(data$AVG_GPP[1:lastval], by=aggrlist, FUN=mean), select=-Group.1)
	HTROPH_RESP <- subset(aggregate(data$AVG_HTROPH_RESP[1:lastval], by=aggrlist, FUN=mean), select=-Group.1)
	PLANT_RESP  <- subset(aggregate(data$AVG_PLANT_RESP[1:lastval], by=aggrlist, FUN=mean), select=-Group.1)
	val         <- (GPP - (PLANT_RESP + HTROPH_RESP))  * umol2gc
	units       <- "unknown"
	title       <- var
} else {
	if (is.null(data[[var]])) {
		var <- sprintf("AVG_%s", var)
	}
	val         <- aggregate(data[[var]][1:lastval], by=aggrlist, FUN=mean)
	metadata    <- attr(data[[var]], "Metadata")
	title       <- sub('[[:space:]]+$', '', gsub("Long Name: (.*)", "\\1", metadata[pmatch("Long Name:", metadata)]))
	units       <- gsub("Units: \\[(.*)\\] *", "\\1", metadata[pmatch("Units:", metadata)])
}

plot <- qplot(Group.1, x, data=val, main=sprintf("%s %s", year, title), geom=c('smooth','point'), span=0.2, xlab="day of year", ylab=units)
png(filename=png)
print(plot)
dev.off()
