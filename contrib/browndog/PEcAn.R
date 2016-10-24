#!/usr/bin/Rscript
# PEcAn
# data
# xml
# pecan.zip, pecan.nc, clim, ed.zip, linkages, dalec

# input files is a xml file specifying what to get
# <input>
#   <type>Ameriflux</type>
#   <site>US-Dk3</site>
#   <lat>35.9782</lat>
#   <lon>-79.0942</lon>
#   <start_date>2001-01-01 00:00:00</start_date>
#   <end_date>2001-12-31 23:59:59</end_date>
# </input>

# send all output to stdout (incl stderr)
sink(stdout(), type = "message")

# get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  allargs <- commandArgs(trailingOnly = FALSE)
  myCommand <- sub("--file=", "", allargs[grep("--file=", allargs)])
  print(paste0("Usage:    ", myCommand, " xml_Input_File  Output_File [tempDirectory] [cacheDirectory]"))
  print(paste0("Example1: ", myCommand, " US-Dk3.xml US-Dk3.pecan.nc [/tmp/watever] [/tmp/cache]"))
  print(paste0("Example2: ", myCommand, " US-Dk3.xml US-Dk3.pecan.zip [/tmp/watever] [/tmp/cache]"))
  quit()
}

# load required libraries
library(XML)
library(RPostgreSQL)
library(PEcAn.data.atmosphere)
library(PEcAn.DB)

# 1st argument is the input xml file
input <- XML::xmlToList(xmlParse(args[1]))

# 2nd argument is the output file
outputfile <- args[2]

# 3rd argument is the temp folder
ifelse(length(args) > 2, tempDir <- args[3], tempDir <- ".")

# 4th argument is the cachefolder
ifelse(length(args) > 3, cacheDir <- args[4], cacheDir <- tempDir)
cacheDir <- "/home/polyglot/cache/PEcAn"

dbparams <- list(user = "bety", dbname = "bety", password = "bety", host = "localhost")
# variable definitions
site_lat <- ifelse(is.null(input$lat), NA, input$lat)
site_lon <- ifelse(is.null(input$lon), NA, input$lon)

# connect to DB and get site name
con <- db.open(dbparams)
# query site based on location
site <- db.query(paste0("SELECT id, sitename AS name FROM sites ORDER BY st_distance(geometry, ST_GeogFromText('POINT(", 
  site_lon, " ", site_lat, ")'))  limit 1"), con)
if (length(site) < 0) {
  # query site based on name
  site <- db.query(paste0("SELECT id, sitename AS name FROM sites WHERE sitename LIKE '%", input$site, 
    "%'"), con)
}
if (length(site) < 0) {
  # insert site info
  quit(status = -1)
} else {
  # remove multiple entries.
  site <- list(id = site$id[1], name = site$name[1])
}
db.close(con)

# if no model specified make a guess based on the output requested
if (is.null(input$model)) {
  if (grepl("\\.ed.zip$", outputfile)) {
    model <- "ED2"
  } else if (grepl("\\.cf$", outputfile)) {
    model <- "LINKAGES"
  } else if (grepl("\\.clim$", outputfile)) {
    model <- "SIPNET"
  } else {
    model <- "BIOCRO"
  }
}
mettype <- ifelse(is.null(input$type), "CRUNCEP", input$type)
input_met <- list(username = "pecan", source = mettype)
start_date <- input$start_date
end_date <- input$end_date
host <- list(name = "localhost")

print("Using met.process to download files")
outfile_met <- met.process(site, input_met, start_date, end_date, model, host, dbparams, cacheDir)

# get start/end year code works on whole years only
start_year <- lubridate::year(start_date)
end_year <- lubridate::year(end_date)

# if more than 1 year, or zip specified, zip result
if (grepl("\\.zip$", outputfile) || (end_year - start_year > 1) && grepl("\\.pecan.nc$", outputfile)) {
  # folder for files with gapfilling
  folder <- dirname(outfile_met)
  outname <- basename(outfile_met)
  # get list of files we need to zip
  files <- c()
  for (year in start_year:end_year) {
    files <- c(files, files <- file.path(folder, list.files(folder, pattern = paste0("*", year, 
      "*"))))
  }
  
  # use intermediate file so it does not get marked as done until really done
  dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
  zipfile <- file.path(tempDir, "temp.zip")
  zip(zipfile, files, extras = "-j")
  # move file should be fast
  file.rename(zipfile, outputfile)
} else {
  if (!file.exists(outfile_met)) {
    outfile_met <- paste0(outfile_met, ".", start_year, ".nc")
  }
  file.link(outfile_met, outputfile)
}
