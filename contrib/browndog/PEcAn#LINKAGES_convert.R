#!/usr/bin/Rscript
#PEcAn
#data
#pecan.zip
#linkages

sink(stdout(),type="message")

# global variables
overwrite <- TRUE
verbose <- TRUE

# get command line arguments
args <- commandArgs(trailingOnly = TRUE)

usage <- function(msg) {
    print(msg)
    print(paste0("Usage:    ", args[0], " cf-nc_Input_File outputfile [tempfolder]"))
    print(paste0("Example1: ", args[0], " US-Dk3.pecan.nc US-Dk3.txt  [/tmp/watever]"))
    print(paste0("Example2: ", args[0], " US-Dk3.pecan.zip US-Dk3.txt [/tmp/watever]"))
    stop()
}

# parse/check arguments
if (length(args) < 2) {
    usage("Not enough arguments")
}
if (length(args) > 2) {
    tempDir <- args[3]
} else {
    tempDir <- "temp"    
}

inputFile <- args[1]
outputFile <- args[2]

# create folders
cffolder <- file.path(tempDir,"cf")
dir.create(cffolder, showWarnings=FALSE, recursive=TRUE)

outfolder <- file.path(tempDir,"linkages")
dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)

# unzip and parse filenames
if (grepl("pecan.zip$", args[1])) {
    system2("/usr/bin/unzip", c("-o", "-d", cffolder, inputFile))
    site <- NA
    startYear <- NA
    endYear <- NA
    for(file in list.files(path=cffolder, pattern="*.nc")) {
        pieces <- strsplit(file, ".", fixed=TRUE)[[1]]
        if (length(pieces) != 3) {
          usage(paste0("invalid file ", file, " should be <site>.<year>.nc"))
        }
        if (is.na(site)) {
            site <- pieces[1]
        } else if (site != pieces[1]) {
            usage(paste0("incosistent sites ", file, " should be ", site, ".<year>.nc"))
        }
        if (is.na(startYear) || pieces[2] < startYear) {
            startYear <- pieces[2]
        }
        if (is.na(endYear) || pieces[2] > endYear) {
            endYear <- pieces[2]
        }
        startDate <- as.POSIXlt(paste0(startYear,"-01-01 00:00:00"), tz = "GMT")
        endDate <- as.POSIXlt(paste0(endYear,"-12-31 23:59:59"), tz = "GMT")
    }
} else if (grepl("pecan.nc$", inputFile)) {
    pieces <- strsplit(inputFile, ".", fixed=TRUE)[[1]]
    if (length(piecesx) != 4) {
      usage("Input file name should be of format <site>.<year>.pecan.nc")
    }
    site <- pieces[1]
    year <- pieces[2]
    file.copy(inputFile, file.path(cffolder, paste(site, year, "nc", sep=".")))
    startDate <- as.POSIXlt(paste0(year,"-01-01 00:00:00"), tz = "GMT")
    endDate <- as.POSIXlt(paste0(year,"-12-31 23:59:59"), tz = "GMT")
} else {
    usage("Did not recognize type of file")
}

# convert CF to output, in this case ed.zip
require(PEcAn.LINKAGES)
result <- met2model.LINKAGES(cffolder, site, outfolder, start_date=startDate, end_date=endDate, overwrite=overwrite)

# next rename outfile to output file
file.rename(result$file, outputFile)
