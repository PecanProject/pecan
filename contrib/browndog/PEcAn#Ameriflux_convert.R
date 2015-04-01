#!/usr/bin/Rscript
#PEcAn
#data
#ameriflux.zip
#pecan.zip

.libPaths("/home/polyglot/R/library")
sink(stdout(),type="message")

# get command line arguments
args <- commandArgs(trailingOnly = TRUE)

filename <-basename(args[1])
underPos <-which(strsplit(filename, "")[[1]]=="_")[1]
isANumber=substr(filename,start=1,stop=underPos-1)
if (  !suppressWarnings(!is.na(as.numeric(isANumber)))   ) {
    underPos <- 0
}

if (length(args) < 2) {
    myCommand <- substr(commandArgs()[4],10,1000000L)
    print(paste0("Usage:    ", myCommand, " amfx-nc_Input_File  cf-nc_Output_File [tempDirectory]"))
    print(paste0("Example1: ", myCommand, " US-Dk3.ameriflux.nc US-Dk3.pecan.nc [/tmp/watever] "))
    print(paste0("Example2: ", myCommand, " US-Dk3.ameriflux.nc US-Dk3.pecan.zip [/tmp/watever] "))
    print(paste0("Example3: ", myCommand, " US-Dk3.ameriflux.zip US-Dk3.pecan.nc [/tmp/watever] "))
    print(paste0("Example4: ", myCommand, " US-Dk3.ameriflux.zip US-Dk3.pecan.zip [/tmp/watever] "))
    q()
} else {
    dotPos <- which(strsplit(args[1], "")[[1]]==".")
    extI <- substr(args[1],start=dotPos[1], stop=nchar(args[1]))
    dotPos <- which(strsplit(args[2], "")[[1]]==".")
    extO <- substr(args[2],start=dotPos[1], stop=nchar(args[2]))
    
    #site  <- substr(filename,start=1+underPos,stop=6+underPos)
    
    if (extI == ".ameriflux.nc") {
        tempVar <- system(paste0("ncdump -h ", args[1], " | grep YEAR:valid_min"), intern = TRUE, ignore.stderr = TRUE)
        yearS <- substr(tempVar,star=20,stop=23)
        yearE <- substr(tempVar,star=20,stop=23)
    } else {
        tempVar <- system(paste0("unzip -l ", args[1], " | grep .nc |head -1 | awk '{print $4}' "), intern = TRUE, ignore.stderr = TRUE)
        dotPos <- which(strsplit(tempVar, "")[[1]]==".")
        site <-substr(tempVar,start=1,stop=dotPos[1]-1)
        tempVar <- system(paste0("unzip -l ", args[1], " | grep .nc |head -1 | awk '{print $4}' "), intern = TRUE, ignore.stderr = TRUE)
        dotPos <- which(strsplit(tempVar, "")[[1]]==".")
        yearS <- substr(tempVar,star=dotPos[1]+1,stop=dotPos[2]-1)
        tempVar <- system(paste0("unzip -l ", args[1], " | grep .nc |tail -1 | awk '{print $4}' "), intern = TRUE, ignore.stderr = TRUE)
        dotPos <- which(strsplit(tempVar, "")[[1]]==".")
        yearE <- substr(tempVar,star=dotPos[1]+1,stop=dotPos[2]-1)
    }    
    require(PEcAn.data.atmosphere)
    require(PEcAn.ED2)
    require(PEcAn.SIPNET)
}


if (length(args) > 2) {
    tempDir <- args[3]
} else {
    tempDir <- "temp"    
}

# variables definition
start_date <- as.POSIXlt(paste0(yearS,"-01-01 00:00:00"), tz = "GMT")
end_date <- as.POSIXlt(paste0(yearE,"-12-31 23:59:59"), tz = "GMT")
overwrite <- TRUE
verbose <- TRUE

# 0 create folders
rawfolder <- file.path(tempDir, "raw")
dir.create(rawfolder, showWarnings=FALSE, recursive=TRUE)

cffolder <- file.path(tempDir,"cf")
dir.create(cffolder, showWarnings=FALSE, recursive=TRUE)

gapfolder <- file.path(tempDir,"gap")
dir.create(gapfolder, showWarnings=FALSE, recursive=TRUE)

# 1 copy input file to input directory
# and unzip if needed 
if (extI == ".ameriflux.zip") {
    #file.copy(args[1],paste0(rawfolder,"/",args[1]))
    file.copy(args[1],paste0(rawfolder,"/"))
    wd <- getwd()
    rootZip <- paste0(tempDir,"/raw")
    setwd(rootZip)
    system(paste0("unzip  ./*"))
    #file.remove(args[1])
    setwd(wd)
} else {
    file.copy(args[1],paste0(tempDir,"/raw/",site,".",yearS,".nc"))
    end_date <-start_date
    #file.copy(args[1],paste0(tempDir,"/raw"))
} 


# 2 convert data to CF
#print(met2CF.Ameriflux(rawfolder, site, cffolder, start_date=start_date, end_date=end_date, overwrite=overwrite))
met2CF.Ameriflux(rawfolder, site, cffolder, start_date=start_date, end_date=end_date, overwrite=overwrite)
metgapfill(cffolder, site, gapfolder, start_date=start_date, end_date=end_date)

# 3 zip (if needed) and deliver
if (extO == ".pecan.zip") {
    wd <- getwd()
    rootZip <- paste0(tempDir,"/gap")
    setwd(rootZip)
    system("zip temp.zip ./*")
    #file.rename("temp.zip", paste0(wd,"/", args[2]))   
    file.rename("temp.zip", args[2])   
    setwd(wd)
} else {
    file.rename(paste0(tempDir,"/gap/",site,".",yearS,".nc"),args[2])
}

#if (length(args) > 2) {
#    unlink(args[3], recursive = TRUE) 
#} else {
#    unlink(tempDir, recursive = TRUE) 
#}

#filename <- paste0(site,".",substr(start_date,1,4),".nc")
#outfile <- file.path(cffolder, filename)
#file.rename(outfile,args[2])

#unlink(tempDir, recursive = TRUE)
