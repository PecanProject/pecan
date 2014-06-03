#---------------- Load libraries. -----------------------------------------------------------------#
require(PEcAn.all)
require(RPostgreSQL)
#--------------------------------------------------------------------------------------------------#

# Dowload 
outfolder  <- "/projectnb/cheas/pecan.data/input/NARR/"
start_year <- 2012
end_year   <- 2013
pkg        <- "PEcAn.data.atmosphere"
NARR.host  <- "geo.bu.edu"

NARR_raw.id <- raw.NARR(outfolder,start_year,end_year,pkg,NARR.host) 
# NARR_raw.id should be 285

#--------------------------------------------------------------------------------------------------#
# Update NARR_CF
input.id  <-  NARR_raw.id
outfolder <- "/projectnb/cheas/pecan.data/input/NARR_CF_Permute/"
pkg       <- "PEcAn.data.atmosphere"
fcn       <- "met2cf.NARR"
write     <-  TRUE
username  <- ""

NARR_cf.id <- convert.input(input.id,outfolder,pkg,fcn,write,username) # doesn't update existing record
# NARR_cf.id should be 288

#--------------------------------------------------------------------------------------------------#
# Extract for location
input.id <- NARR_cf.id
newsite  <- 1161
str_ns   <- paste0(newsite %/% 1000000000, "-", newsite %% 1000000000)

outfolder <- paste0("/projectnb/cheas/pecan.data/input/NARR_CF_site_",str_ns,"/")
pkg       <- "PEcAn.data.atmosphere"
fcn       <- "extract.NARR"
write     <- TRUE
username  <- ""

NARR_extract.id <- convert.input (input.id,outfolder,pkg,fcn,write,username,newsite = newsite)

#  input.id <- 288
#  l <- list(newsite=ns)
#  filename<-outfolder; siteid<-site$id; startdate<-paste(input$start_date); enddate<-paste(input$end_date); mimetype; formatname; parentid<-input$id; con<-con; hostname<-machine$hostname; name<-outname





