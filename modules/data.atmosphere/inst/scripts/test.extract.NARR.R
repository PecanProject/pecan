source("closest_xy.R")
source("extract.NARR.R")

in.path <- "/projectnb/cheas/pecan.data/input/NARR_CF"
in.prefix <- ""
outfolder <- "/projectnb/cheas/pecan.data/input/NARR_CF_test"

# Mt Rainier, WA
slat <- 46.8529  
slon <- -121.7604 

start_year <- 2012
end_year   <- 2013

extract.NARR(in.path,in.prefix,outfolder,slat,slon,start_year,end_year)
