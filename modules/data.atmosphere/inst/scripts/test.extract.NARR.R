source("closest_xy.R")
source("extract.NARR.R")

infolder <- "/projectnb/cheas/gapmacro/NARR/NewNARR"
infile <- ""
outfolder <- "/projectnb/cheas/mandifore/rainier/NARR"

# Mt Rainier, WA
slat <- 46.8529  
slon <- -121.7604 

extract.NARR(slat,slon,infolder,infile,outfolder)
