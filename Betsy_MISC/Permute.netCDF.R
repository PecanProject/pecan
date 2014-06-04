#!/usr/bin/env Rscript

# Rechunk dimensions and convert the  time dimension from unlimited to fixed.

in.path <- "/projectnb/cheas/pecan.data/input/NARR_CF_Rechunk/"
prefix <- "NARR."
out.path <- "/projectnb/cheas/pecan.data/input/NARR_CF_Permute/"

## get file names

files = dir(in.path,prefix)
files = files[grep(pattern="*.nc",files)]

if(length(files) == 0) {
  return(NULL)
}  

if(!file.exists(out.path)){
  dir.create(out.path)
}

## Rechunk dimensions and convert the  time dimension from unlimited to fixed.

for(i in 1:length(files)){    
  infile = file.path(in.path,files[i])
  outfile = file.path(out.path,files[i])
  if(file.exists(infile)==TRUE && file.exists(outfile)==FALSE){
    system(paste("ncpdq --no_tmp_fl -h -O -a y,x,time ",infile," ",outfile))
  }
}

