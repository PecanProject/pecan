# --------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
# --------------------------------------------------------------------------------

#library(PEcAn.visualization)

# ----------------------------------------------------------------------
# COMMAND LINE ARGUMENTS
# ----------------------------------------------------------------------
# arguments are-args year variable
args     <- commandArgs(trailingOnly = TRUE)
datafile <- args[1]
year     <- args[2]
xvar     <- 'time'
yvar     <- args[3]
width    <- as.numeric(args[4])
height   <- as.numeric(args[5])
filename <- args[6]

# datafile="../../output/PEcAn_14/out/23/2006.nc"
# year=2006
# xvar='time'
# yvar="GPP"
# width=800
# height=600
# filename="plot.png"
 
#error_reporting(E_ALL | E_STRICT);
PEcAn.visualization::plot.netcdf(datafile, yvar, xvar, width, height, filename, year);
 
