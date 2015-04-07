#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
library(PEcAn.rtm)
#################### Some examples for testing #####################

## Load test data
data(poplar)  # test poplar dataset
data(clover)  # test clover dataset
data(beech)   # test beech dataset

spectra = clover  # chosen spectra to invert
plot(spectra[,1],spectra[,2],type="l",lwd=2.5,ylim=c(0,1))
lines(spectra[,1],1-spectra[,3],lwd=2.5,col="dark grey")
box(lwd=2.2)

## Test PROSPECT inversion
waves <- spectra[,1]
refl <- spectra[,2]
tran <- spectra[,3]

## PROSPECT-4
inv = invprospect(refl,tran,model=4,method="DEoptim",strategy=2,threshold=0.01,cpus=4)
plot.prospect.inv(inv,outdir='/Users/serbin/Data/',file='test_prospect.inv4')

## PROSPECT-5
inv = invprospect(refl,tran,model=5,"DEoptim",strategy=2,threshold=0.01,cpus=4)
plot.prospect.inv(inv,outdir='/Users/serbin/Data/',file='test_prospect.inv5')


