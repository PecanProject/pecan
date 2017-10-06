#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


library("plotrix")
set.seed(5)
testdata <- data.frame(site=c(1,1,1,2,2,3),
                   date=c(2001,2001,2002,2003,2004,2005),
                   obs=rnorm(6,10,2),model1=rnorm(6,10,3),model2=rnorm(6,10,3))

set.seed(1)
testdata <- data.frame(site=c(1,1,1,2,2,3),
                   date=c(2001,2001,2002,2003,2004,2005),
                   obs=rnorm(6,10,2),model1=rnorm(6,10,3)+2,model2=rnorm(6,11,3)+2)

taylor.diagram(testdata$obs,testdata$model1,pos.cor=FALSE)
taylor.diagram(testdata$obs,model=testdata$model2,add=TRUE,col="blue")
