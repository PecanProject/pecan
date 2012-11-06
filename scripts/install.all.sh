#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

for f in utils common db modules/meta.analysis modules/uncertainty modules/emulator modules/assim.batch modules/assim.sequential modules/data.land modules/priors modules/rtm models/ed models/sipnet models/biocro all

do
  R CMD build $f && R CMD INSTALL $f
done
