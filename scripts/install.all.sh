for f in common utils db modules/meta.analysis modules/uncertainty modules/ensemble models/ed models/sipnet all
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
do
  R CMD build $f && R CMD INSTALL $f
done
