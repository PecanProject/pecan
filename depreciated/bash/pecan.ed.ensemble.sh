#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
R --vanilla < ./rscripts/write.configs.R

for i in config*xml; do mv "${i}" "${i/config/c}"; done
for i in *_*xml; do mv "${i}" "${i/_/}"; done
for i in *_*xml; do mv "${i}" "${i/_/}"; done
for i in *factor*xml; do mv "${i}" "${i/factor/}"; done
for i in *root*xml; do mv "${i}" "${i/root/rt}"; done
for i in *turnover*xml; do mv "${i}" "${i/turnover/tnvr}"; done
for i in *conductance*xml; do mv "${i}" "${i/conductance/cndctnc}"; done
for i in *respiration*xml; do mv "${i}" "${i/respiration/resp}"; done
for i in *nonlocaldispersal*xml; do mv "${i}" "${i/nonlocaldispersal/nldisprs}"; done
for i in *quantumefficiency*xml; do mv "${i}" "${i/quantumefficiency/quantef}"; done
for i in *water*xml; do mv "${i}" "${i/water/h2o}"; done
for i in *stomatalslope*xml; do mv "${i}" "${i/stomatalslope/stmslope}"; done
for i in c*xml; do mv "${i}" "${i/.xml/}"; done

tar zcf saconfigs.tgz c*  
