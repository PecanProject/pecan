#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# to be run after dump.db.subset.sh

NEWDB=$1
mysqladmin create $NEWDB

for table in citation cultivar covariate pft pfts_prior pfts_specie prior site specie trait treatment variable yield
do
    mysql $NEWDB < ${table}s.sql
done
