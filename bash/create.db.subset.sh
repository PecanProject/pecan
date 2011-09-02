#!/bin/bash

# to be run after dump.db.subset.sh

NEWDB={$1:-"pecan_subset"}
mysqladmin create $NEWDB

for table in citation cultivar covariate pft pfts_prior pfts_specie prior site specie trait treatment variable yield
do
    mysql pecan_subset < ${table}s.sql
done
