# Data preprocessing workflows for irrigation inputs

This directory uses `pixi` for dependency management and `clustermq` for parallelizing work automatically across an SGE cluster (like the BU SCC).

## CIMIS reference ET

Prerequisites: Raw CIMIS ETref data downloaded from spatialcimis.water.ca.gov.

- `cimis-01-weights.R` --- Pre-calculate the area weights of CIMIS ETref pixels for each harmonized LandIQ polygon using `exactextractr` (slow)
- `cimis-02-extract.R` --- Apply the weights to each CIMITS ETref raster file (fast)
- `cimis-03-combine.sql` --- Recombine the results into a properly hive-partitioned parquet dataset.

## CHIRPS

Prerequisites: Raw CHIRPS v2 data downloaded from https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/netcdf/p05/.

- `chirps-preprocess.R` --- Entire workflow. For each year, use `exactextractr::exact_extract` to pull out the area-weighted values of CHIRPS for each harmonized LandIQ parcel.

## SSURGO soil data

Prerequisites: A downloaded copy of gSSURGO for all of California (https://nrcs.app.box.com/v/soils/folder/233398887779).

- `ssurgo-01-spatial-weights.R` --- Calculate the area weights of each SSURGO mapping unit for each harmonized LandIQ parcel. Parallelized across batches of parcels.
- `ssurgo-02-combine.R` --- Recombine the batches into a single parquet file. 
