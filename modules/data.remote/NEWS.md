# PEcAn.data.remote 1.9.1

## Added

* New function `extract_thredds_nc` collects data from a Thredds URL into a single dataframe (#2458).
* New function `get_site_info` looks up in the BETY database all sites found in a settings object (#2458).
* new function `merge_image_tiles` stitches multiple hdf or tif files into a larger spatial image (#3573, #3617).
* New function `gdal_conversion` provides an R interface to format conversion utilities provided by the GDAL library (#3585, #3588).


## Changed

* `MODIS_LAI_prep` gains two arguments (#3565):
	- `skip_download` (with default FALSE) to work offline from an existing file named "LAI.csv".
	- `boundary` (with default NULL, ie no effect) to set upper and lower quantiles for trimming LAI data
* `download.thredds.AGB` renamed to `download.thredds` (#2458).
* `GEDI_AGB_prep` argument `credential.folder` (with default "~") renamed to `credential_path` (with default "~/.netrc") (#3540).
* `GEDI_AGB_prep` will use existing local files if present rather than re-download them (#3572).
* All `*DAAC` functions now take argument `credential_path`, which should be set to the path to a valid `.netrc` file (#3572).



# PEcAn.data.remote 1.9.0

* Refactored GEDI, LAI, and SMAP workflows for more efficient parallel processing
* Added `GEDI_L4A*` functions to work with footprint-level GEDI biomass data


# PEcAn.data.remote 1.8.0

## Internal changes

* `call_MODIS` now checks QC flags using base R functions, and therefore no longer depends on the `binaryLogic` package.

# PEcAn.data.remote 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see 
https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
