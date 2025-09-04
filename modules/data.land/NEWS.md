# Unreleased

* Add function `clip_and_save_raster_file()` for subsetting rasters to match a polygon of interest (#3537).
* New utility script `IC_SOILGRID_Utilities.R` for processing SoilGrids data to generate soil carbon initial condition (IC) files. This includes (#3508):
  - **`soilgrids_ic_process`**: A function to extract, process, and generate ensemble members from SoilGrids250m data.
  - **`preprocess_soilgrids_data`**: A helper function to handle missing values and ensure data integrity during preprocessing. 
  - **`generate_soilgrids_ensemble`**: A function to create ensemble members for a site based on processed soil carbon data. 

# PEcAn.data.land 1.8.2
- Removed unused parameter `machine` from put_veg_module()
- Fixed "external pointer is not valid" error and addressed key bugs in `soilgrids_soilC_extract()` function (#3506)


# PEcAn.data.land 1.8.1

* Dependency `datapack` is now optional. It is only used by `dataone_download()` (#3373).
* `soilgrids_soilC_extract()` no longer returns an empty dataframe when none of the queried locations are missing data. (#3409)
* Functions that name output folders from `settings$siteID` no longer assume IDs are numeric


# PEcAn.data.land 1.8.0

## Added

* New function `soilgrids_soilC_extract` retrieves soil C estimates with uncertainty from the ISRIC SoilGrids 250m data. (#3040, @Qianyuxuan)
* Included all relevant carbon pools (`ROOT_BIOMASS`, `AG_BIOMASS`, `SOIL_STOCK`, `LIT_BIOMASS`) in BADM-based IC extraction; excluded non-pool variables like `SOIL_CHEM`.
* Added explicit support for `LIT_BIOMASS` to fully utilize **BADM** biomass capabilities.
* Added `test-IC_BADM_Utilities.R` to validate BADM initial condition extraction and processing
* `extract_soil_gssurgo` now supports spatial sampling using a grid of user-defined size and spacing. And supports ensemble simulation of soil organic carbon (SOC) stocks, using area-weighted aggregation

## Fixed

* `gSSURGO.Query()` now always returns all the columns requested, even ones that are all NA. It also now always requires `mukeys` to be specified.
* Updated `gSSURGO.Query()` and `extract_soil_gssurgo()` to work again after formatting changes in the underlying gSSURGO API

## Removed

* `find.land()` has been removed. It is not used anywhere we know if, has apparently not been working for some time, and relied on the `maptools` package which is scheduled for retirement.
* Removed dependency on `PEcAn.data.atmosphere`, notably by retrieving site latitude and longitude directly from `PEcAn.DB::query.site` instead of custom lookups (#3300, Abhinav Pandey).
* Fixed a bugs and BADM now process both single-site and multi-site settings, detecting the input structure and processing each site independently to generate the correct number of ensemble members per site.


# PEcAn.data.land 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see 
https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
