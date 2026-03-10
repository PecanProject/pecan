# PEcAn.data.land 1.9.0.9000

## Fixed

* `soil2netcdf()` no longer drops depth information for soils with only one layer. (#3785)
* `soil_params()` where bulk density is not specified by the user now look up `soil_bulk_density` from the soil type (as always intended) instead of always reporting 1350 kg/m3 because of a flow control typo. (#3776)

## Added

* Datasets
  *  `landiq_crop_mapping_codes` dataset mapping LandIQ crop classification codes to human-readable crop names.
  *  `bism_kc_by_crop` dataset containing BISm crop coefficient schedules and stage timing references for use in ET estimation, including columns that map to LandIQ class and subclass.

## Changed

* Package `traits`, used by `match_pft()` and `match_species_id()` only when no database connection is provided, is now suggested rather than required.
* Packages `doSNOW`, `dplR`, `httr`, `MCMCpack`, `mvtnorm`, `neonUtilities`,
  `neonstore`, `PEcAn.benchmark`, `PEcAn.visualization`, `rjags`, `sirt`, and
  `sp` are now suggested rather than required. They are only needed for
  specific optional functionality. (#3599)

# PEcAn.data.land 1.9.0

## Added

* New function `soilgrids_ic_process()`, with helpers `preprocess_soilgrids_data()` and `generate_soilgrids_ensemble()`, generates soil carbon initial conditions from SoilGrids 250m data (#3508).
* New function `clip_and_save_raster_file()` subsets rasters to match a polygon of interest (#3537).
* New function `look_up_fertilizer_component()` contains typical carbon and nitrogen composition of common fertilizer types (#3559).
* New PEcAn standard for `events.json` files. These contain information about management events (planting, harvest, irrigation, etc). The standard is defined in `inst/events_schema_v0.1.0.json` and event files can be validated against the schema with new function `validate_events()` (#3623, #3521).

## Changed

* `Read.IC.info.BADM` now processes both single-site and multi-site settings, and uses more carbon pools (`ROOT_BIOMASS`, `AG_BIOMASS`, `SOIL_STOCK`, `LIT_BIOMASS`) if they are present (#3536).
* Package `swfscMisc` is no longer imported; it was formerly used in `extract_NEON_veg()` to compute distances and has been replaced by use of `terra::distance()` (#3552).
* `extract_soil_gssurgo()` now supports spatial grid sampling using new arguments `grid_size` and `grid_spacing`. Previously available argument `radius` has been removed (#3534).
* `extract_soil_gssurgo()` now reports an estimate of soil organic carbon stocks (#3534).

## Removed

* Removed unused parameter `machine` from `put_veg_module()` (#3575).

## Fixed

* Fixed an invalid external pointer error in `soilgrids_soilC_extract()` (#3506).



# PEcAn.data.land 1.8.1

* Dependency `datapack` is now optional. It is only used by `dataone_download()` (#3373).
* `soilgrids_soilC_extract()` no longer returns an empty dataframe when none of the queried locations are missing data. (#3409)
* Functions that name output folders from `settings$siteID` no longer assume IDs are numeric


# PEcAn.data.land 1.8.0

## Added

* New function `soilgrids_soilC_extract` retrieves soil C estimates with uncertainty from the ISRIC SoilGrids 250m data. (#3040, @Qianyuxuan)

## Fixed

* `gSSURGO.Query()` now always returns all the columns requested, even ones that are all NA. It also now always requires `mukeys` to be specified.
* Updated `gSSURGO.Query()` and `extract_soil_gssurgo()` to work again after formatting changes in the underlying gSSURGO API

## Removed

* `find.land()` has been removed. It is not used anywhere we know if, has apparently not been working for some time, and relied on the `maptools` package which is scheduled for retirement.
* Removed dependency on `PEcAn.data.atmosphere`, notably by retrieving site latitude and longitude directly from `PEcAn.DB::query.site` instead of custom lookups (#3300, Abhinav Pandey).



# PEcAn.data.land 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see 
https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
