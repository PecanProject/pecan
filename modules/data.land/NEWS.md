# PEcAn.data.land 1.7.2.9000

## Fixed

* `gSSURGO.Query()` now always returns all the columns requested, even ones that are all NA. It also now always requires `mukeys` to be specified.
* Updated `gSSURGO.Query()` and `extract_soil_gssurgo()` to work again after formatting changes in the underlying gSSURGO API

## Removed

* `find.land()` has been removed. It is not used anywhere we know if, has apparently not been working for some time, and relied on the `maptools` package which is scheduled for retirement.
* Updated `soil_process` function in `soil_process.R` and `ic_process` to use `PEcAn.DB::query.site` instead of `PEcAn.data.atmosphere::db.site.lat.lon` for retrieving site latitude and longitude.
* Dependency of `PEcAn.data.atmosphere` has been completly removed. This change was introduced via PR [#3300](https://github.com/PecanProject/pecan/pull/3300) by Abhinav Pandey.

# PEcAn.data.land 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see 
https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
