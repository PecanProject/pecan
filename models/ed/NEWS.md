# PEcAn.ED2 (development version)

* Warning messages for `model2netcdf.ed2()` coming from `ncdf4::ncvar_put()` now are prepended with the variable name for easier debugging (#3078)
* Fixed a bug in `model2netcdf.ed2()` where .nc file connections were being closed multiple times, printing warnings (#3078)
* Fixed a bug affecting the generation of job.sh for runs with many PFTs (#3075)
* Added optional `process_partial` argument to `model2netcdf.ED2()` to allow it to process existing output from failed runs.
* write.config.xml.ED2() wasn't using the <revision> tag in settings correctly (#3080)
* Fixed a bug where `plant_min_temp` trait value wasn't being converted from ºC to K when writing config file for ED2 (#3110)

# PEcAn.ED2 1.7.2.9000

* Added a `NEWS.md` file to track changes to the package. Prior to this point changes are tracked in the main CHANGELOG for the PEcAn repository.
* Added optional `process_partial` argument to `model2netcdf.ED2()` to allow it to process existing output from failed 