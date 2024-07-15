# PEcAn.ED2 1.8.0.9000


# PEcAn.ED2 1.8.0

* Warning messages for `model2netcdf.ed2()` coming from `ncdf4::ncvar_put()` now are prepended with the variable name for easier debugging (#3078)
* Fixed a bug in `model2netcdf.ed2()` where .nc file connections were being closed multiple times, printing warnings (#3078)
* Fixed a bug affecting the generation of job.sh for runs with many PFTs (#3075)
* Added optional `process_partial` argument to `model2netcdf.ED2()` to allow it to process existing output from failed runs.
* write.config.xml.ED2() wasn't using the <revision> tag in settings correctly (#3080)
* Fixed a bug where `plant_min_temp` trait value wasn't being converted from ºC to K when writing config file for ED2 (#3110)
* Fixed a bug in `read_E_files()` affecting `model2netcdf.ED2()` that resulted in incorrect calculations (#3126)
* DDBH (change in DBH over time) is no longer extracted and summarized from monthly -E- files by `model2netcdf.ED2()`.  We are not sure it makes sense to summarize this variable across cohorts of different sizes.
* The `yr` and `yfiles` arguments of `read_E_files()` are no longer used and the simulation date is extracted from the names of the .h5 files output by ED2.
* Fixed a bug where dimensions of output .nc file would be incorrect if a PFT was missing from ED2 output for less than a full year (#3140, #3143).
* Added optional `process_partial` argument to `model2netcdf.ED2()` to allow it to process existing output from failed 
* Added a `NEWS.md` file to track changes to the package. Prior to this point changes are tracked in the main CHANGELOG for the PEcAn repository.
