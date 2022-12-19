# PEcAn.ED2 (development version)

* Warning messages for `model2netcdf.ed2()` coming from `ncdf4::ncvar_put()` now are prepended with the variable name for easier debugging (#3078)
* Fixed a bug in `model2netcdf.ed2()` where .nc file connections were being closed multiple times, printing warnings (#3078)

# PEcAn.ED2 1.7.2.9000

* Added a `NEWS.md` file to track changes to the package. Prior to this point changes are tracked in the main CHANGELOG for the PEcAn repository.
