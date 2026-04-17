# PEcAn.BIOCRO 1.7.5.9000

* Fixed a crash when using BioCro 0.9x by enforcing single-year weather data input (#3787)

# PEcAn.BIOCRO 1.7.5

* model2netcdf.BIOCRO no longer writes separate `<year>.nc.var` files for every year of output. Use `PEcAn.utils::nc_write_varfiles()` to create these as needed.


# PEcAn.BIOCRO 1.7.4

## License change
* PEcAn.BIOCRO is now distributed under the BSD three-clause license instead of the NCSA Open Source license.

## Added
* Added a `NEWS.md` file to track changes to the package. Prior to this point changes are tracked in the main CHANGELOG for the PEcAn repository.
