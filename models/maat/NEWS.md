# PEcAn.MAAT 1.7.5

* model2netcdf.MAAT no longer writes separate `<year>.nc.var` files for every year of output. Use `PEcAn.utils::nc_write_varfiles()` to create these as needed.
* Vignette: Turned off evaluation of code chunks that download Ameriflux data.

# PEcAn.MAAT 1.7.4

## License change
* PEcAn.MAAT is now distributed under the BSD three-clause license instead of the NCSA Open Source license.

* Vignette fixes


## Added
* Added a `NEWS.md` file to track changes to the package. Prior to this point changes are tracked in the main CHANGELOG for the PEcAn repository.
