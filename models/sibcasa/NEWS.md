# PEcAn.SIBCASA 0.0.3.9000

* Removed `tests/Rcheck_reference.log`, which was used to ignore historic check messages that have now been fixed.


# PEcAn.SIBCASA 0.0.3

* model2netcdf.SIBCASA no longer writes separate `<year>.nc.var` files for every year of output. Use `PEcAn.utils::nc_write_varfiles()` to create these as needed.



# PEcAn.SIBCASA 0.0.2

## License change
* PEcAn.SIBCASA is now distributed under the BSD three-clause license instead of the NCSA Open Source license.

# PEcAn.LDNDC 0.0.1

First unstable public release. This package is experimental.