# PEcAn.STICS 1.8.2.9000

* Removed `tests/Rcheck_reference.log`, which was used to ignore historic check messages that have now been fixed.


# PEcAn.STICS 1.8.2

* model2netcdf.STICS no longer writes separate `<year>.nc.var` files for every year of output. Use `PEcAn.utils::nc_write_varfiles()` to create these as needed.
* `write.config.STICS()` now modifies parameters with vectors rather than individually, substantially speeding up XML creation (@qdbell, #3395)



# PEcAn.STICS 1.8.1

## License change
* PEcAn.STICS is now distributed under the BSD three-clause license instead of the NCSA Open Source license.


# PEcAn.STICS 1.7.1

First public release, numbered to match the current PEcAn version.