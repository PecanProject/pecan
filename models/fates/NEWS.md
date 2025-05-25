# PEcAn.FATES 1.8.0

## License change
* PEcAn.FATES is now distributed under the BSD three-clause license instead of the NCSA Open Source license.

## Changed
* met2model.FATES now produces three monthly files, including precipitation, solar radiation and temperature+humidity
* model2netcdf.FATES now turns monthly PFT level output and grid level output variables of FATES into yearly file in PEcAn format
* Added new arguments to model2netcdf.FATES: site location, start/end dates, vars, and pfts

## Added
* Added a `NEWS.md` file to track changes to the package. Prior to this point changes are tracked in the main CHANGELOG for the PEcAn repository.
