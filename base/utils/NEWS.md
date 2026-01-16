# PEcAn.utils 1.8.2

## Added

* New function `nc_write_varfiles()` creates text summaries of the variables in all netCDF files in an output directory, writing either one file named `nc_vars.txt` if `output_mode = "combined"` or a separate `[filename].nc.var` alongside each netCDF if `output_mode = "paired"` (#3611).
* New function `nc_merge_all_sites_by_year()` combines many netCDFs into one file per year (#3620)
* New function `extract_nc_sda`() extracts data from a multi-site netCDF created by `nc_merge_all_sites_by_year` (#3620).
* Added CH4 and N2O variables to `standard_vars`



# PEcAn.utils 1.8.1

## License change
* PEcAn.utils is now distributed under the BSD three-clause license instead of the NCSA Open Source license.

## Changed
* utility function `zero.bounded.density` is now exported.
* Roxygen fixes



# PEcAn.utils 1.8.0

## Added

* Shifted `convert.input` function from `PEcAn.utils` to `PEcAn.DB` with a new name `convert_input`to remove circular dependency.
  (#3026; @nanu1605)
* Added a stub function `convert.input`. (#3026; @nanu1605)
* Updated unit conversions throughout PEcAn to use the `units` R package instead of the unmaintained `udunits2`. Note that both `units` and `udunits2` interface with the same underlying compiled code, so the `udunits2` *system library* is still required. (#2989; @nanu1605)
* Fixed a bug in `ud_convert()` where it failed with objects of class "difftime" introduced by refactoring to use the `units` package instead of `udunits` (#3012)

# PEcAn.utils 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of
  the PEcAn packages; please see
  https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.