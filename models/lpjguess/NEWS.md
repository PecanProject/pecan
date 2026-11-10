# PEcAn.LPJGUESS 1.9.0.9000

* `write.config.LPJGUESS()` now looks up each PFT's sampled traits in `trait.values` by PFT name instead of by position. A PFT with no sampled traits gets the defaults (with the usual warning) wherever it appears in `settings$pfts`; before, only the last PFT was handled correctly, and one in the middle shifted the traits of every PFT after it (#4225, @aarmens702-hub).


# PEcAn.LPJGUESS 1.9.0

* model2netcdf.LPJGUESS no longer writes separate `<year>.nc.var` files for every year of output. Use `PEcAn.utils::nc_write_varfiles()` to create these as needed.
* Support for model restarts via new functions read.restart.LPJGUESS, read.binary.LPJGUESS, write.restart.LPJGUESS and write.binary.LPJGUESS (#3533, @yinghaoSunn)
* CRU driver tweaks


# PEcAn.LPJGUESS 1.8.0

* PEcAn.LPJGUESS is now distributed under the BSD three-clause license instead of the NCSA Open Source license.
* Support for model restarting and SDA


# PEcAn.LPJGUESS 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see 
https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
