# Unreleased

## Added

* `write.events.SIPNET()` to generate SIPNET `events.in` files from a `events.json` file.

# PEcAn.SIPNET 1.9.1

## Changed

* Breaking: Renamed the setting used to pass soil and hydrology parameters. `write.config.SIPNET` previously read these from `settings$run$inputs$soilinitcond`, now `settings$run$inputs$soil_physics` to better reflect that these are state factors applicable to the whole run rather than initial conditions. (Quianyu Xuan, #3406)
* model2netcdf.SIPNET no longer writes separate `<year>.nc.var` files for every year of output. Use `PEcAn.utils::nc_write_varfiles()` to create these as needed.
* The `sipnet2datetime` function is no longer used anywhere and therefore has been removed.

## Fixed

* `write.config.SIPNET` now checks more carefully whether an optional variable exists in an initial condition file before trying to read it, therefore printing fewer messages about (expectedly) missing variables. (#3545)
* When passed a vector of multiple input paths, `write.config.SIPNET` was choosing one at random; it now throws an error (Blesson Thomas, #3298). Note that a single input path per call has always been the intended usage; being passed many was a second bug in PEcAn.uncertainty that is also now fixed.
* Fixed a bug within the `model2netcdf.SIPNET` function where we assumed the constant calculations of `pecan_start_doy` across years (the calculations should vary depending on the last date from the last loop and the start date of the current loop), which will lead to incorrect calculations of the start `sub_dates` and `sub_dates_cf` if we are jumping between years (e.g., from 2012-12-31 to 2013-01-01).

# PEcAn.SIPNET 1.9.0

## License change
* PEcAn.SIPNET is now distributed under the BSD three-clause license instead of the NCSA Open Source license.

## Fixed

* `met2model.SIPNET()` now stops with an error if the result contains missing values, which [are not allowed](https://github.com/PecanProject/sipnet/issues/38#issuecomment-2701749926) in SIPNET inputs (#3474).
* `write.config.SIPNET()` now consults PFT trait definitions to decide whether to initialize LAI in the leaf-on or leaf-off state, instead of the previous hard-coded defaults (#3419). Specifically:
	- a PFT with `fracLeafFall` > 0.5 will be treated as deciduous (previously hardcoded to deciduous for anything other than boreal conifers)
	- deciduous PFTs will get laiInit=0 if the simulation start date is not between `leafOnDay` and `leafOffDay` (previously hardcoded to May through September)
* The generated Sipnet run script (job.sh) now works correctly, including across machines, when met/input/output files are specified relative to the working directory (#3418). Absolute paths continue to work as always.

# PEcAn.SIPNET 1.8.0

* Support for all Sipnet variables in read_restart and write_restart, for integration with state data assimilation workflows

# PEcAn.SIPNET 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see 
https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
