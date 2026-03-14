# PEcAn.SIPNET 1.10.0.9000

* Updated README with a more complete model description and instructions for installing SIPNET (#3705)
* Removed `tests/Rcheck_reference.log`, which was used to ignore historic check messages that have now been fixed.
* Initial support for SIPNET v2.0, whose features include simplified input files,
    simulation of management events, tracking of N cycle components, and anaerobic CH4 generation.
* Breaking: `met2model.SIPNET` now writes 12-column clim files (as expected by Sipnet >= v2.0) by default.
	To get the previously standard 14-column output, set `clim_format_version = "v1"`.
* NITROGEN_CYCLE, LITTER_POOL, and ANAEROBIC enabled by default for SIPNET v2 runs.
* `model2netcdf.SIPNET` now handles both V1 and v2 output header format (v2 has no Notes line; SIPNET #267).
* Removed 13 obsolete v1 parameters from the v2 parameter template `template.param_v2` (microbeInit, qualityLeaf, etc.)
  and added 16 new nitrogen cycle, anaerobic, and methane parameters.
* Added nitrogen cycle (mineral_N, soil_organic_N, litter_N, N2O, N leaching, N fixation,
  N uptake) and methane (CH4) output conversion to NetCDF in `model2netcdf.SIPNET`.
* Fixed crash when `litterWater` column absent in v2 output (LITTER_WATER removed in v2).
* `write.config.SIPNET` now validates runtime flag dependencies (NITROGEN_CYCLE requires
  LITTER_POOL and ANAEROBIC) and guards v1 only parameters (litterWHC, litWaterDrainRate,
  litterWFracInit, microbeInit, m_ballBerry) from being set when using v2 templates.
* Removed workarounds for column naming bugs in output from long-outdated legacy
  Sipnet version `sipnet.unk`.
  

# PEcAn.SIPNET 1.10.0

## Added
* `write.events.SIPNET()` generates SIPNET `events.in` files from an `events.json` file (#3623).
* `met2model.SIPNET` now accepts argument `var.names`, listing which variables should be extracted from the file. If not provided, it extracts all variables in the file (#3563).

## Removed
* The `sipnet2datetime` function is no longer used anywhere and therefore has been removed (#3622).

## Changed
* Breaking: Renamed the setting used to pass soil and hydrology parameters. `write.config.SIPNET` previously read these from `settings$run$inputs$soilinitcond`, now `settings$run$inputs$soil_physics` to better reflect that these are state factors applicable to the whole run rather than initial conditions (Quianyu Li, #3406).
* model2netcdf.SIPNET no longer writes separate `<year>.nc.var` files for every year of output. Use `PEcAn.utils::nc_write_varfiles()` to create these as needed (#3611).
* Restart and met2model functions now print less to the console unless `verbose = TRUE` (#3544, #3563).

## Fixed
* `write.config.SIPNET` now checks more carefully whether an optional variable exists in an initial condition file before trying to read it, therefore printing fewer messages about (expectedly) missing variables (#3545).
* When passed a vector of multiple input paths, `write.config.SIPNET` was choosing one at random; it now throws an error (Blesson Thomas, #3298). Note that a single input path per call has always been the intended usage; being passed many was a second bug in PEcAn.uncertainty that is also now fixed.
* `model2netcdf.SIPNET` no longer assumes a constant value of `pecan_start_doy` across years, which lead to incorrect calculations of `sub_dates` and `sub_dates_cf` at year boundaries (@DongchenZ, #3622).
* When phenology inputs contain missing values, `write.config.SIPNET` now tries to use an average across years for that site before falling back to fixed cross-site defaults (Quianyu Li, #3680).
* `write.config.SIPNET` now adjusts soil water capacity to match the specified soil depth (#3634).
* Fixed unit errors in `write.config.SIPNET` calculation of `leafCSpWt` and `Amax` (#3608, #3664).



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
