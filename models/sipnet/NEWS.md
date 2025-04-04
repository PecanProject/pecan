# PEcAn.SIPNET 1.8.0.9000

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
