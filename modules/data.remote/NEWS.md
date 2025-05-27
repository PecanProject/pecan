# PEcAn.data.remote 1.9.0

* Refactored GEDI, LAI, and SMAP workflows for more efficient parallel processing
* Added `GEDI_L4A*` functions to work with footprint-level GEDI biomass data


# PEcAn.data.remote 1.8.0

## Internal changes

* `call_MODIS` now checks QC flags using base R functions, and therefore no longer depends on the `binaryLogic` package.

# PEcAn.data.remote 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see 
https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
