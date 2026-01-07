# Example workflow for multisite RothC simulations

This is a quick example of a set of scripts to run a simple PEcAn ensemble
that simulates soil carbon using the RothC model.

The run scripts scripts were adapted from a set under development for the
[MAGiC project](https://github.com/ccmmf/workflows) with a goal of keeping
this set simple enough to be easy to (1) understand as demos, and
(2) modify for your specific project.

## Important caveats

This package is still under development and many inputs are still hard-coded.
As I write this on 2025-12-04, only met data is read from site-specific files;
all soil and management is hard-coded. Do not interpret the outputs as
meaningful predictions yet.


## Required but not yet provided here

* ERA5 weather data in PEcAn standard netcdf format (instructions TK)
* a RothC binary, available from https://github.com/Rothamsted-Models/RothC_Code

## To run

* put weather data in `data_raw/ERA5_CA_nc/` (or update paths to where your weather already is)
* Update line 45 of `template.xml` to the path where you installed your copy of RothC
	(or put RothC at `/usr/local/bin/RothC_v2.1.0`)
* update `site_info.csv` with your sites of interest
* TK: Add site-specific management and soil information once implemented
* `./run.sh`
