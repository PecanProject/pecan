# Example workflow for multisite RothC simulations

This is a quick example of a set of scripts to run a simple PEcAn ensemble
that simulates soil carbon using the RothC model.

The run scripts scripts were adapted from a set under development for the
[MAGiC project](https://github.com/ccmmf/workflows) with a goal of keeping
this set simple enough to be easy to (1) understand as demos, and
(2) modify for your specific project.

## Important caveats

This package is still under development and many inputs are still hard-coded.
As I write this on 2026-01-09, met and soil data are read from site-specific
files, and all management and initial conditions are hard-coded.
Do not interpret the outputs as meaningful predictions yet.


## Required but not yet provided here

* ERA5 weather data in PEcAn standard netcdf format (instructions TK)
* a RothC binary, compiled from https://github.com/Rothamsted-Models/RothC_Code

## To run

* put weather data in `data_raw/ERA5_CA_nc/` (or update paths to where your
	weather already is)
* Update line 45 of `template.xml` to the path where you installed your copy
	of RothC (or put RothC at `/usr/local/bin/RothC_v2.1.1`)
* update `site_info.csv` with your sites of interest
* TK: Add site-specific management and soil information once implemented
* `./run.sh`

## Troubleshooting

* `fetch_soil_data.R` (called in `run.sh`) uses
	`PEcAn.data.land::extract_soil_gssurgo()`, which sometimes fails for sites
	with only one soil type. A more robust version is under test at
	https://github.com/PecanProject/pecan/pull/3643/files
	If some of your sites fail, try updating PEcAn.data.land from that branch.
