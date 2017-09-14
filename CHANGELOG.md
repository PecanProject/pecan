# Change Log
All notable changes are kept in this file. All changes made should be added to the section called
`Unreleased`. Once a new release is made this file will be updated to create a new `Unreleased`
section for the next release.

For more information about this file see also [Keep a Changelog](http://keepachangelog.com/) .

## [Unreleased]

### Fixes
- `PEcAn.BIOCRO` now uses PEcAn-standard variable names. As a result, two output variables have been renamed but keep their exiting units and definitions:
	- `StemBiom` renamed to `AbvGrndWood`
	- `RootBiom` renamed to `root_carbon_content`
- Improved make install logic (#1558)
- Fixed remote code execution #1545
- Added check for NA end/start year in read.output
- Fixed jagify bug for raw field data
- Fixed bug (order of dims in nc_create) introduced in model2netcdf.DALEC by standard_vars changes
- Cleaned up NAMESPACE and source code of `PEcAn.DB` (#1520)
- Debugged python script in call_MODIS in data.remote to allow MODIS downloads
- Fixed FATES build script to work on ubuntu
- SIPNET output netcdf now includes LAI; some variable names changed to match standard
- Cleanup of leap year logic, using new `PEcAn.utils::days_in_year(year)` function (#801).
- Replace many hard-coded unit conversions with `udunits2::ud.convert` for consistency, readability, and clarity
- Refactored extract_soil_nc to create soil2netcdf, which will write soil data out in PEcAn standard.
- Added a new retry.func() to base/utils to provide ability to re-try a function X times before stopping.  Currently using this function in the download.CRUNCEP() function to handle slow responses from THREDDS.

### Added
- Expanded initial conditions workflow for pool-based models, including PEcAn.data.land::prepare_pools to calculate pools from IC file (to be coupled with write.configs)
- New `PEcAn.utils::days_in_year(year)` function that should make it easier to work with leap years.
- New `PEcAn.data.atmosphere::solar_angle` function that replaces math that occurs in some models.

- #1594 shiny/workflowPlot Adding interactiveness using ggploltly
- #1594 shiny/workflowPlot Load outputs from multiple runs of the model
- #1594 shiny/workflowPlot Ways to toggle geometries (e.g. geom_point vs. geom_line).
- #1594 shiny/workflowPlot Smoothing using geom_smooth (Slider for specifying moving window width)
- #1594 shiny/workflowPlot Comparing model output vs loaded data according to [tutorial](https://github.com/PecanProject/pecan/blob/develop/documentation/tutorials/AnalyzeOutput/modelVSdata.Rmd) 

- Allow SIPNET and DALEC met files and model2netcdf to start or end mid year


### Changed
- Clean up directory structure:
    * Move `base` packages (`utils`, `settings`, `db`, `visualizaton`) to a `base` directory, for consistency with `modules` and `models`
    * Move `logger.*` functions out of the `PEcAn.utils` package and into the `pecan.logger` package
- #1594 shiny/workflowPlot Refactoring of code. `get_workflow_ids` in db/R/query.dplyr.R changed with `ensemble = FALSE`. Also allowing to load all workflow IDs. `load_data_single_run` and `var_names_all` also moved from shiny/workflowPlot/server.R to query.dplyr.R

## [1.5.0] - 2017-07-13
### Added
- Added PEcAn.utils::download.file() to allow for use of alternative FTP programs
- Updated downloadAmeriflux and downloadNARR to make use of PEcAn.utils::download.file()
- Added -w flag to load.bety.sh script to specify the URL to fetch the data from
- add new table sites_cultivars to betydb sync scripts (dump and load)
- added docker container scrips (.yml) to create docker container for PEcAn
- added the configuration edit page to allow easy modification of config via web interface
- thredds server documentation and catlog generating script
- added new standard variables table (standard_vars.csv) and to_ncvar and to_ncdim functions in PEcAn.utils
- added initial conditions file io functions for pool-based models in data.land

### Changed
- upscale_met now accepts ~any valid CF file (not just full years), retains correct time units, and respects the previously ignored `overwrite` parameter
- Better date handling in BioCro functions

## [1.4.10.1] - 2017-04-18

### Changed
- Bugfix in Fluxnet2015
- Update Git workflow in Documentation
- download.CRUNCEP now uses CF-compliant time units (days since start of year instead of "secs")
- Bugfixes in met.process


## [1.4.10] - 2017-03-27
Documentation

### Added
- Source Rmarkdown and deploy scripts for PEcAn documentation
- Autocorrelation correction in PDA and scaling factor in emulator

### Changed
- now dumping/loading experiments, cultivars_pfts, current_posteriors, experiments_sites experiments_treatments, trait_covariate_associations [BETY #403](https://github.com/PecanProject/bety/issues/403) [BETY #491](https://github.com/PecanProject/bety/issues/491)

### Removed
- Ameriflux is no longer selectable from the web gui [#1291](https://github.com/PecanProject/pecan/issues/1291)

## [1.4.9] - 2016-12-10
Benchmarking, code cleanup

### Added
- benchmarking code

### Changed
- no more build.sh, using Makefile
- Lots of code cleanup thanks to @bpbond

## [1.4.8] - 2016-08-11
Camp PEON: Assimilation, multi-site, soil params, Maespa, LPJ-GUESS, improvements to web & tutorials

## [1.4.7] - 2016-07-13
CMIP5, Shiny, FLUXNET2015, Global Sensitivity
