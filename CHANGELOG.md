# Change Log
All notable changes are kept in this file. All changes made should be added to the section called
`Unreleased`. Once a new release is made this file will be updated to create a new `Unreleased`
section for the next release.

For more information about this file see also [Keep a Changelog](http://keepachangelog.com/) .

## [Unreleased]

## [1.5.10] - Prerelease
### Added
- Added PEcAn.utils::download.file() to allow for use of alternative FTP programs
- Updated downloadAmeriflux and downloadNARR to make use of PEcAn.utils::download.file()
- Added -w flag to load.bety.sh script to specify the URL to fetch the data from
- add new table sites_cultivars to betydb sync scripts (dump and load)
- thredds server documentation and catlog generating script

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
