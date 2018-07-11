# Change Log
All notable changes are kept in this file. All changes made should be added to the section called
`Unreleased`. Once a new release is made this file will be updated to create a new `Unreleased`
section for the next release.

For more information about this file see also [Keep a Changelog](http://keepachangelog.com/) .


## [Unreleased]

### Fixes
- Fixed issue #1939 which corrects output time vector for FATES output
- Update to read.output to look for and read only PEcAn formatted .nc output based on the pecan standard filename format of YYYY.nc.  Solves issues with models such as FATES and dvm-dos-tem where the original model output is also in .nc file format and was not ignored by read.output, causing errors with output parsing and plotting with Shiny. Removed deprecated function convert.outputs
- PEcAn.data.atmosphere: 
    - download.Geostreams is pickier about formatting start/end datess, for fewer surprises in result timestamps
    - Fixed swapped lat/lon in met2CF.Geostreams
    - download.GFDL now records reference date in time units field, as required by the CF met standard
    - Reduced download.GFDL network load by not preloading dimension data
    - Fixed spurious `No geonamesUsername set` warning by updating geonames package to development version
- ED:
    - Change all history parameter files to have zero storage respiration
    
- dataone_download.R:
    - Added functionality that spoofs our user address to prevent authentication errors with downloading files via wget.
    
- Data_Ingest_App:
    - use `updateSelectizeInput` to populate `selectizeInput` with choices from BETYdb. This instantly loads the inputfields where other methods take minutes to load. 
    
### Added
- In modules/rtm new function foursail()  to interface with the 4SAIL Fortran code. To enable the use of 4SAIL with any version of PROSPECT (i.e. 4, 5, 5b, D) and custom soil/background reflectance inputs
- shiny/ Dependency explorer 
  - Explore the interdependencies between pecan packages/functions.

- From history you can now select an old run and show the curl command to re-execute this run. This only works with runs submitted through web interface right now.
- Experimental support for docker (#1028)

- dataone_download.R:
  - Added progress messages to indicate that the function is working during longer downloads via PEcAn logger. 
  - Store path to downloaded data as newdir_D1 so that the download app can call this path. 
  
- shiny/Data-Ingest 
  - Download data from DataONE to a temporary directory, display the contents of that directory, and then move files from the temporary directory to dbfiles
  - Upload files from local machines (via drag and drop on some browsers), display files, and then move these files to a directory in dbfiles.
  - Spinner displays when class "shiny-busy" is invoked during the dataONE download process. In this way, users can be sure that the app is live and is processing their request. 
  - Users can now input the name of the destination directory that they wish to create within dbfiles. 
  - Updated Travis.yml to include librdf0-dev so that it can download redland, datapack, and dataone. 
  - Added Data-Ingest UI (inputs, dbfiles, and formats record UI and some basic server side functionality are online)
  - Modularized input record, format record, and dbfiles record into shiny modules. This allows the app to be greatly simplified to two, single-page workflows. These functions can also be used "plug-and-play" style elsewhere in PEcAn shiny apps to load in data. 

  
### Removed
  - pecan.worldmap function no longer used, dropped from visualization package

### Changed
- PEcAn.utils functions run.write.configs and runModule.run.write.configs have been moved to PEcAn.workflow. The versions in PEcAn.utils are deprecated and will be removed in a future release.


## [1.5.3] - 2018-05-15

### Fixes
- read.output now accepts date-times for start.year and end.year argument (#1887)
- read.output no longer assumes timestamps are in days since the beginning of the year
- Fixed xss issue in setup folder, now require users to login before accessing the setup folder scripts.
- Fixed issue where in website not all sites are shown #1884
- Fixed status page, should now be run from cronjob, creates static page
- Fixed bug that overwrote remote  met file paths with local file paths
- PEcAnRTM:
    - Remove non-portable extensions from `src/Makevars`. This should make the package Windows-compatible.
- Fixed BrownDog shiny issues of removing site without geometry; fixing sites.length==0; removing old map markers when change input$type; fixing agreement bug when change input$type

### Added
- Functionality to read pft-specific outputs and to run sensitivity analysis on pft-specific outputs.
- Ability to allow for insecure sync using -k flag
- Added information on how to join slack
- PEcAn.BIOCRO now supports BioCro version 1.0. BioCro 0.9x models should still work as before, but note parameter and weather format changes in the `Changed` section below.  
- Added new model package (PEcAn.dvmdostem) and initial wrappers for integration of the DVM-DOS-TEM model and tested.
- PEcAn now supports PFTs whose members are cultivars rather than species, and will automatically restrict the meta-analysis to matching records, e.g. runs with a PFT containing only Panicum virgatum 'Cave-In-Rock' will not use observations from Panicum virgatum 'Alamo', but a PFT containing the whole species will use observations from both. However, there is not yet any BETYdb interface to *create* cultivar-PFTs other than manual SQL.
- New base package `PEcAn.workflow`, for functions used to perform the each major step of the analysis. These were previously scattered in other base packages.
- Added PR review time estimate to PR template 
- New set of `PEcAn.logger` functions similar to `stopifnot` to facilitate assertive programming: `severeifnot`, `errorifnot`, `warnifnot`, `infoifnot`, `debugifnot`
- PEcAnRTM:
    - Exposed PROSPECT absorption coefficients and `gpm()` function ("generalized plate model"), facilitating experimentation with different absorption coefficients
    - Added `spectra` S3 class and methods for subsetting (e.g. `myspec[[400:700]]`), plotting (`plot()` and `matplot()`), and combining spectra by wavelength.
    - Added `resample` functions for quickly resampling spectra (and, more generally, vectors and functions) to different dimensions. 
    - `EDR` API has been revised. Setup has been refactored from EDR via new `setup_edr` function, which relies on the ED utilities (see `PEcAn.ED2` below), and the `EDR` function now focuses only on execution. Also, added new `params2edr` function to make it easy to convert complex EDR parameters list to flat parameter vector required by `invert_bt` (or other optimization functions).
- PEcAn.ED2:
    - New set of utilities for working with ED meteorology and vegetation inputs, and the ED2IN file. Existing PEcAn code has been revised to use these utilities.
- PEcAn.data.atmosphere:
    - New utilities for efficiently downloading NARR time series using THREDDS/OpenDAP

### Removed
- Removed deprecated copies of PEcAn.utils::SafeList, PEcAn.utils::listToXml (both moved to PEcAn.settings in v 1.5.2), and PEcAn.utils::fqdn (moved to PEcAn.remote in 1.5.2). This fixes the masses of deprecation warnings in otherwise normal run logs (#1719).

### Changed
- Updated wrappers for FATES model to work with recent CLM5 release. Updated write.config, job.template, and other associated files to work with CLM5 inputs and met drivers
- Updated model2netcdf.MAAT to use ncdf4::ncvar_def to define netCDF variables
- Fixed an remote code execution discovered by NCSA security team.
- Column name changes for newly generated biocromet csvs: `SolarR` is now `solar` and `WS` is now `windspeed`. Previously generated met files with the old names will still work for BioCro 0.9 runs, but will need to be renamed before using them with BioCro 1.0.
- write.configs.BIOCRO now requires a model version specification so that it can format parameters to match your version of BioCro. Set it in your Bety model record or in the model$revision field of your pecan.xml.
- When using BioCro 1.0 and no parameter file is given, PEcAn will make an attempt to find default parameters for your genus in the datasets provided by the BioCro package. Note that the default parameter files provided in `models/biocro/inst/extdata/defaults` will *not* work when using BioCro 1.0.
- Added documentation how to submit a run from the command line
- Updated models/maat to provide support for latest model code updates
- PEcAn.DB function `rename.jags.columns` renamed to `rename_jags_columns` to avoid conflict with S3 method naming conventions
- Replaced `rhdf5` library with `hdf5r`, a more modern alternative that is available on CRAN.
- PEcAn.DB function `runModule.get.trait.data` has been moved to the new PEcAn.workflow package to avoid a circular package dependency between PEcAn.DB and PEcAn.settings.
- Major documentation refactoring. The documentation names are now directly tied to the order in which they are rendered, and all `Rmd` files in all subdirectories of the documentation source are rendered by default. The overall structure of the documentation has been revised for clarity and cohesiveness.
- Edited met2model.ED2 to not enforce leap years. 
- Integrate demo 1 into basic user guide

## [1.5.2] - 2017-12-07

### Fixes
- Updated models/ed/data/pftmapping.csv to include two new BETYdb PFTs
- Simple fix to models/ed/R/write.configs.ed.R to properly align pss and css file prefix
- Fixed issue #1752 by updating the site.lst() function to include `site.id=site$id` instead of site.id=site, as site is an object not just the id
- Update to PEcAn.ED2::met2model.ED2 to fix issue with rhdf5::h5write. Bug fix to #1742
- Fixed write.config.xml.ED2 parsing of data/history* files
- `PEcAn.utils` now lazy-loads data for faster execution of functions that consult lookup tables, especially `to_ncvar`.
- Fixed incorrect `PEcAn.BIOCRO` daily and yearly results: Was calculating every row from whole simulation instead of that day (#1738)

### Added
- New Dockerfile to create PEcAn specific container for SIPNET.

### Removed
- Removed `PEcAn.utils::model2netcdf`, which has been deprecated since PEcAn 1.3.7. Use `model2netcdf.<YOURMODEL>` in the appropriate model package instead.

### Changed
- Major namespace cleanup in the `PEcAn.utils` package. It now loads more quietly and is much less likely to mask functions in a package you loaded earlier.
- Moved many functions from `PEcAn.utils` into other PEcAn packages. The `PEcAn.utils` versions still work with a deprecation warning, but will be removed in next release.
	- `listToXml` and `SafeList` moved to `PEcAn.settings`
	- `fqdn` moved to `PEcAn.remote`
- PEcAnRTM: Removed effective sample size normalization from likelihood calculation. It was giving weird results.

## [1.5.1] - 2017-10-05

### Fixes
- Fixed hyperparameter draws in PDA
- Show workflowid in the URL when run is finshed and user clicks results (#1659)
- `PEcAn.BIOCRO` now uses PEcAn-standard variable names. As a result, two output variables have been renamed but keep their existing units and definitions:
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
 SIPNET output netcdf now includes LAI; some variable names changed to match standard
- Cleanup of leap year logic, using new `PEcAn.utils::days_in_year(year)` function (#801).
- Replace many hard-coded unit conversions with `udunits2::ud.convert` for consistency, readability, and clarity
- Refactored extract_soil_nc to create soil2netcdf, which will write soil data out in PEcAn standard.
- Added a new retry.func() to base/utils to provide ability to re-try a function X times before stopping.  Currently using this function in the download.CRUNCEP() function to handle slow responses from THREDDS.
- Reformatted call_MODIS netcdf output to reflect the orientation of the MODIS grid
- Remote execution is more robust to errors in the submission process, not just the actual model execution
- PRELES model run script bug fix 

### Added
- Functionality to pass hyperparameters via settings
- Created new (and very rudimentary) web interface for downloading data from the dataone federation into the PEcAn database. More updates to come.
- Expanded initial conditions workflow for pool-based models, including PEcAn.data.land::prepare_pools to calculate pools from IC file (to be coupled with write.configs)
- New `PEcAn.utils::days_in_year(year)` function that should make it easier to work with leap years.
- New `PEcAn.data.atmosphere::solar_angle` function that replaces math that occurs in some models.
- New `PEcAn.benchmarking::align_pft` fucntion that aligns data assosiated with two different plant functional types
- #1594 shiny/workflowPlot Adding interactiveness using ggploltly
- #1594 shiny/workflowPlot Load outputs from multiple runs of the model
- #1594 shiny/workflowPlot Ways to toggle geometries (e.g. geom_point vs. geom_line).
- #1594 shiny/workflowPlot Smoothing using geom_smooth (Slider for specifying moving window width)
- #1594 shiny/workflowPlot Comparing model output vs loaded data according to [tutorial](https://github.com/PecanProject/pecan/blob/develop/documentation/tutorials/AnalyzeOutput/modelVSdata.Rmd) 
- Allow SIPNET and DALEC met files and model2netcdf to start or end mid year
- A Pre-release database clean up script that deletes unused/unassosiated entries from the database

### Changed
- Clean up directory structure:
    * Move `base` packages (`utils`, `settings`, `db`, `visualizaton`) to a `base` directory, for consistency with `modules` and `models`
    * Move `logger.*` functions out of the `PEcAn.utils` package and into the `PEcAn.logger` package
    * Move `remote` functions out of the `PEcAn.utils` package and into their own `PEcAn.remote` package.
- #1594 shiny/workflowPlot Refactoring of code. `get_workflow_ids` in db/R/query.dplyr.R changed with `ensemble = FALSE`. Also allowing to load all workflow IDs. `load_data_single_run` and `var_names_all` also moved from shiny/workflowPlot/server.R to query.dplyr.R
- `PEcAn.remote::start.model.runs` has been significantly refactored to be less redundant and more robust
- `betyConnect` function in `query.dplyr.R` is now refactored into `read_web_config` so that the the Data-Ingest app can leverage `read_web_config` and provide it with a machine specific filepath for `.../dbfiles`
- Rpreles and Maeswrap package moved to suggest checked for within package function.


## [1.5.0] - 2017-07-13
### Added
- Added cron job and script for the sync of the database.
- Added PEcAn.utils::download.file() to allow for use of alternative FTP programs
- Updated downloadAmeriflux and downloadNARR to make use of PEcAn.utils::download.file()
- Added -w flag to load.bety.sh script to specify the URL to fetch the data from
- Add new table sites_cultivars to betydb sync scripts (dump and load)
- Added docker container scrips (.yml) to create docker container for PEcAn
- Added the configuration edit page to allow easy modification of config via web interface
- Thredds server documentation and catlog generating script
- Added new standard variables table (standard_vars.csv) and to_ncvar and to_ncdim functions in PEcAn.utils
- Added initial conditions file io functions for pool-based models in data.land

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
