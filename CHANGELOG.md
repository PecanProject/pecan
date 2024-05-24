# Change Log
All notable changes are kept in this file. All changes made should be added to the section called
`Unreleased`. Once a new release is made this file will be updated to create a new `Unreleased`
section for the next release.

For more information about this file see also [Keep a Changelog](http://keepachangelog.com/) .

## [Unreleased]

Some changes have been made to the docker-compose file. Check your .env file to
see if you need to change any of these:
- TRAEFIK_HOST is now TRAEFIK_HOSTNAME without the Host: and ;, for example if you have
  `TRAEFIK_HOST=Host:pecan.example.com;` you will need to change this to
  `TRAEFIK_HOST=pecan.example.com`.
- TRAEFIK_IPFILTER is no longer used, and should be removed.
- TRAEFIK_HTTP_PORT now defaults to port 80
- TRAEFIK_HTTPS_PORT now defaults to port 443
- TRAEFIK_ACME_ENABLE is no longer used. If you want to use https you will need to add
  `docker-compose.https.yml` to your command line. You can use the script `compose.sh`
  to star PEcAn with https.
- TRAEFIK_HTTPS_OPTIONS is no longer used, this is the default when you use https.
- TRAEFIK_HTTP_REDIRECT is no longer used, this is the default when you use https.

### Added
- Created a new soilgrids function to extract the mean soil organic carbon profile with associated undertainty values at each depth for any lat/long location (#3040). Function was created for the CMS SDA workflow

- `PEcAn.all` gains new function `pecan_version`, which reports for each PEcAn
  package the version that was provided with this release and the version that
  is currently installed. Use it in scripts to record your system state for
  reproducibility, or for debugging.
- Added a new function `unit_is_parseable` in PEcAn.utils to replace `udunits2::ud.is.parseable`.
  (#3002; @nanu1605)
- Initial LDNDC model coupling
- `PEcAn.settings::read.settings()` now strips comments so HTML style comments (e.g. `<!-- a comment -->`) are now allowed in pecan.xml files
- `PEcAn.logger::setLevel()` now invisibly returns the previously set logger level
-  Warning messages for `model2netcdf.ed2()` coming from `ncdf4::ncvar_put()` now are prepended with the variable name for easier debugging (#3078)
- Added optional `process_partial` argument to `model2netcdf.ED2()` to allow it to process existing output from failed runs.
- Added litter_mass_content_of_water to standard_vars table
- Added litter_mass_content_of_water to model2netcdf.SIPNET
- Added all SIPNET state variables to read_restart and write_restart
- Added Observation preparation functions into the SDA workflow, which supports AGB, LAI, Soil Carbon, and Soil moisture.
- We are slowly change the license from NCSA opensource to BSD-3 to help with publishing PEcAn to CRAN.
- Added an optional `pfts` argument to `PEcAn.uncertainty::run.sensitivity.analysis()` so that sensitivity analysis and variance decomposition can be run on a subset of PFTs defined in `settings` if desired (#3155).
- Added new features of the SDA function including: 1) allow user-defined free-run mode; 
2) allow user-defined parallel mode for the qsub submission; 3) allow user-defined email option to report the progress.
- The analysis function now supports the parallelization of multi-chain MCMC sampling with the fully randomized inits function.
- Added the new feature of the block-based SDA workflow, which supports the parallel computation.
- Added new SDA workflow for the 342 North America anchor sites.
- Added new feature of preparing initial conditions for MODIS LAI, AGB, ISCN SOC, and soil moisture across NA anchor sites.

### Fixed

- Replaced the broken ameriflux download pipeline with the new amerifluxr package's function.
- Fix broken build caused by BioCro updates #2925
- rstudio was not working behind traefik.
- plots now work in docker containers
- when specifying diferent rstudio user, dev setup would mount pecan folder in wrong path.
- fixed the pagination error in the next_page field of the workflows, inputs and runs Rest API.
- bugs in `model2ncdf.ED2()` that were causing it to both error and also only
convert data for a single PFT fixed (#1329, #2974, #2981)
- Code cleanup in PEcAn.MA to protect against global namespace pollution (#2965, #2973; @nanu1605) 
- Fixed vignettes and cleaned up internal warnings in PEcAn.DB (#2966, #2971; @nanu1605).
- Updated unit conversions throughout PEcAn to use the `units` R package
  instead of the unmaintained `udunits2`.
  Note that both `units` and `udunits2` interface with the same underlying
  compiled code, so the `udunits2` *system library* is still required.
  (#2989; @nanu1605)
- Occasionally some run directories were not getting copied over to remote hosts.  This should be fixed now (#3025)
- Fixed a bug with ED2 where ED2IN tags supplied in `settings` that were not in the ED2IN template file were not getting added to ED2IN config files (#3034, #3033)
- Fixed a bug where warnings were printed for file paths on remote servers even when they did exist (#3020)
- Fixed bug in model2netcdf.SIPNET that caused LE to be overestimaed 10^3 (#3036)
- Added an updated ED2IN template file, `models/ed/inst/ED2IN.r2.2.0.github`, to reflect new variables in the development version of ED2
- `PEcAn.data.land::gSSURGO.Query` has been updated to work again after changes to the gSSURGO API.
- `PEcAn.settings::read.settings()` now prints a warning when falling back on default `"pecan.xml"` if the named `inputfile` doesn't exist.
- fqdn() can access hostname on Windows (#3044 fixed by #3058)
- The model2netcdf_SIPNET function can now export full year nc files by using 
  the cdo_setup argument in the template job file. In detail, people will need
  to specify cdosetup = "module load cdo/2.0.6" in the host section. More details
  are in the Create_Multi_settings.R script. (#3052)
- write.config.xml.ED2() wasn't using the <revision> tag in settings correctly (#3080)
- runModule.get.trait.data() now correctly respects the settings$database$bety$write setting (#2968)
- Fixed a bug in `model2netcdf.ed2()` where .nc file connections were being closed multiple times, printing warnings (#3078)
- Fixed a bug causing the model2netcdf.ED2() step in jobs.sh to be incorrectly written (#3075)
- Fixed a bug where `plant_min_temp` trait value wasn't being converted from ºC to K when writing config file for ED2 (#3110)
- Fixed a bug in `PEcAn.ED2::read_E_files()` affecting `PEcAn.ED2::model2netcdf.ED2()` that resulted in incorrect calculations (#3126)
- DDBH (change in DBH over time) is no longer extracted and summarized from monthly -E- files by `PEcAn.ED2::model2netcdf.ED2()`.  We are not sure it makes sense to summarize this variable across cohorts of different sizes.
- The `yr` and `yfiles` arguments of `PEcAn.ED2::read_E_files()` are no longer used and the simulation date is extracted from the names of the .h5 files output by ED2.
- Update Dockerfile for sipnet/maespa/template to use pecan/models:tag to build.
- Fixed a bug in `PEcAn.utils::ud_convert()` where it failed with objects of class "difftime" introduced by refactoring to use the `units` package instead of `udunits` (#3012)
- The propagation of aqq and bqq for the SINGLE Q type has been corrected.
- The issue where the indirect constraints will be increase with the increase of the covariance sizes.

### Changed

- Using R4.0 and R4.1 tags to build PEcAn. Default is now 4.1
- Database connections consistently use `DBI::dbConnect` instead of the deprecated `dplyr::src_postgres` (#2881). This change should be invisible to most users, but it involved converting a lot of internal variables from `bety$con` to `con`. If you see errors involving these symbols it means we missed a place, so please report them as bugs.
- `PEcAn.utils::download.url` argument `retry404` is now renamed to `retry` and
  now functions as intended (it was being ignored completely before).
- Update URL for MERRA downloads (#2888)
- PEcAn.logger is now BSD-3 License
- Skipped ICOS and MERRA download tests when running in github actions
- Converted .zenodo.json to CITATION.cff
- Using traefik 2.5 instead of 1.7
- api is now open by default (was auth required in the past)
- Installation instructions updated in documentation
- PEcAn.assim.sequential is renamed to PEcAnAssimSequential
- `convert.input` is moved from PEcAn.utils to PEcAn.DB and renamed as `convert_input`.
  This was needed to resolve a cyclic dependency between PEcAn.DB and PEcAn.utils.
  (#3026; @nanu1605)
- Internal changes to keep up to date with tidyselect v1.2.0
- The `PEcAn.utils::download.file()` function has now been renamed to `PEcAn.utils::download_file()`
- The `regrid()` and `grid2netcdf()` function from `PEcAn.utils` have been moved to `PEcAn.data.remote` package.
- Web is now build using PHP 8 since PHP 7 is EOL.

### Removed

- the check image (used in kubernetes) is removed, please use ncsa/checks instead.
- Unused (and apparently long-broken) function `PEcAn.data.land::find.land` has been removed.
- No longer building r136 sipnet docker image.

## [1.7.2] - 2021-10-04

### Due to dependencies, PEcAn is now using R 4.0.3 for Docker images.

This is a major change:

- Newer version of R
- Ubuntu 20.04 instead of Debian.

### Fixed

- Removed sender.py and now allow the submission of workflows from inside the rstudio container.
- Use TRAEFIK_FRONTEND_RULE in compose file and TRAEFIK_HOST in env.example, using TRAEFIK_HOST everywhere now. Make sure TRAEFIK_HOST is used in .env
- Use initial biomass pools for Sorghum and Setaria #2495, #2496
- PEcAn.DB::betyConnect() is now smarter, and will try to use either config.php or environment variables to create a connection. It has switched to use db.open helper function (#2632).
- PEcAn.utils::tranformstats() assumed the statistic names column of its input was a factor. It now accepts character too, and returns the same class given as input (#2545).
- fixed and added tests for `get.rh` function in PEcAn.data.atmosphere
- Invalid .zenodo.json that broke automatic archiving on Zenodo ([b56ef53](https://github.com/PecanProject/pecan/commit/b56ef53888d73904c893b9e8c8cfaeedd7b1edbe))
- Fixed a filehandle leak in multi-year runs of PEcAn.BIOCRO::met2model.BIOCRO: It was only closing the last input file it processed (#2485).
- Fix issue with cruncep download: use netcdf subset (ncss) method instead of opendap (#2424).
- The `parse` option to `PEcAn.utils::read_web_config` had no effect when `expand` was TRUE (#2421).
- Fixed a typo that made `PEcAn.DB::symmetric_setdiff` falsely report no differences (#2428).
- sipnet2netcdf will now only extract the data for the year requested (#2187)
- Fixed Priors vignette (#2439).
- When building sipnet model would not set correct model version
- Update pecan/depends docker image to have latest Roxygen and devtools.
- Update ED docker build, will now build version 2.2.0 and git
- Do not override meta-analysis settings random-effects = FALSE https://github.com/PecanProject/pecan/pull/2625
- model2netcdf.ED2 no longer detecting which varibles names `-T-` files have based on ED2 version (#2623)
- Changed docker-compose.yml to use user & group IDs of the operating system user (#2572)  
- gSSURGO file download now added as inputs into BETY through extract_soil_gssurgo (#2666)
- ensure Tleaf converted to K for temperature corrections in PEcAn.photosynthesis::fitA (#2726)
- fix bug in summarize.result to output stat, which is needed to turn on RE in the meta-analysis (#2753)
- ensure that control treatments always receives the random effect index of 1; rename madata.Rdata to jagged.data.Rdata and include database ids and names useful for calculating parameter estimates by treatment (#2756)
- ensure that existing meta-analysis results can be used for pfts with cultivars (#2761)
- Major code cleanup by GSoC student @moki1202, fixing many check warnings across 10 packages
  (#2771, #2773, #2774, #2775, #2805, #2815, #2826, #2830, #2857)

### Changed

- Removed deprecated mstmip_vars and mstmip_local; now all functions use the combined standard_vars.csv  
- RabbitMQ is set to be 3.8 since the 3.9 version can no longer be configured with environment variables.
- Removed old api, now split into rpecanapi and apps/api.
- Replaced `tmvtnorm` package with `TruncatedNormal` package for speed up per #2621.
- Continuous integration changes: Added experimental GitHub Actions CI builds (#2544), streamlined Travis CI builds, added a fourth R version (second-newest old release; currently R 3.5) to Travis test matrix (#2592).
- Functions that update database entries no longer pass `created_at` or `updated_at` timestamps. The database now updates these itself and ensures they are consistently in UTC (#1083).
- `PEcAn.DB::insert_table` now uses `DBI::dbAppendTable` internally instead of manually constructed SQL (#2552).
- Rebuilt documentation using Roxygen 7. Readers get nicer formatting of usage sections, writers get more flexible behavior when inheriting parameters and less hassle when maintaining namespaces (#2524).
- Renamed functions that looked like S3 methods but were not:
    * PEcAn.priors: `plot.posterior.density`->`plot_posterior.density`, `plot.prior.density`->`plot_prior.density`, `plot.trait`->`plot_trait` (#2439).
    * PEcAn.visualization: `plot.netcdf`->`plot_netcdf` (#2526).
    * PEcAn.assim.sequential: `Remote.Sync.launcher` -> `Remote_Sync_launcher` (#2652)
- Stricter package checking: `make check` and CI builds will now fail if `R CMD check` returns any ERRORs or any "newly-added" WARNINGs or NOTEs. "Newly-added" is determined by strict string comparison against a check result saved 2019-09-03; messages that exist in the reference result do not break the build but will be fixed as time allows in future refactorings (#2404).
- No longer writing an arbitrary num for each PFT, this was breaking ED runs potentially.
- The pecan/data container has no longer hardcoded path for postgres
- PEcAn.JULES: Removed dependency on `ncdf4.helpers` package, which has been removed from CRAN (#2511).
- data.remote: Arguments to the function `call_MODIS()` have been changed (issue #2519).
- Changed precipitaion downscale in `PEcAn.data.atmosphere::download.NOAA_GEFS_downscale`. Precipitation was being downscaled via a spline which was causing fake rain events. Instead the 6 hr precipitation flux values from GEFS are preserved with 0's filling in the hours between.
- Changed `dbfile.input.insert` to work with inputs (i.e soils) that don't have start and end dates associated with them
- Default behavior for `stop_on_error` is now `TRUE` for non-ensemble runs; i.e., workflows that run only one model simulation (or omit the `ensemble` XML group altogether) will fail if the model run fails. For ensemble runs, the old behavior is preserved; i.e., workflows will continue even if one of the model runs failed. This behavior can also be manually controlled by setting the new `run -> stop_on_error` XML tag to `TRUE` or `FALSE`.
- Several functions have been moved out of `PEcAn.utils` into other packages (#2830, #2857):
  * `ensemble.filename`, `get.results`, `runModule.get.results`, `read.sa.output`, `sensitivity.filename`,
    and `write.sa.configs` have been moved to `PEcAn.uncertainty`.
  * `create.base.plot`, `dhist`, `plot_data` and `theme_border` have been moved to `PEcAn.visualizaton`.

### Added

- BioCro can export Aboveground Biomass (#2790)
- Functionality for generating the same ensemble parameter sets with randtoolbox functions.
- Functionality for joint sampling from the posteriors using randtoolbox functions.
- BASGRA-SDA couplers.
- Now creates docker images during a PR, when merged it will push them to docker hub and github packages
- New functionality to the PEcAn API to GET information about PFTs, formats & sites, submit workflows in XML or JSON formats & download relevant inputs/outputs/files related to runs & workflows (#2674 #2665 #2662 #2655)
- Functions to send/receive messages to/from rabbitmq.
- Documentation in [DEV-INTRO.md](DEV-INTRO.md) on development in a docker environment (#2553)
- PEcAn API that can be used to talk to PEcAn servers. Endpoints to GET the details about the server that user is talking to, PEcAn models, workflows & runs. Authetication enabled. (#2631)
- New versioned ED2IN template: ED2IN.2.2.0 (#2143) (replaces ED2IN.git)
- model_info.json and Dockerfile to template (#2567)
- Dockerize BASGRA_N model.
- Basic coupling for models BASGRA_N and STICS.
- PEcAn.priors now exports functions `priorfig` and `plot_densities` (#2439).
- Models monitoring container for Docker now shows a webpage with models it has seen
- Added small container to check if certain services are up, used as initi container for kubernetes
- Documentation how to run ED using singularity
- PEcAn.DB gains new function `get_postgres_envvars`, which tries to look up connection parameters from Postgres environment variables (if they are set) and return them as a list ready to be passed to `db.open`. It should be especially useful when writing tests that need to run on systems with many different database configurations (#2541).
- New shiny application to show database synchronization status (shiny/dbsync)
- Ability to run with [MERRA-2 meteorology](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/) (reanalysis product based on GEOS-5 model)
- Ability to run with ICOS Ecosystem products

### Removed

- Removed travis integration
- Removed the sugarcane and db folders from web, this removes the simple DB editor in the web folder. (#2532)
- Removed ED2IN.git (#2599) 'definitely going to break things for people' - but they can still use PEcAn <=1.7.1
- Database maintenance scripts `vacuum.bety.sh` and `reindex.bety.sh` have been moved to the [BeTY database repository](https://github.com/PecanProject/bety) (#2563).
- Scripts `dump.pgsql.sh` and `dump.mysql.sh` have been deleted. See the ["BeTY database administration"](https://pecanproject.github.io/pecan-documentation/develop/database.html) chapter of the PEcAn documentation for current recommendations (#2563).
- Old dependency management scripts `check.dependencies.sh`, `update.dependencies.sh`, and `install_deps.R` have been deleted. Use `generate_dependencies.R` and the automatic dependency handling built into `make install` instead (#2563).
- Deprecated copies of functions previously moved to other packages have been removed from `PEcAn.utils` (#2830):
  * `do_conversions` and `runModule.run.write.configs`, `run.write.configs`. These are now in `PEcAn.workflow`
  * `get.ensemble.samples`, `read.ensemble.output`, `write.ensemble.configs`. These are now in `PEcAn.uncertainty`
  * `logger.debug`, `logger.error`, `logger.getLevel`, `logger.info`, `logger.setLevel`,
    `logger.setOutputFile`, `logger.setQuitOnSevere`, `logger.setWidth`, `logger.severe`, `logger.warn`.
    These are now in `PEcAn.logger`

## [1.7.1] - 2018-09-12

### Fixed
- Replace deprecated `rlang::UQ` syntax with the recommended `!!`
- Explicitly use `PEcAn.uncertainty::read.ensemble.output` in `PEcAn.utils::get.results`. Otherwise, it would sometimes use the deprecated `PEcAn.utils::read.ensemble.output` version.
- `PEcAn.ED2::met2model.ED2` now skips processing of years for which all output files are already present (unless `overwrite = TRUE`). This prevents a lot of unnecessary work when extending an existing ED met record.
- Fixed issue that prevented modellauncher from working properly #2262
- Use explicit namespacing (`package::function`) throughout `PEcAn.meta.analysis`. Otherwise, many of these functions would fail when trying to run a meta-analysis outside of the PEcAn workflow (i.e. without having loaded the packages first) (#2351).
- Standardize how `PEcAn.DB` tests create database connections, and make sure tests work with both the newer `Postgres` and older `PostgreSQL` drivers (#2351).
- Meta-analysis = "AUTO" now correctly skips the meta analysis if the PFT definition has not changed (#1217).
- Replace deprecated `rlang::UQ` syntax with the recommended `!!`
- Explicitly use `PEcAn.uncertainty::read.ensemble.output` in `PEcAn.utils::get.results`. Otherwise, it would sometimes use the deprecated `PEcAn.utils::read.ensemble.output` version.
- History page would not pass the hostname parameter when showing a running workflow, this would result in the running page showing an error.

### Changed
- Updated modules/rtm PROSPECT docs
- Updated models/sipnet/R/model2netcdf.SIPNET.R to address issues in PR #2254
- Improved testing (#2281). Automatic Travis CI builds of PEcAn on are now run using three versions of R in parallel. This should mean fewer issues with new releases and better backwards compatibility, but note that we still only guarantee full compatibility with the current release version of R. The tested versions are:
  - `release`, the current public release of R (currently R 3.5). Build failures in this version are fixed before merging the change that caused them. When we say PEcAn is fully tested and working, this is the build we mean.
  - `devel`, the newest available development build of R. We will fix issues with this version before the next major R release.
  - `oldrel`, the previous major release of R (currently R 3.4). We will fix issues with this version as time allows, but we do not guarantee that it will stay compatible.
- Reverting back from PR (#2137) to fix issues with MAAT wrappers.
- Moved docker files for models into model specific folder, for example Dockerfile for sipnet now is in models/sipnet/Dockerfile.
- `PEcAn.utils`:
  - Remove, or make "Suggests", a bunch of relatively underutilized R package dependencies.
- Add template for documentation issues and add button to edit book.
- Conditionally skip unit tests for downloading raw met data or querying the database when the required remote connection is not available.
- Reorganization of docker folder
  - All dockerfiles now live in their own folder
  - `scripts/generate_dependencies.R` is now used to generate dependencies for make and docker
- In `PEcAn.DB::get.trait.data`, if `trait.names` is `NULL` or missing, use the traits for which at least one prior is available among the input list of PFTs. (Previously, we were getting this from the `PEcAn.utils::trait.dictionary`, which we are trying to deprecate #1747). (#2351)
- Cleanup and improve logging and code readability in parts of `PEcAn.DB` related to getting trait data, including replacing many manual database queries with `dplyr` calls.
- Reorganization of PEcAn documentation in accordance with isue #2253.
- SIPNET now is installed from the source code managed in git

### Added
- Meta analysis functionality to not use greenhouse data.
- Dockerize the BioCro model.
- Added PRO4SAIL-D model, using existing 4SAIL src and coupling with PROSPECT-D Fortran code
- Models will not advertise themselvs, so no need to register them a-priori with the database #2158
- Added simple Docker container to show all containers that are available (http://localhost:8000/monitor/). This will also take care of registering the models with the BETY database.
- Added unit tests for `met2model.<MODEL>` functions for most models.
- Added MAESPA model to docker build
- PEcAn has more robust support for `RPostgres::Postgres` backend. The backend is officially supported by `db.query`, and basic workflows run top-to-bottom with the `Postgres` backend. However, `RPostgreSQL` is still the default until we do more robust testing of all modules.
- `PEcAn.DB::db.query` now optionally supports prepared statements (#395).
- New function `PEcAn.DB::query_priors` that expands the functionality of `query.priors` by (1) accepting PFTs by name or ID and (2) allowing the user to request all possible combinations of the input PFTs and traits (i.e. `expand.grid(pfts, traits)`) or just the pairwise combinations (i.e. `pft[1]-trait[1], pft[2]-trait[2]`). This function also comes with more robust error handling and a set of unit tests (#2351).
- New function `PEcAn.DB::query_pfts` for finding PFT IDs and types from the PFT name and (optionally) model type (#2351).
- Run Travis integration tests with both Postgres and PostgreSQL drivers (#2351).
- New function `PEcAn.utils::load_local` reads `Rdata` files into a named list (instead of into the current environment).

### Removed
- Removed unused function `PEcAn.visualization::points2county`, thus removing many indirect dependencies by no longer importing the `earth` package.
- Removed package `PEcAn.data.mining` from the Make build. It can still be installed directly from R if desired, but is skipped by default because it is in early development, does not yet export any functions, and creates a dependency on the (large, often annoying to install) ImageMagick library.
- Fully deprecate support for `MySQL` database driver. Now, only `PostgreSQL` (and, experimentally, `RPostgres`) are supported. With this, remove `RMySQL` dependency in several places.

## [1.7.0] - 2018-12-09

### Fixes
- Fixed minor bug in query.trait.data related to stem respiration covariates (https://github.com/PecanProject/pecan/issues/2269)
- Removed google maps and replaced with leaflet #2105
- Added ability to add a new site from web interface
- Small updated to models/ed/R/model2netcdf.ED2.R to fix issue realted to writing the time_bounds time attribute. Needed to add a check for which file types exitst (e.g. -E-, -T-, etc) and only write the appropriate attribute(s).
- Fixed error in `read_web_config` which would filter out all variables.
- Docker:
  - Make sure web interface posts RabbitMQ messages even after editing files (fixes #2151)
  - Can specify name of docker cluster using PECAN_FQDN and PECAN_NAME (fixes #2128)
  - Fixed issue where setting username/password for rabbitmq would break web submit (fixes #2185)
  - data image only registers sipnet and ed, has all data pre-downloaded
- ED2:
  - Fix processing of `ed2in_tags` from XML. Now numbers (e.g. `<TRAIT_PLASTICITY_SCHEME>0</TRAIT_PLASTICITY_SCHEME>`) and numeric vectors (e.g. `<INCLUDE_THESE_PFT>9,10,11,12</INCLUDE_THESE_PFT>`) are correctly written to ED2IN _without_ quotes.

### Added
- NEW FEATURE: PEcAn R API (PR #2192). Features include:
    - Modified `docker/receiver.py` to accept a `pecan_json` object containing a JSON version of the PEcAn settings. Can now Use RabbitMQ HTTP API (called from R with `httr`) to send a settings list (function `submit_workflow`)
    - Helper functions to make it easier to build the settings object, and to register a new workflow.
    - Helper functions for tracking workflow status
    - Helper functions for accessing workflow output files through THREDDS. All files are accessible through `fileServer` (basically, direct download), and NetCDF files are also readable through OpenDAP.
        - THREDDS catalog filter has been removed, so that _all_ workflow outputs are available to THREDDS.
        - Added another `datasetScan` to the THREDDS catalog to search for `dbfiles`. Now, model inputs (e.g. meteorology files) are accessible via THREDDS as well.
- Lots of new documentation for running PEcAn using Docker
- Added Docker container with documentation #2160
- Download method (`method`) argument for `data.atmosphere::download.CRUNCEP`, which defaults to `opendap` (as it was), but can be switched to the slower but more robust NetCDF subset (`ncss`).
- In `download.CRUNCEP`, check target coordinate against the land-sea mask. If sea, pick the nearest land pixel within 1 degree of target. This facilitates doing runs at coastal sites that may get masked out.
- Added a prototype of the THREDDS data server (TDS) to the PEcAn Docker stack.
- Added portainer to the PEcAn Docker stack to easily look at running containers.
- Added ability to specify short name for a host (hostlist->displayname)
- Added `PEcAn.logger::print2string` function -- capture the output
- Cleanup and enhancements to `PEcAn.utils::read.output`:
  - Pass `variables = NULL` to try to read _all_ variables from file
  - New argument `ncfiles` for passing file names explicitly (useful for remote file access where `list.files` doesn't work; e.g. THREDDS)
  - Variable summary stats are only calculated if new argument `print_summary` is `TRUE` (default). Summary is rendered nicely as a variable x statistic matrix.
  - New argument `verbose` (default = `FALSE`) to print out (`logger.debug`) at every variable and year
  - Minor code cleanup for style (spacing, long lines, etc.) and logic (replace several `else` statements with early returns)
- ED2:
  - Add ability to pass arbitrary arguments to the ED binary through the `pecan.xml` (#2183; fixes #2146).
  - Add new `model` tag `<all_pfts>`. If "false" (default), set ED2IN's `INCLUDE_THESE_PFT` to only PFTs explicitly configured through PEcAn. If "true", use all 17 of ED2's PFTs.
  - Add new `model` tag `<barebones_ed2in>`. If "true", only write ED2IN tags, and do not include comment annotations. If "false" (default), try to transfer comments from ED2IN template to target ED2IN. Because of comments are written by matching line numbers, leaving this as "false" can lead to unexpected results whenever `<ed2in_tags>` contains tags missing from the `ED2IN` template.
  - Add some additional documentation for ED2 `pecan.xml` tags.

### Removed

### Changed
- Updated MAAT model model2netcdf.MAAT.R to reflect general changes to the netCDF time variable in PEcAn standard output. Added time_bounds attribute and variable.  Updated inst/ scripts for created MAAT drivers from NGEE-Tropics met sources (WIP)
- `PEcAn.utils::do_conversions` has been moved to `PEcAn.workflow::do_conversions`.
  `PEcAn.utils::do_conversions` still works for now with a warning, but is deprecated and will be removed in the future.
- Docker:
  - Change base image for R code from `r-base` to `rocker/tidyverse:3.5.1`. This (1) saves build time (because many R packages and system dependencies are pre-installed), and (2) enhances reproducibility (because of the strict versioning rules of the `rocker` packages)
  - Re-factor web interface RabbitMQ create connections and post messages into their own PHP functions.

## [1.6.0] - 2018-09-01

### Fixes
- Updated model2netcdf.SIPNET() to address issue #2094. Revised netCDF time to be from 0-364./365. (if leap) so time would be properly parsed by R and python (cf tools)
- Fixed output time variable in models/ed/R/model2netcdf.ED2.R to provide correct fractional DOY
- Running tests for PEcAn.settings package no longer leaves empty temp directories in test folder (#2075)
- Fixed issue #2064 which sends one met path to write.sa.config.
- `PEcAn.data.land::soil_params` now accepts any 2 out of 3 texture components as documented, and correctly converts percentages to proportion (#2043).
- Added missing ncdf4 library calls in model2netcdf.JULES

### Added
- Added download.LandTrendr.AGB and extract.LandTrendr.AGB functions in modules/data.remote
- Added new time_bounds variable in SIPNET output netCDF files to define the exact start time and end time for each model timestep.
- Updated models/ed/R/model2netcdf.ED2.R to include new time_bounds variable
- Added a first vignette to models/maat with the plan to add more examples
- Added scaling to documentation

### Removed
- Removed unused PEcAn.utils::counter(), which existed to increment a global variable that is also unused.

### Changed
- Updated models/fates/R/model2netcdf.FATES.R to increase supported model outputs. Added longname to nc file variables
- Updated models/dalec/R/model2netcdf.DALEC.R to add time_bounds variable
- Updated models/maat/R/write.config.MAAT.R to improve flow, remove bugs, and to work with the release version of the MAAT model.
- Minor update to modules/data.atmosphere/R/met2CF.csv.R to include recursive=TRUE for outfolder.  Seemed to work better
- Updated models/maat/R/met2model.MAAT.R to include additional output variables, fix a bug, and conduct overall cleanup. Updated docs
- Updated models/maat/R/model2netcdf.MAAT.R to work with the release version of the MAAT model. Other small MAAT wrapper code cleanup
- Small change to modules/data.atmosphere/R/download.NARR_site.R to set parallel=TRUE to match documentation and sub-function calls


## [1.6.0] - Not yet

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
- missing_port_bety    
- dataone_download.R:

    - Added functionality that spoofs our user address to prevent authentication errors with downloading files via wget.
- Could not specify the port for BETY in config.php. Can now use `db_bety_port` to specify port.

    - Added functionality that spoofs our user address to prevent authentication errors with downloading files via wget.

- Data_Ingest_App:
    - use `updateSelectizeInput` to populate `selectizeInput` with choices from BETYdb. This instantly loads the inputfields where other methods take minutes to load.


### Added
- sda.enkf function inside the `PEcAn.assim.sequential` package was replaced with the refactored version, while the original sda function can be found in the same package with the name of sda.enkf.original.
- PEcAn.undertainty gains one new function (input.ens.gen) and three functions moved from PEcAn.utils (see "Changed" below)
- IC workflow now has functionality to generate ensembles.
- You can now generate ensembles for parameters and met separatly and using different methods.
- Soil process is now capable of reading in soil data from gSSURGO databse.
- In modules/rtm new function foursail()  to interface with the 4SAIL Fortran code. To enable the use of 4SAIL with any version of PROSPECT (i.e. 4, 5, 5b, D) and custom soil/background reflectance inputs
- Shiny/Dependency explorer
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
  - Replaced modularized input, format and dbfiles records with static "Ingest Workflow" page. On this page, the user can select either importing from dataONE or Uploading from local files. If creating a new format is necessary, the user can click "Create New Format" and a dropdown menu will walk them through this process.
  - Selected files now autofill name value in input record workflow
  - Store inputs and formats in the global environment
  - "Test BETY" button allows users create a record in BETY with `dbfile.input.insert`
  - Added `input.format.vars` to query the BETYdb
  - New File: `helper.R`
  - New Function: `auto.name.directory` This function uses the format_name and the site_id for a given input to create a directory name in the style of other dbfiles names.
  - `Next Step` buttons progress workflow programmatically
  - New formats-variables UI allows user to create a table of formats-variable records before completing the ingest process
  - Two separate complete Ingest buttons are rendered at the end of the workflow to trigger actions specific to local upload or dataONE download workflows. These buttons are rendered programmatically depending on the state of the selectInputMethod radio button.
  - Converted time inputs to properly merge startDate and startTime with EndDate and EndTime so they can be inserted into the start_date and end_date columns in BETYdb.
  - Error handling introduced using `shinytoastr` package
  - DESCRIPTION: `Depends`: PEcAn.visualization, shinytoastr, shinyWidgets, shinyjs

- pecan/base/db
  - New File: `input.format.vars.R`. This function registers the format and the (optional) formats_variables record using `db_merge_into`.

- `data.atmosphere`
	- `check_met_input_file` -- Check that target met file conforms to PEcAn meteorology data standard.
	- `get_cf_variables_table` -- Retrieve CF variables table as a `data.frame`


- docker:
  - Added updated docker container builds
    - Use docker.sh to create docker images
    - Use release.sh to push released images to push to docker registry (hub.docker.com by default)
  - Create pecan/depends docker image that holds all PEcAn dependencies
    - Needs to build seperatly, not part of the docker.sh build process to speed things up
    - Build using `(cd docker ; docker build -t pecan/depends:latest -f Dockerfile.depends .)`
  - docker-compose.yml file to bring up full PEcAn stack using docker
    - First time to start requires to install BETY database (see documentation)
  - SIPNET docker image which works with PEcAn docker stack
  - Data container that will download and install demo data in /data folder


### Removed
  - pecan.worldmap function no longer used, dropped from visualization package
  - shiny/Data-Ingest/DESCRIPTION no longer `DEPENDS` on `shinyFiles` or `shinycssloaders`

### Changed
- PEcAn.utils functions run.write.configs and runModule.run.write.configs have been moved to PEcAn.workflow. The versions in PEcAn.utils are deprecated and will be removed in a future release.
- Fixed Git instructions and remote execution instructions.
- Five functions from PEcAn.utils functions have been moved to other packages. The versions in PEcAn.utils are deprecated, will not be updated with any new features, and will be removed in a future release.
  - run.write.configs and runModule.run.write.configs have been moved to PEcAn.workflow
  - read.ensemble.output, get.ensemble.samples and write.ensemble.configs have been moved to PEcAn.uncertainty
- Change the way packages are checked for and called in SHINY apps. DESCRIPTION files in SHINY apps are not the place to declare pacakge dpendencies.

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
