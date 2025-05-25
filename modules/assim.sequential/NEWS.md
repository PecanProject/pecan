# PEcAnAssimSequential 1.9.0

## Added

* `tobit_model_censored`, technically not new but newly exported after being refactored out from inside the existing SDA analysis code.
* `aggregate` for spatiol aggregation of model downscaling outputs

# Changed

* `NA_downscale` renamed `SDA_downscale_hrly`
* Documentation improvements


# PEcAnAssimSequential 1.8.0


## Added 

* New features of the SDA function including:
  - allow user-defined free-run mode;
  - allow user-defined parallel mode for the qsub submission;
  - allow user-defined email option to report the progress.

* `GET.MultiSite()` now supports the parallelization of multi-chain MCMC sampling with the fully randomized inits function.
* Added new block-based SDA workflow for the 342 North America anchor sites.

# PEcAnAssimSequential 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.

