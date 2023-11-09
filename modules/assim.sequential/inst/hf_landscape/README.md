## README: Harvard Forest landscape-scale data assimilation

1. 01_SiteSelection.Rmd: used to classify a landscape into patches. Outputs patches.csv, a summary table of patch statistics, and patches.tif, a map of the patches. Assumes NEON AOP LAI files have been downloaded.

2. 02_SiteConfig.Rmd: builds reference pecan multisettings object (saved as binary) and initial conditions. Can set up sitegroup if needed.

3. 03_InitEnsNoSDA.Rmd: doesn't need to be run. Was a first pass at running an ensemble forecast outside of the SDA code. Has some useful visualization code

4. 04_ForwardOnlyForecast.R: Needs to be run for the initial time point in an iterative forecast or reanalysis to establish the parameter sampling and first round of outputs that the iterative SDA needs to start the "restart" cycle. Probably could tweak the reanalysis script to handle this case too, but probably not worth the effort

NOTE: This script and others currently assume that NOAA GEFS forecast has already been downloaded as a meteorological driver

5. 05_SDA_Workflow_NA.R: Code to run an iterative forecast or reanalysis with NO DATA CONSTRAINTS (i.e. NA). Runs a single iteration. The script 05B_SDA_Workflow_NA.reanalysis.R sets up a loop that calls this function iteratively (every day) to run the forecast reanalysis. The equivalent 05C is for the daily forecast, and runs for the last k days, checking to see if the forecast was already run. 05C is what should be put in your cron job. 

NOTE: both 05B and 05C push results to a minio S3 bucket, which would need to be configured for a specific server. The code also assumes the existance of a minio_secrets.R file that contains the following:
minio_key <- Sys.getenv("MINIO_ACCESS_KEY", "username")
minio_secret <- Sys.getenv("MINIO_SECRET_KEY", "password")

6. 06_cron.Rmd: Code that sets up the cron jobs for the automated forecasts. Currently calls 05C (unconstrained forecast) and 07 (SMAP data prep)

7. 07_SMAP.R: Code that does the SMAP data download required for ObsPrep. Can be run both to grab past data (by manually defining smap_dates vector) or as a cron job to just grab the last k days. (currently k = 5)


Next Steps:

* Refine and harden code for visualization and post-processing

* Abstract out a lot of hard-coded paths to a meta-settings file

* Get code working in "remote" mode

* SMAP SDA

* Data download and prep for MODIS. Will require development of a fractional operation operator

* Data prep for flux data. Will require development of a flux footprint operation operator.

* Extend to more sites
