#code taken from https://github.com/eco4cast/neon4cast/blob/main/R/noaa_gefs.R
#' noaa_stage2
#'
#' @param cycle Hour at which forecast was made, as character string 
#' (`"00"`, `"06"`, `"12"` or `"18"`). Only `"00"` (default) has 30 days horizon.
#' @param version GEFS forecast version. Prior versions correspond to forecasts
#' issued before 2020-09-25 which have different ensemble number and horizon,
#' among other changes, and are not made available here. Leave as default.
#' @param endpoint the EFI host address (leave as default)
#' @param verbose logical, displays or hides messages
#' @param start_date forecast start date yyyy-mm-dd format
#' 
#' @export
#'
#' @author Alexis Helgeson (taken from neon4cast package)
noaa_stage2 <- function(cycle = 0,
                        version = "v12",
                        endpoint = "data.ecoforecast.org",
                        verbose = TRUE,
                        start_date = "") {
  noaa_gefs_stage(file.path("stage2/parquet",cycle, start_date), 
                  partitioning = "start_date",
                  version = version, 
                  endpoint = endpoint,
                  verbose = verbose,
                  start_date = start_date)
  
}

noaa_gefs_stage <- function(stage = "stage1",
                            partitioning = c("cycle","start_date"),
                            cycle = 0,
                            version = "v12",
                            endpoint = "data.ecoforecast.org",
                            verbose = getOption("verbose", TRUE),
                            start_date = start_date) {
  if(verbose) 
    message(paste("establishing connection to", stage, "at", endpoint, "..."))
  s3 <- noaa_gefs(version, endpoint)
  if (!is.na(as.Date(start_date))) {
    ds <- arrow::open_dataset(s3$path(stage))
  } else {
    ds <- arrow::open_dataset(s3$path(stage), partitioning = partitioning)
  }
  if(verbose)
    message(paste0("connected! Use dplyr functions to filter and summarise.\n",
                   "Then, use collect() to read result into R\n"))
  ds  
}
noaa_gefs <- function(version = "v12",
                      endpoint = "data.ecoforecast.org") {
  
  vars <- arrow_env_vars()
  gefs <- arrow::s3_bucket(paste0("neon4cast-drivers/noaa/gefs-", version),
                           endpoint_override = endpoint,
                           anonymous = TRUE)
  #error is coming from this chunk Error: NotImplemented: Got S3 URI but Arrow compiled without S3 support
  on.exit(unset_arrow_vars(vars))
  gefs
  
}

arrow_env_vars <- function(){
  user_region <- Sys.getenv("AWS_DEFAULT_REGION")
  user_meta <- Sys.getenv("AWS_EC2_METADATA_DISABLED")
  Sys.unsetenv("AWS_DEFAULT_REGION")
  Sys.setenv(AWS_EC2_METADATA_DISABLED="TRUE")
  
  list(user_region=user_region, user_meta = user_meta)
}

unset_arrow_vars <- function(vars) {
  Sys.setenv("AWS_DEFAULT_REGION" = vars$user_region)
  if (vars$user_meta != "") {
    Sys.setenv(AWS_EC2_METADATA_DISABLED = vars$user_meta)
  }
}
