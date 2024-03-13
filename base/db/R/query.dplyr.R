#' Connect to bety using current PEcAn configuration
#' @param php.config Path to `config.php`
#' @export
#'
betyConnect <- function(php.config = "../../web/config.php") {
  ## Read PHP config file for webserver
  if (file.exists(php.config)) {
    php_params <- PEcAn.utils::read_web_config(php.config)
  } else {
    php_params <- list()
  }

  ## helper function
  getphp = function (item, default = "") {
    value = php_params[[item]]
    if (is.null(value)) default else value
  }

  ## fill in all data from environment variables
  dbparams <- get_postgres_envvars(host = getphp("db_bety_hostname", "localhost"),
                                   port = getphp("db_bety_port", "5432"),
                                   dbname = getphp("db_bety_database", "bety"),
                                   user = getphp("db_bety_username", "bety"),
                                   password = getphp("db_bety_password", "bety"))

  ## force driver to be postgres (only value supported by db.open)
  dbparams[["driver"]] <- "Postgres"

  ## Database connection
  db.open(dbparams)
}  # betyConnect

#' Convert number to scientific notation pretty expression
#' @param l Number to convert to scientific notation
#' @export
fancy_scientific <- function(l) {
  options(scipen = 12)
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # keep 0 as 0
  l <- gsub("0e\\+00", "0", l)
  # return this as an expression
  return(parse(text = l))
}  # fancy_scientific


#' Count rows of a data frame
#' @param df Data frame of which to count length
#' @export
dplyr.count <- function(df) {
  return(dplyr::collect(dplyr::tally(df))[["n"]])
}  # dplyr.count


#' Convert netcdf number of days to a datetime
#' @param time number of time units elapsed since origin
#' @param unit string containing CF-style time unit including origin (e.g. "days since 2010-01-01")
#' @export
ncdays2date <- function(time, unit) {
  date    <- lubridate::parse_date_time(unit, c("ymd_HMS", "ymd_H", "ymd"))
  days    <- PEcAn.utils::ud_convert(time, unit, paste("days since ", date))
  seconds <- PEcAn.utils::ud_convert(days, "days", "seconds")
  return(as.POSIXct.numeric(seconds, origin = date, tz = "UTC"))
}  # ncdays2date


#' Database host information
#'
#' @param bety BETYdb connection, as opened by `betyConnect()`
#' @export
dbHostInfo <- function(bety) {
  # get host id
  result <- db.query(query = "select cast(floor(nextval('users_id_seq') / 1e9) as bigint);", con = bety)
  hostid <- result[["floor"]]

  # get machine start and end based on hostid
  machine <- dplyr::tbl(bety, "machines") %>%
    dplyr::filter(.data$sync_host_id == !!hostid)


  if (is.na(nrow(machine)) || nrow(machine) == 0) {
    return(list(hostid = hostid,
                hostname = "",
                start = 1e+09 * hostid,
                end = 1e+09 * (hostid + 1) - 1,
                sync_url = "",
                sync_contact = ""))
  } else {
    return(list(hostid = hostid,
                hostname = machine$hostname,
                start = machine$sync_start,
                end = machine$sync_end,
                sync_url = machine$sync_url,
                sync_contact = machine$sync_contact))
  }
}  # dbHostInfo


#' list of workflows that exist
#' @param ensemble Logical. Use workflows from ensembles table.
#' @inheritParams dbHostInfo
#' @export
workflows <- function(bety, ensemble = FALSE) {
  hostinfo <- dbHostInfo(bety)
  if (ensemble) {
    query <- paste("SELECT ensembles.id AS ensemble_id, ensembles.workflow_id, workflows.folder",
                   "FROM ensembles, workflows WHERE runtype = 'ensemble'")
  } else {
    query <- "SELECT id AS workflow_id, folder FROM workflows"
  }
  dplyr::tbl(bety, dbplyr::sql(query)) %>%
    dplyr::filter(.data$workflow_id >= !!hostinfo$start & .data$workflow_id <= !!hostinfo$end) %>%
    return()
}  # workflows


#' Get single workflow by workflow_id
#' @param workflow_id Workflow ID
#' @inheritParams dbHostInfo
#' @export
workflow <- function(bety, workflow_id) {
  workflows(bety) %>%
    dplyr::filter(.data$workflow_id == !!workflow_id)
}  # workflow


#' Get table of runs corresponding to a workflow
#' @inheritParams dbHostInfo
#' @inheritParams workflow
#' @export
runs <- function(bety, workflow_id) {
  Workflows <- workflow(bety, workflow_id) %>%
    dplyr::select("workflow_id", "folder")
  Ensembles <- dplyr::tbl(bety, "ensembles") %>%
    dplyr::select(ensemble_id = "id", "workflow_id") %>%
    dplyr::inner_join(Workflows, by = "workflow_id")
  Runs <- dplyr::tbl(bety, "runs") %>%
    dplyr::select(run_id = "id", "ensemble_id") %>%
    dplyr::inner_join(Ensembles, by = "ensemble_id")
  dplyr::select(Runs, -"workflow_id", -"ensemble_id") %>%
    return()
}  # runs


#' Get vector of workflow IDs
#' @inheritParams dbHostInfo
#' @param query Named vector or list of workflow IDs
#' @param all.ids logical: return a list of all workflow_ids in the BETY database,
#'  or just those that are part of the query?
#' @export
get_workflow_ids <- function(bety, query, all.ids = FALSE) {
  # If we dont want all workflow ids but only workflow id from the user url query
  if (!all.ids && "workflow_id" %in% names(query)) {
    ids <- unlist(query[names(query) == "workflow_id"], use.names = FALSE)
  } else {
    # Get all workflow IDs
    ids <- workflows(bety, ensemble = FALSE) %>%
      dplyr::distinct(.data$workflow_id) %>%
      dplyr::collect() %>%
      dplyr::pull("workflow_id") %>%
      sort(decreasing = TRUE)
  }
  return(ids)
}  # get_workflow_ids

#' Get data frame of users and IDs
#' @inheritParams dbHostInfo
#' @export
get_users <- function(bety) {
  hostinfo <- dbHostInfo(bety)
  query <- "SELECT id, login FROM users"
  out <- dplyr::tbl(bety, dbplyr::sql(query)) %>%
    dplyr::filter(.data$id >= hostinfo$start & .data$id <= hostinfo$end)
  return(out)
}  # get_workflow_ids


#' Get vector of run IDs for a given workflow ID
#' @inheritParams dbHostInfo
#' @inheritParams workflow
#' @export
get_run_ids <- function(bety, workflow_id) {
  run_ids <- c("No runs found")
  if (workflow_id != "") {
    runs <- runs(bety, workflow_id)
    if (dplyr.count(runs) > 0) {
    run_ids <- dplyr::pull(runs, "run_id") %>% sort()
    }
  }
  return(run_ids)
}  # get_run_ids


#' Get vector of variable names for a particular workflow and run ID
#' @inheritParams dbHostInfo
#' @inheritParams workflow
#' @param run_id Run ID
#' @param remove_pool logical: ignore variables with 'pools' in their names?
#' @export
get_var_names <- function(bety, workflow_id, run_id, remove_pool = TRUE) {
  var_names <- character(0)
  if (workflow_id != "" && run_id != "") {
    workflow <- dplyr::collect(workflow(bety, .data$workflow_id))
    if (nrow(workflow) > 0) {
      outputfolder <- file.path(workflow$folder, "out", run_id)
      if (utils::file_test("-d", outputfolder)) {
        files <- list.files(outputfolder, "*.nc$", full.names = TRUE)
        for (file in files) {
          nc <- ncdf4::nc_open(file)
          lapply(nc$var, function(x) {
            if (x$name != "") {
              var_names[[x$longname]] <<- x$name
            }
          })
          ncdf4::nc_close(nc)
        }
      }
    }
    if (length(var_names) == 0) {
      var_names <- "No variables found"
    }
    if (remove_pool) {
      var_names <- var_names[!grepl("pool", var_names, ignore.case = TRUE)]  ## Ignore 'poolnames' and 'carbon pools' variables
    }
  }
  return(var_names)
}  # get_var_names

#' Get vector of variable names for a particular workflow and run ID
#' @inheritParams get_var_names
#' @param run_id Run ID
#' @param workflow_id Workflow ID
#' @export
var_names_all <- function(bety, workflow_id, run_id) {
  # @return List of variable names
  # Get variables for a particular workflow and run id
  var_names <- get_var_names(bety, workflow_id, run_id)
  # Remove variables which should not be shown to the user
  removeVarNames <- c('Year','FracJulianDay')
  var_names <- var_names[!var_names %in% removeVarNames]
  return(var_names)
} # var_names_all

#' Load data for a single run of the model
#' @inheritParams var_names_all
#' @inheritParams workflow
#' @param run_id Run ID
#' @param workflow_id Workflow ID
#' @export

load_data_single_run <- function(bety, workflow_id, run_id) {
  # For a particular combination of workflow and run id, loads
  # all variables from all files.
  # @return Dataframe for one run
  # Adapted from earlier code in pecan/shiny/workflowPlot/server.R
  globalDF <- data.frame()
  workflow <- dplyr::collect(workflow(bety, .data$workflow_id))
  # Use the function 'var_names_all' to get all variables
  var_names <- var_names_all(bety, workflow_id, run_id)
  # lat/lon often cause trouble (like with JULES) but aren't needed for this basic plotting
  var_names <- setdiff(var_names, c("lat", "latitude", "lon", "longitude"))
  outputfolder <- file.path(workflow$folder, 'out', run_id)
  out <- PEcAn.utils::read.output(runid = run_id, outdir = outputfolder, variables = var_names, dataframe = TRUE)
  ncfile <- list.files(path = outputfolder, pattern = "\\.nc$", full.names = TRUE)[1]
  nc <- ncdf4::nc_open(ncfile)

  globalDF <- tidyr::gather(out, key = "var_name", value = "vals", names(out)[names(out) != "posix"]) %>%
    dplyr::rename(dates = "posix")
  globalDF$workflow_id <- workflow_id
  globalDF$run_id <- run_id
  globalDF$xlab <- "Time"
  globalDF$ylab <- unlist(sapply(globalDF$var_name, function(x){
    if(!is.null(nc$var[[x]]$units)){
      return(nc$var[[x]]$units)
    }else{
      return("")
    }
  } ))
  globalDF$title <- unlist(lapply(globalDF$var_name, function(x){
    long_name <- names(which(var_names == x))
    ifelse(length(long_name) > 0, long_name, x)
  }
  ))

  return(globalDF)
} #load_data_single_run
