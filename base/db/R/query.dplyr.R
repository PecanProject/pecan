#' Connect to bety using current PEcAn configuration
#' @param php.config Path to `config.php`
#' @export
#' 
betyConnect <- function(php.config = "../../web/config.php") {
  ## Read PHP config file for webserver

  config.list <- PEcAn.utils::read_web_config(php.config)

  ## Database connection
  # TODO: The latest version of dplyr/dbplyr works with standard DBI-based
  # objects, so we should replace this with a standard `db.open` call.
  src_postgres(dbname = config.list$db_bety_database,
               host = config.list$db_bety_hostname,
               user = config.list$db_bety_username,
               password = config.list$db_bety_password)
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


#' Convert netcdf number of days to date
#' @export
ncdays2date <- function(time, unit) {
  date    <- lubridate::parse_date_time(unit, c("ymd_hms", "ymd_h", "ymd"))
  days    <- udunits2::ud.convert(time, unit, paste("days since ", date))
  seconds <- udunits2::ud.convert(days, "days", "seconds")
  return(as.POSIXct.numeric(seconds, origin = date, tz = "UTC"))
}  # ncdays2date


#' Database host information
#'
#' @param bety BETYdb connection, as opened by `betyConnect()`
#' @export
#' @importFrom magrittr %>%
dbHostInfo <- function(bety) {
  # get host id
  result <- db.query(query = "select cast(floor(nextval('users_id_seq') / 1e9) as bigint);", con = bety$con)
  hostid <- result[["floor"]]

  # get machine start and end based on hostid
  machine <- dplyr::tbl(bety, "machines") %>%
    dplyr::filter(sync_host_id == hostid) %>%
    dplyr::select(sync_start, sync_end)

  if (is.na(nrow(machine)) || nrow(machine) == 0) {
    return(list(hostid = hostid,
                start = 1e+09 * hostid,
                end = 1e+09 * (hostid + 1) - 1))
  } else {
    return(list(hostid = hostid,
                start = machine$sync_start,
                end = machine$sync_end))
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
    dplyr::filter(workflow_id >= hostinfo$start & workflow_id <= hostinfo$end) %>%
    return()
}  # workflows


#' Get single workflow by workflow_id
#' @param workflow_id Workflow ID
#' @inheritParams dbHostInfo
#' @export
workflow <- function(bety, workflow_id) {
  workflows(bety) %>%
    dplyr::filter_(paste("workflow_id ==", workflow_id)) %>%
    return()
}  # workflow


#' Get table of runs corresponding to a workflow
#' @inheritParams dbHostInfo
#' @inheritParams workflow
#' @export
runs <- function(bety, workflow_id) {
  Workflows <- workflow(bety, workflow_id) %>%
    dplyr::select(workflow_id, folder)
  Ensembles <- dplyr::tbl(bety, "ensembles") %>%
    dplyr::select(ensemble_id = id, workflow_id) %>%
    inner_join(Workflows, by = "workflow_id")
  Runs <- dplyr::tbl(bety, "runs") %>%
    dplyr::select(run_id = id, ensemble_id) %>%
    dplyr::inner_join(Ensembles, by = "ensemble_id")
  dplyr::select(Runs, -workflow_id, -ensemble_id) %>%
    return()
}  # runs


#' Get vector of workflow IDs
#' @inheritParams dbHostInfo
#' @param session Session object passed through Shiny
#' @export
get_workflow_ids <- function(bety, session, all.ids=FALSE) {
  query <- isolate(shiny::parseQueryString(session$clientData$url_search))
  # If we dont want all workflow ids but only workflow id from the user url query
  if (!all.ids & "workflow_id" %in% names(query)) {
    ids <- unlist(query[names(query) == "workflow_id"], use.names = FALSE)
  } else {
    # Get all workflow IDs

    ids <- workflows(bety, ensemble = FALSE) %>% distinct(workflow_id) %>% collect %>% 
      .[["workflow_id"]] %>% sort(decreasing = TRUE)
    # pull(.,workflow_id) %>% sort(decreasing = TRUE)

#    ids <- workflows(bety, ensemble = TRUE) %>%
#      dplyr::distinct(workflow_id) %>%
#      dplyr::pull() %>%
#      sort(decreasing = TRUE)

  }
  return(ids)
}  # get_workflow_ids

#' Get data frame of users and IDs
#' @inheritParams dbHostInfo
#' @param session Session object passed through Shiny
#' @export
get_users <- function(bety, session) {
  hostinfo <- dbHostInfo(bety)
  query <- "SELECT id, login FROM users"
  out <- dplyr::tbl(bety, dbplyr::sql(query)) %>%
    dplyr::filter(id >= hostinfo$start & id <= hostinfo$end)
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
      run_ids <- dplyr::pull(runs, run_id) %>% sort()
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
    workflow <- collect(workflow(bety, workflow_id))
    if (nrow(workflow) > 0) {
      outputfolder <- file.path(workflow$folder, "out", run_id)
      if (file_test("-d", outputfolder)) {
        files <- list.files(outputfolder, "*.nc$", full.names = TRUE)
        for (file in files) {
          nc <- nc_open(file)
          lapply(nc$var, function(x) {
            if (x$name != "") {
              var_names[[x$longname]] <<- x$name
            }
          })
          nc_close(nc)
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
  workflow <- dplyr::collect(workflow(bety, workflow_id))
  # Use the function 'var_names_all' to get all variables
  var_names <- var_names_all(bety, workflow_id, run_id)
  # TODO: This looks a lot like `read.output`. Should probably just use that here.
  # Using earlier code, refactored
  if (nrow(workflow) > 0) {
    outputfolder <- file.path(workflow$folder, 'out', run_id)
    files <- list.files(outputfolder, "*.nc$", full.names = TRUE)
    for (file in files) {
      nc <- nc_open(file)
      for (var_name in var_names) {
        dates <- NA
        vals <- NA
        title <- var_name
        ylab <- ""
        var <- ncdf4::ncatt_get(nc, var_name)
        #sw <- if ('Swdown' %in% names(nc$var)) ncdf4::ncvar_get(nc, 'Swdown') else TRUE
        # Snow water
        sw <- TRUE
        # Check required bcoz many files don't contain title
        if (!is.null(var$long_name)) {
          title <- var$long_name
        }
        # Check required bcoz many files don't contain units
        if (!is.null(var$units)) {
          ylab <- var$units
        }
        x <- ncdays2date(ncdf4::ncvar_get(nc, 'time'), ncdf4::ncatt_get(nc, 'time'))
        y <- ncdf4::ncvar_get(nc, var_name)
        b <- !is.na(x) & !is.na(y) & sw != 0
        
        dates <- if(is.na(dates)) x[b] else c(dates, x[b])
        dates <- as.POSIXct(dates)
        vals <- if(is.na(vals)) y[b] else c(vals, y[b])

        xlab <- "Time"
        # Values of the data which we will plot
        valuesDF <- data.frame(dates,vals)
        # Meta information about the data.
        metaDF <- data.frame(workflow_id,run_id,title,xlab,ylab,var_name)
        # Meta and Values DF created differently because they would of different
        # number of rows. cbind would repeat metaDF(1X6) to the size of valuesDF
        currentDF <- cbind(valuesDF,metaDF)
        globalDF <- rbind(globalDF,currentDF)
      }
      ncdf4::nc_close(nc)
    }
  }
  # Required to convert from factors to characters
  # Otherwise error by ggplotly
  globalDF$title <- as.character(globalDF$title)
  globalDF$xlab <- as.character(globalDF$xlab)
  globalDF$ylab <- as.character(globalDF$ylab)
  globalDF$var_name <- as.character(globalDF$var_name)
  return(globalDF)
} #load_data_single_run
