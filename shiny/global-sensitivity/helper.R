library(shiny)
library(PEcAn.DB)
library(RPostgreSQL)
library(udunits2)
library(lubridate)
library(dplyr)

options(scipen=12)

betyConnect <- function(php.config="../../web/config.php") {
  ## Read PHP config file for webserver
  config = scan(php.config,what="character",sep="\n")
  config = config[grep("^\\$",config)]  ## find lines that begin with $ (variables)
  config = sub("$","",config,fixed = TRUE) ## remove $
  config = sub(";","",config,fixed = TRUE) ## remove ;
  config = sub("false","FALSE",config,fixed = TRUE) ##  Boolean capitalization
  config = sub("true","TRUE",config,fixed = TRUE) ##  Boolean capitalization
  config = config[-grep("$",config,fixed=TRUE)] ## lines with variable references fail
  config = config[-grep("exec",config,fixed=TRUE)] ## lines 'exec' fail
  config.list = eval(parse(text=paste('list(', paste0(config[1:14],collapse=","), ')')))

  ## Database connection
  src_postgres(dbname   = config.list$db_bety_database,
               host     = config.list$db_bety_hostname,
               user     = config.list$db_bety_username,
               password = config.list$db_bety_password)
}

fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # keep 0 as 0
  l <- gsub("0e\\+00","0", l)
  # return this as an expression
  parse(text=l)
}

dplyr.count <- function(df) {
  collect(tally(df))[['n']]
}

ncdays2date <- function(time, unit) {
  date <- parse_date_time(unit, c("ymd_hms", "ymd_h", "ymd"))
  days <- ud.convert(time, unit, paste("days since ", date))
  seconds <- ud.convert(days, 'days', 'seconds')
  as.POSIXct.numeric(seconds, origin = date, tz = "UTC")
}

db.hostInfo <- function(bety) {
  # get host id
  result <- db.query("select cast(floor(nextval('users_id_seq') / 1e9) as bigint);", bety$con)
  hostid <- result[['floor']]

  # get machine start and end based on hostid
  machine <- tbl(bety, 'machines') %>%
                filter(sync_host_id == hostid) %>%
                dplyr::select(sync_start, sync_end)

  if (is.na(nrow(machine)) || nrow(machine) == 0) {
    list(hostid = hostid, start = 1000000000 * hostid, end = 1000000000 * (hostid + 1) - 1)
  } else {
    list(hostid = hostid, start = machine$sync_start, end = machine$sync_end)
  }
}

# list of workflows that exist
workflows <- function(bety, ensemble=FALSE) {
  hostinfo <- db.hostInfo(bety)
  if(ensemble){
    query <- paste("SELECT ensembles.id AS ensemble_id, ensembles.workflow_id, workflows.folder",
                   "FROM ensembles, workflows WHERE runtype = 'ensemble'")
  } else {
    query <- "SELECT id AS workflow_id, folder FROM workflows"
  }
  out <- tbl(bety, sql(query)) %>% filter(workflow_id >= hostinfo$start & workflow_id <= hostinfo$end)
  return(out)
}

workflow <- function(bety, workflowId) {
  workflows(bety) %>% filter(workflow_id == workflowId)
}

runs <- function(bety, workflowId) {
  Workflows <- workflow(bety, workflowId) %>%
    dplyr::select(workflow_id, folder)
  Ensembles <- tbl(bety, 'ensembles') %>%
    dplyr::select(ensemble_id=id, workflow_id) %>%
    inner_join(Workflows, by='workflow_id')
  Runs <- tbl(bety, 'runs') %>%
    dplyr::select(run_id=id, ensemble_id) %>%
    inner_join(Ensembles, by='ensemble_id')
  dplyr::select(Runs, -workflow_id, -ensemble_id)
}

get_workflow_ids <- function(bety, session){
  query <- isolate(parseQueryString(session$clientData$url_search))
  if ("workflow_id" %in% names(query)) {
    ids <- unlist(query[names(query) == 'workflow_id'], use.names=FALSE)
  } else {
    # Get all workflow IDs
    ids <- workflows(bety, ensemble=TRUE) %>% distinct(workflow_id) %>% collect %>% .[["workflow_id"]] %>% sort(decreasing = TRUE)
  }
  return(ids)
}

get_run_ids <- function(bety, workflow_id){
    run_ids <- c("No runs found")
    if (workflow_id != "") {
      runs <- runs(bety, workflow_id)
      if (dplyr.count(runs) > 0) {
        run_ids <- collect(runs)[['run_id']] %>% sort
      }
    }
    return(run_ids)
}

get_var_names <- function(bety, workflow_id, run_id, remove_pool = TRUE){
  var_names <- character(0)
  if (workflow_id != "" && run_id != "") {
    workflow <- collect(workflow(bety, workflow_id))
    if(nrow(workflow) > 0) {
      outputfolder <- file.path(workflow$folder, 'out', run_id)
      if(file_test('-d', outputfolder)) {
        files <- list.files(outputfolder, "*.nc$", full.names=TRUE)
        for(file in files) {
          nc <- nc_open(file)
          lapply(nc$var, function(x) if(x$name != "") var_names[[x$longname]] <<- x$name)
          nc_close(nc)
        }
      }
    }
    if (length(var_names) == 0) {
      var_names <- "No variables found"
    }
    if (remove_pool){
      var_names <- var_names[!grepl("pool", var_names, ignore.case = TRUE)]  ## Ignore "poolnames" and "carbon pools" variables
    }
  }
  return(var_names)
}
