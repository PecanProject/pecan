library(shiny)
library(PEcAn.DB)
library(RPostgreSQL)
library(dplyr)
library(udunits2)
library(lubridate)

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
  hostid <- 99

  # get machine start and end based on hostid
  machine <- tbl(bety, 'machines') %>%
                filter(sync_host_id == hostid) %>%
                select(sync_start, sync_end)

  if (nrow(machine) == 0) {
    list(hostid = hostid, start = 1000000000 * hostid, end = 1000000000 * (hostid + 1) - 1)
  } else {
    list(hostid = hostid, start = machine$sync_start, end = machine$sync_end)
  }
}

# list of workflows that exist
workflows <- function(bety) {
  hostinfo <- db.hostInfo(bety)

  tbl(bety, 'workflows') %>%
    filter(id >= hostinfo$start & id <= hostinfo$end)
}

workflow <- function(bety, workflowId) {
  tbl(bety, 'workflows') %>%
    filter(id == workflowId)
}

runs <- function(bety, workflowId) {
  workflows <- tbl(bety, 'workflows') %>%
    filter(id == workflowId) %>%
    select(workflow_id=id, folder)
  ensembles <- tbl(bety, 'ensembles') %>%
    select(ensemble_id=id, workflow_id) %>%
    inner_join(workflows, by='workflow_id')
  runs <- tbl(bety, 'runs') %>%
    select(run_id=id, ensemble_id) %>%
    inner_join(ensembles, by='ensemble_id')
  select(runs, -workflow_id, -ensemble_id)
}

