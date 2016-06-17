library(shiny)
library(PEcAn.DB)
library(RPostgreSQL)
library(dplyr)
library(lubridate)

bety <-  src_postgres(dbname = "bety", host = "localhost",
                      user = "bety", password = "bety")

options(scipen=12)

dplyr.count <- function(df) {
  collect(tally(df))[['n']]
}

ncdays2date <- function(time) {
  basetime.string <- time$units
  base.date <- parse_date_time(basetime.string, c("ymd_hms", "ymd_h", "ymd"))
  base.units <- strsplit(basetime.string, " since ")[[1]][1]
  time.idx <- c(min(nc$dim$time$vals), max(nc$dim$time$vals))
  time.idx <- ud.convert(time.idx, basetime.string, paste("days since ", base.date))
  time.idx <- ud.convert(time.idx, 'days', 'seconds')
  as.POSIXct.numeric(time.idx, origin = base.date, tz = "UTC")
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

