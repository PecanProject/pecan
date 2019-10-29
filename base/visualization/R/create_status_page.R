#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' Create PEcAn Network status page.
##'
##' This will create a webpage (file_prefix.html) as well as an image
##' (file_prefix.png) that shows the PEcAn Network satus. This will
##' save the geo location of each site as well as some additional
##' information in a cache file (file_prefix.RData). If an update to
##' the BETY schema is detected it will be logged in a file
##' (file_prefix.log).
##'
##' @param config_file Path to `config.php`
##' @param file_prefix prefix used for all files saved
##' @param delta number of seconds before
##'
##' @author Michael Dietze
##' @author Rob Kooper
##'
##' @export
##'
##' @examples
##' \dontrun{
##'   create_status_page('/home/carya/pecan/web/config.php')
##' }
create_status_page <- function(config_file, file_prefix='status', delta=3600) {
  ## Read PHP config file for webserver
  config <- PEcAn.utils::read_web_config(config_file)

  ## Database connection
  bety <- list(user     = config$db_bety_username,
               password = config$db_bety_password,
               host     = config$db_bety_hostname,
               dbname   = config$db_bety_database)

  ## load previous state or initialize
  if(file.exists(paste0(file_prefix, ".RData"))){
    load(paste0(file_prefix, ".RData"))
  } else {
    geoinfo <- list()
    schema_versions <- list()
    nodes <- list()
  }

  ## get nodes from database
  con <- PEcAn.DB::db.open(bety)
  pecan_nodes <- PEcAn.DB::db.query("select * from machines where sync_host_id >= 0 and sync_url != '' order by sync_host_id",con)
  PEcAn.DB::db.close(con)

  ## make sure we have information for all nodes
  for(i in seq_len(nrow(pecan_nodes))) {
    x <- pecan_nodes[i,]

    # small fix for missing http://
    if (!startsWith(x$sync_url, 'http')) {
      x$sync_url <- paste0('http://', x$sync_url)
    }

    # find hostname
    hostname <- sub("https://","", x[['sync_url']])
    hostname <- sub("http://","", hostname)
    hostname <- strsplit(hostname, split = "/")[[1]][1]
    hostname <- strsplit(hostname, split=":")[[1]][1]

    # get geoinfo  (need to use cache)
    if (!(hostname %in% names(geoinfo))) {
      print(paste("Getting geoinfo for", hostname))
      result <- RCurl::getURL(paste0("ip-api.com/csv/",hostname))
      geoinfo[[hostname]] <- strsplit(result,",")[[1]]

      # shift illinois sites into triangle to make them visible
      if (hostname == 'ebi-forecast.igb.illinois.edu') {
        geoinfo[[hostname]][[8]] <- as.numeric(geoinfo[[hostname]][[8]])+0.5
      }
      if (hostname == 'terraref.ncsa.illinois.edu') {
        geoinfo[[hostname]][[8]] <- as.numeric(geoinfo[[hostname]][[8]])-0.5
        geoinfo[[hostname]][[9]] <- as.numeric(geoinfo[[hostname]][[9]])+1
      }
      if (hostname == 'terra-mepp.igb.illinois.edu') {
        geoinfo[[hostname]][[8]] <- as.numeric(geoinfo[[hostname]][[8]])-0.5
        geoinfo[[hostname]][[9]] <- as.numeric(geoinfo[[hostname]][[9]])-1
      }
    }

    # get version info
    version_url <- sub("bety.tar.gz", "version.txt", x$sync_url)
    state    <- 2 # DOWN is the default
    schema   <- '' # no schema found
    lastdump <- 0 # never dumped
    if(RCurl::url.exists(version_url)) {
      temporaryFile <- tempfile()
      utils::download.file(version_url, destfile = temporaryFile, quiet = TRUE)
      version <- scan(temporaryFile,what = "character", sep = "\t", quiet = TRUE)
      unlink(temporaryFile)

      # make sure there are 4 elements (old version no longer supported)
      if (length(version) == 4) {
        state    <- 0
        schema   <- version[2]
        lastdump <- strptime(version[4], '%FT%TZ', tz='UTC')

        # make sure we know about this schema
        if (!(schema %in% schema_versions)) {
          schema_versions <- c(schema_versions, schema)
        }

        # check if the schema has been updated, if so log the event
        if ((hostname %in% nodes) && (nodes[[hostname]]['schema'] != schema)) {
          msg <- paste(Sys.time(), "SCHEMA UPDATE DETECTED ON NODE", x$sync_host_id, hostname,
                       "FROM", nodes[[hostname]]['schema'], "TO", schema)
          write(msg, file = paste0(file_prefix, ".log"), append = TRUE)
        }
      }
    }

    # get sync.log
    log_url <- sub("bety.tar.gz","sync.log", x$sync_url)
    sync <- list()
    if (RCurl::url.exists(log_url)) {
      temporaryFile <- tempfile()
      utils::download.file(log_url, destfile = temporaryFile, quiet = TRUE)
      log <- scan(temporaryFile,what = "character", sep = "\n", quiet = TRUE)
      unlink(temporaryFile)

      # collect last time synced and status
      for(row in log) {
        sync.time <- sub("UTC ","",substr(row, 1, 28))
        sync.time <- strptime(sync.time,"%a %b %d %T %Y", tz="UTC")
        sync.cols <- strsplit(row," ")[[1]]
        if (!is.na(sync.cols[7])) {
          sync[[as.character(sync.cols[7])]] <- list(id = sync.cols[7],
                                                     when = as.numeric(sync.time),
                                                     status = as.numeric(sync.cols[8]))
        }
      }
    }

    # save all information
    nodes[[as.character(x$sync_host_id)]] <- list(sync_host_id=x$sync_host_id,
                                                  sync_url=x$sync_url,
                                                  hostname=hostname,
                                                  lat=as.numeric(geoinfo[[hostname]][8]),
                                                  lon=as.numeric(geoinfo[[hostname]][9]),
                                                  schema=schema,
                                                  state=state,
                                                  lastdump=as.numeric(lastdump),
                                                  sync=sync)
  }

  ## check all nodes:
  ## - schema check, is schema for node latests
  ## - sync check, has node synced with known nodes
  latest_schema <- schema_versions[length(schema_versions)]
  for(x in nodes) {
    # have latest schema
    if (x$state != 2) {
      if (x$schema != latest_schema) {
        nodes[[as.character(x$sync_host_id)]][['state']] <- 1 # behind
      }
    }
    # did this node sync with all other nodes
    for(y in x$sync) {
      if (y$status == 0 && (nodes[[y$id]][['lastdump']] - y$when) > delta) {
        nodes[[as.character(x$sync_host_id)]][['sync']][[y$id]][['status']] <- 2
      }
    }
  }

  ## save some variables
  save(geoinfo, schema_versions, nodes, file=paste0(file_prefix, ".RData"))

  ## create image
  grDevices::png(filename = paste0(file_prefix, ".png"), width = 1200)
  xlim <- grDevices::extendrange(sapply(nodes, function(x) { x$lon }), f = 1)
  ylim <- grDevices::extendrange(sapply(nodes, function(x) { x$lat }), f = 1)
  maps::map("world", xlim = xlim, ylim = ylim)
  maps::map("state", add = TRUE)

  # show all edges
  edgecolors <- c("green","red", "yellow")
  x <- lapply(nodes, function(x) {
    lapply(x$sync, function(y) {
      graphics::segments(
        (x$lon + nodes[[y$id]]$lon) / 2,
        (x$lat + nodes[[y$id]]$lat) / 2,
        x$lon,
        x$lat,
        col = edgecolors[y$status + 1], lwd = 2)
    })
  })

  # show all pecan sites
  nodecolors <- c("green", "yellow", "red")
  x <- lapply(nodes, function(x) {
    graphics::points(
      x$lon, x$lat,
      col = nodecolors[x$state + 1],
      pch = 19, cex = 3)
    graphics::text(x$lon, x$lat, labels = x$sync_host_id)
  })

  # graph done
  graphics::text(xlim[1], ylim[1], labels = Sys.time(), pos = 4)
  grDevices::dev.off()

  ## create html file
  htmltable <- '<html>'
  htmltable <- paste(htmltable, '<head>', sep='\n')
  htmltable <- paste(htmltable, '<link rel="shortcut icon" type="image/x-icon" href="favicon.ico" />', sep='\n')
  htmltable <- paste(htmltable, '<meta http-equiv="refresh" content="60">', sep='\n')
  htmltable <- paste(htmltable, '<title>PEcAn Network Status</title>', sep='\n')
  htmltable <- paste(htmltable, '</head>', sep='\n')
  htmltable <- paste(htmltable, '<body>', sep='\n')
  htmltable <- paste(htmltable, paste0('<img src="', basename(paste0(file_prefix, '.png')), '"/>'), sep='\n')

  htmltable <-  paste(htmltable, '<table>', sep='\n')
  row <- '<tr><th>Sync ID</th><th>Hostname</th><th>Status</th><th>Last</th>'
  for(n in names(nodes)) {
    row <- paste0(row, '<th>', n, '</th>')
  }
  row <- paste0(row, '</tr>')
  htmltable <- paste(htmltable, row, sep='\n')

  status <- c("UP", "BEHIND", "DOWN")
  now <- Sys.time()
  x <- sapply(nodes, function(x) {
    row <- '<tr>'
    row <- paste0(row, '<td>', x$sync_host_id, '</td>')
    row <- paste0(row, '<td>', x$hostname, '</td>')
    row <- paste0(row, '<td align="center">', status[x$state+1], '</td>')
    row <- paste0(row, '<td align="center">', as.POSIXct(x$lastdump, origin = "1970-01-01", tz = "UTC"), '</td>')
    for(n in names(nodes)) {
      if (n == x$sync_host_id) {
        row <- paste0(row, '<td align="center">-</td>')
      } else if (n %in% names(x$sync)) {
        when <- as.POSIXct(x$sync[[n]]$when, origin = "1970-01-01", tz = "UTC")
        row <- paste0(row, '<td align="center">', paste(format(now - when), 'ago'), '</td>')
      } else {
        row <- paste0(row, '<td align="center"></td>')
      }
    }
    row <- paste0(row, '</tr>')
    htmltable <<- paste(htmltable, row, sep='\n')
  })
  htmltable <- paste(htmltable, '</table>', sep='\n')
  htmltable <- paste(htmltable, '</body>', sep='\n')
  htmltable <- paste(htmltable, '</html>', sep='\n')
  write(htmltable, file=paste0(file_prefix, ".html"), append=FALSE)
}
