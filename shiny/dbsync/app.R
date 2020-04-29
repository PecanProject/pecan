library(shiny)
library(leaflet)
library(dbplyr)
library(RPostgreSQL)

# cached geo information to prevent frequent lookups
geoip <- list()
geocache <- Sys.getenv("GEOCACHE", "geoip.json")

# maximum number of lines to consider of sync log, if this is
# large the update sync can take long time
maxlines <- 5000

# maximum time in hours before sync is red
maxtime <- 24

# number of bins to use when rendering lines
maxbins <- 5

# show hosts with missing sync_url
allow_no_url <- FALSE

# mapping to fix hostnames
host_mapping <- list(
    "wisconsin"="tree.aos.wisc.edu",
    "terra-mepp.igb.illinois.edu"="terra-mepp.illinois.edu",
    "ecn.purdue.edu"="engineering.purdue.edu",
    "paleon-pecan.virtual.crc.nd.edu"="crc.nd.edu"
)

# ignored servers, is reset on refresh
ignored_servers <- c()

# given a IP address lookup geo spatital info
# uses a cache to prevent to many requests (1000 per day)
get_geoip <- function(ip) {
    if (length(geoip) == 0 && file.exists("geoip.json")) {
        geoip <<- jsonlite::read_json(geocache, simplifyVector = TRUE)
    }
    if (! ip %in% geoip$ip) {
        print(paste("CACHE MISS", ip))
        res <- curl::curl_fetch_memory(paste0("http://free.ipwhois.io/json/", ip))
        if (res$status -- 200) {
            geoloc <- jsonlite::parse_json(rawToChar(res$content))
            geoloc[lengths(geoloc) == 0] <- NA
            geoloc <- type.convert(geoloc, as.is = TRUE)
        } else {
            geoloc <- list(ip=ip, lat=0, lon=0, city="?", countr="?")
        }
        if (length(geoip) == 0) {
            geoip <<- as.data.frame(geoloc)
        } else {
            geoip <<- rbind(geoip, as.data.frame(geoloc))
        }
        jsonlite::write_json(geoip, geocache)
    }
}

# get a list of all servers in BETY and their geospatial location
get_servers <- function() {
    ignored_servers <<- c()
    
    # connect to BETYdb
    bety <- DBI::dbConnect(
        DBI::dbDriver("PostgreSQL"), 
        dbname = Sys.getenv("PGDATABASE", "bety"),
        host = Sys.getenv("PGHOST", "localhost"),
        user = Sys.getenv("PGUSER", "bety"),
        password = Sys.getenv("PGPASSWORD", "bety")
    )

    servers <- dplyr::tbl(bety, "machines") %>% 
        dplyr::filter(!is.na(sync_host_id)) %>%
        dplyr::filter(sync_url != "" || allow_no_url) %>%
        dplyr::arrange(sync_host_id) %>%
        dplyr::select(hostname, sync_host_id, sync_url, sync_start, sync_end) %>% 
        dplyr::collect() %>%
        dplyr::mutate(ip = unlist(lapply(hostname, function(x) {
            if (x %in% names(host_mapping)) {
                ip <- nsl(host_mapping[[x]])
            } else {
                ip <- nsl(x)
            }
            ifelse(is.null(ip), NA, ip)
        }))) %>%
        dplyr::mutate(version = NA, lastdump = NA, migrations = NA) %>%
        dplyr::filter(!is.na(ip)) %>%
        dplyr::arrange(ip)
        
    # close connection
    DBI::dbDisconnect(bety)
    
    # convert ip address to geo location
    lapply(servers$ip, get_geoip)
    locations <- geoip %>% 
        dplyr::filter(ip %in% servers$ip) %>%
        dplyr::arrange(ip) %>%
        dplyr::select("city", "country", "latitude", "longitude")
    
    # combine tables
    servers <- cbind(servers, locations)
    
    # add columns for all sync_ids
    servers[, paste0("server_", servers$sync_host_id)] <- NA
    
    # return servers
    servers %>% dplyr::arrange(sync_host_id)
}

# fetch information from the actual servers
check_servers <- function(servers, progress) {
    check_servers <- servers$sync_url[! servers$sync_host_id %in% ignored_servers]

    # generic failure message to increment progress
    failure <- function(res) {
        print(res)
        progress$inc(amount = 1)
    }
    
    # version information  
    server_version <- function(res) {
        url <- sub("version.txt", "bety.tar.gz", res$url)
        progress$inc(amount = 0, message = paste("Processing", progress$getValue(), "of", progress$getMax()))
        print(paste(res$status, url))
        if (res$status == 200 || res$status == 226) {
            check_servers <<- check_servers[check_servers != url]
            version <- strsplit(rawToChar(res$content), '\t', fixed = TRUE)[[1]]
            if (!is.na(as.numeric(version[1]))) {
                servers[servers$sync_url == url,'version'] <<- version[2]
                servers[servers$sync_url == url,'lastdump']  <<- version[4]
                servers[servers$sync_url == url,'migrations']  <<- version[1]
            } else {
                servers[servers$sync_url == url,'version'] <<- NA
                servers[servers$sync_url == url,'lastdump'] <<- NA
                servers[servers$sync_url == url,'migrations']  <<- NA
            }
        }
        progress$inc(amount = 1)
    }
    urls <- sapply(check_servers, function(x) { sub("bety.tar.gz", "version.txt", x) })
    lapply(urls, function(x) { curl::curl_fetch_multi(x, done = server_version, fail = failure) } )
    
    # log information  
    server_log  <- function(res) {
        url <- sub("sync.log", "bety.tar.gz", res$url)
        progress$inc(amount = 0, message = paste("Processing", progress$getValue(), "of", progress$getMax()))
        print(paste(res$status, url))
        if (res$status == 200 || res$status == 226) {
            lines <- strsplit(rawToChar(res$content), '\n', fixed = TRUE)[[1]]
            now <- as.POSIXlt(Sys.time(), tz="UTC")
            for (line in tail(lines, maxlines)) {
                pieces <- strsplit(trimws(line), ' ', fixed=TRUE)[[1]]
                if (length(pieces) == 8) {
                    if (pieces[8] == 0) {
                        when <- strptime(paste(pieces[1:6], collapse = " "), format="%a %b %d %T UTC %Y", tz="UTC")
                        tdiff <- min(maxtime, difftime(now, when, units = "hours"))
                        servers[servers$sync_url == url, paste0('server_', pieces[7])] <<- tdiff
                    }
                } else {
                    print(line)
                }
            }
        }
        progress$inc(amount = 1)
    }
    urls <- sapply(check_servers, function(x) { sub("bety.tar.gz", "sync.log", x) })
    lapply(urls, function(x) { curl::curl_fetch_multi(x, done = server_log, fail = failure) } )
    
    # run queries in parallel
    curl::multi_run()
    ignored_servers <<- c(ignored_servers, servers[servers$sync_url %in% check_servers, "sync_host_id"])

    return(servers)
}

# return vector to use in polylines
check_sync <- function(servers) {
    ids <- servers$sync_host_id
    
    # helper function to see if two servers are connected    
    connected <- function(src, dst) {
        v <- servers[servers$sync_host_id==src, paste0("server_", dst)]
        !is.null(v) && !is.na(v)
    }
    
    # build up list of all connections
    lat <- c()
    lon <- c()
    tdiff <- c()
    
    for (src in ids) {
        src_x <- servers[servers$sync_host_id==src, 'longitude']
        src_y <- servers[servers$sync_host_id==src, 'latitude']
        for (dst in ids) {
            if (connected(src, dst)) {
                tdiff <- c(tdiff, servers[servers$sync_host_id==src, paste0("server_", dst)])
                
                dst_x <- servers[servers$sync_host_id==dst, 'longitude']
                lon <- c(lon, c(src_x, (src_x + (dst_x - src_x) / 2), NA))
                
                dst_y <- servers[servers$sync_host_id==dst, 'latitude']
                lat <- c(lat, c(src_y, (src_y + (dst_y - src_y) / 2), NA))
            }
        }
    }
    
    # need to have at least one polyline, will draw a line from server 1 to server 1
    if (length(tdiff) == 0) {
        src_x <- servers[1, 'longitude']
        src_y <- servers[1, 'latitude']
        return(list(latitude=c(src_y, src_y, NA), longitude=c(src_x, src_x, NA), value=c(0)))
    } else {
        return(list(latitude=lat, longitude=lon, value=tdiff))
    }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    singleton(tags$head(HTML(
        '
<script type="text/javascript">
    $(document).ready(function() {
        // disable start_proc button after a click
        Shiny.addCustomMessageHandler("disableUI", function(message) {
            $("#refresh_servers").attr("disabled", "true");
            $("#refresh_sync").attr("disabled", "true");
        });
        
        // Enable start_proc button when computation is finished
        Shiny.addCustomMessageHandler("enableUI", function(message) {
            $("#refresh_servers").removeAttr("disabled");
            $("#refresh_sync").removeAttr("disabled");
        });
    })
</script>
        '
    ))),
    
    # Application title
    titlePanel("PEcAn DB Sync"),

    # Map with sites
    leaflet::leafletOutput("map"),

        # data table
    DT::dataTableOutput("table"),

    # Refresh button
    actionButton("refresh_servers", "Update Servers"),
    actionButton("refresh_sync", "Update Sync")
)

# Define server logic required to draw map
server <- function(input, output, session) {
    # red -> green color spectrum
    colors <- leaflet::colorBin("RdYlGn", domain = c(0, maxtime), bins = maxbins, na.color = "purple", reverse = TRUE)
    
    # servers is what is changed, start with just data from database
    servers <- get_servers()
    values <- reactiveValues(servers=servers,
                             sync=check_sync(servers))

    # update server list (quick)
    observeEvent(input$refresh_servers, {
        session$sendCustomMessage("disableUI", "")
        values$servers <- get_servers()
        session$sendCustomMessage("enableUI", "")
    })
    
    # update sync list (slow)
    observeEvent(input$refresh_sync, {
        servers <- values$servers
        session$sendCustomMessage("disableUI", "")
        progress <- Progress$new(session, min=0, max=2*(nrow(servers)-length(ignored_servers)))
        servers <- check_servers(servers, progress)
        sync <- check_sync(servers)
        progress$close()
        session$sendCustomMessage("enableUI", "")
        values$servers <- servers
        values$sync <- sync
    })
    
    # create a map of all servers that have a sync_host_id and sync_url
    output$map <- renderLeaflet({
        leaflet(values$servers) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(~longitude, ~latitude,
                       label = ~htmltools::htmlEscape(hostname),
                       clusterOptions = markerClusterOptions(maxClusterRadius = 1)) %>%
            addPolylines(~longitude, ~latitude,
                         color = colors(values$sync$value), data=values$sync) %>%
            addLegend("bottomright", colors, values$sync$value,
                      title = "since last sync", labFormat = labelFormat(suffix =" hours"))
    })

    # create a table of all servers that have a sync_host_id and sync_url
    output$table <- DT::renderDataTable({
        ignored <- rep("gray", length(ignored_servers) + 1)
        DT::datatable(values$servers %>% 
                          dplyr::select("sync_host_id", "hostname", "city", "country", "lastdump", "migrations"),
                      rownames = FALSE) %>%
        DT::formatStyle('sync_host_id',  target = "row", color = DT::styleEqual(c(ignored_servers, "-1"), ignored))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
