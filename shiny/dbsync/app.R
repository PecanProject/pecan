library(shiny)
library(leaflet)
library(dbplyr)
library(rgeolocate)
library(RPostgreSQL)

# this file can be downloaded from 
# https://lite.ip2location.com/database/ip-country-region-city-latitude-longitude
file <- "IP2LOCATION-LITE-DB5.BIN"

# get a list of all servers in BETY and their geospatial location
get_servers <- function() {
    # connect to BETYdb
    bety <- DBI::dbConnect(
        DBI::dbDriver("PostgreSQL"), 
        dbname = Sys.getenv("PGDATABASE", "bety"),
        host = Sys.getenv("PGHOST", "postgres"),
        user = Sys.getenv("PGUSER", "bety"),
        password = Sys.getenv("PGPASSWORD", "bety")
    )

    servers <- dplyr::tbl(bety, "machines") %>% 
        dplyr::filter(!is.na(sync_host_id)) %>%
        dplyr::filter(sync_url != "") %>%
        dplyr::arrange(sync_host_id) %>%
        dplyr::select(hostname, sync_host_id, sync_url, sync_start, sync_end) %>% 
        dplyr::collect() %>%
        dplyr::mutate(hostname = replace(hostname, hostname=="wisconsin", "tree.aos.wisc.edu")) %>%
        dplyr::mutate(hostname = replace(hostname, hostname=="terra-mepp.igb.illinois.edu", "terra-mepp.illinois.edu")) %>%
        dplyr::mutate(ip = unlist(lapply(hostname, function(x) {
            ip <- nsl(x)
            ifelse(is.null(ip), NA, ip)
        }))) %>%
        dplyr::mutate(version = NA, lastdump = NA) #%>%
        #dplyr::filter(!is.na(ip))
    
    # close connection
    DBI::dbDisconnect(bety)
    
    # convert ip address to geo location    
    locations <- rgeolocate::ip2location(servers$ip, file, c("city", "lat", "long"))
    
    # combine tables
    servers <- cbind(servers, locations)
    
    # add columns for all sync_ids
    servers[, paste0("server_", servers$sync_host_id)] <- NA
    
    # return servers
    servers
}

# fetch information from the actual servers
check_servers <- function(servers, progress) {
    # generic failure message to increment progress
    failure <- function(res) {
        progress$inc(amount = 1)
    }
    
    # version information  
    server_version <- function(res) {
        progress$inc(amount = 0, message = paste("Processing", progress$getValue(), "of", progress$getMax()))
        if (res$status == 200) {
            url <- sub("version.txt", "bety.tar.gz", res$url)
            version <- strsplit(rawToChar(res$content), '\t', fixed = TRUE)[[1]]
            if (!is.na(as.numeric(version[1]))) {
                servers[servers$sync_url == url,'version'] <<- version[2]
                servers[servers$sync_url == url,'lastdump']  <<- version[4]
            } else {
                servers[servers$sync_url == url,'version'] <<- NA
                servers[servers$sync_url == url,'lastdump'] <<- NA
            }
        }
        progress$inc(amount = 1)
    }
    urls <- sapply(servers[,'sync_url'], function(x) { sub("bety.tar.gz", "version.txt", x) })
    lapply(urls, function(x) { curl::curl_fetch_multi(x, done = server_version, fail = failure, handle = curl::new_handle(connecttimeout=1)) })
    
    # log information  
    server_log  <- function(res) {
        progress$inc(amount = 0, message = paste("Processing", progress$getValue(), "of", progress$getMax()))
        if (res$status == 200) {
            url <- sub("sync.log", "bety.tar.gz", res$url)
            lines <- strsplit(rawToChar(res$content), '\n', fixed = TRUE)[[1]]
            data <- list()
            for (line in lines) {
                pieces <- strsplit(trimws(line), ' ', fixed=TRUE)[[1]]
                if (length(pieces) == 8) {
                    if (pieces[8] == 0) {
                        servers[servers$sync_url == url, paste0('server_', pieces[7])] <<- paste(pieces[1:6], collapse = " ")
                    }
                } else {
                    print(line)
                }
            }
        }
        progress$inc(amount = 1)
    }
    urls <- sapply(servers[,'sync_url'], function(x) { sub("bety.tar.gz", "sync.log", x) })
    lapply(urls, function(x) { curl::curl_fetch_multi(x, done = server_log, fail = failure, handle = curl::new_handle(connecttimeout=1)) })
    
    # run queries in parallel
    curl::multi_run()
    myservers <<- servers
    return(servers)
}

# return vector to use in polylines
check_sync <- function(servers, what) {
    ids <- servers$sync_host_id
    
    # helper function to see if two servers are connected    
    connected <- function(src, dst) {
        v <- servers[servers$sync_host_id==src, paste0("server_", dst)]
        !is.null(v) && !is.na(v)
    }
    
    # build up list of all connections
    result <- c()
    for (src in ids) {
        for (dst in ids) {
            if (connected(src, dst)) {
                if (what == "color") {
                    when <- strptime(servers[servers$sync_host_id==src, paste0("server_", dst)],
                                     format="%a %b %d %T UTC %Y",
                                     tz="UTC")
                    now <- as.POSIXlt(Sys.time(), tz="UTC")
                    tdiff <- difftime(now, when, units = "hours")
                    if (tdiff < 0) {
                        result <- c(result, "purple")
                    } else if (tdiff < 6) {
                        result <- c(result, "green")
                    } else if (tdiff < 24) {
                        result <- c(result, "yellow")
                    } else {
                        result <- c(result, "red")
                    }
                    mycolors <<- result
                } else {
                    src_x <- servers[servers$sync_host_id==src, what]
                    dst_x <- servers[servers$sync_host_id==dst, what]
                    result <- c(result, c(src_x, src_x + (dst_x - src_x) / 2, NA))
                }
            }
        }
    }
    
    # need to have at least one polyline, will draw a line from server 1 to server 1
    if (length(result) == 0) {
        if (what == "color") {
            result <- c("black")
        } else {
            result <- c(servers[1, what], servers[1, what], NA)
        }
    }
    return(result)
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
    actionButton("refresh_sync", "Update Sync"),
    
    # footer
    hr(),
    div(HTML('This site or product includes IP2Location LITE data available from <a href="https://lite.ip2location.com">https://lite.ip2location.com</a>.'))
)

# Define server logic required to draw map
server <- function(input, output, session) {

    # servers is what is changed, start with just data from database
    values <- reactiveValues(servers=get_servers())
    
    # update server list (quick)
    observeEvent(input$refresh_servers, {
        session$sendCustomMessage("disableUI", "")
        values$servers <- get_servers()
        session$sendCustomMessage("enableUI", "")
    })
    
    # update sync list (slow)
    observeEvent(input$refresh_sync, {
        session$sendCustomMessage("disableUI", "")
        progress <- Progress$new(session, min=0, max=2*nrow(values$servers))
        values$servers <- check_servers(values$servers, progress)
        progress$close()
        session$sendCustomMessage("enableUI", "")
    })
    
    # create a map of all servers that have a sync_host_id and sync_url
    output$map <- renderLeaflet({
        leaflet(values$servers) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(~long, ~lat,
                       label = ~htmltools::htmlEscape(hostname),
                       clusterOptions = markerClusterOptions(maxClusterRadius = 1)) %>%
            addPolylines(lng = ~check_sync(values$servers, "long"),
                         lat = ~check_sync(values$servers, "lat"),
                         color = ~check_sync(values$servers, "color"))
    })

    # create a table of all servers that have a sync_host_id and sync_url
    output$table <- DT::renderDataTable({
        DT::datatable(values$servers %>% dplyr::select("sync_host_id", "hostname", "city", "lastdump", "version"))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
