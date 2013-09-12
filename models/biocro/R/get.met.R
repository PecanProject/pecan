##' Get Met data from NCEP
##'
##' Retrieves NCEP met data for specified lat x lon and time span
##' @title get NCEP met
##' @param lat 
##' @param lon 
##' @param start.date 
##' @param end.date 
##' @param site.id 
##' @param con
##' @export
##' @return weather data frame with weather formatted for BioCro model
##' @author David LeBauer
get.ncepmet <- function(lat = as.numeric(settings$run$site$lat),
                        lon = as.numeric(settings$run$site$lon),
                        start.date = settings$run$start.date,
                        end.date = settings$run$end.date,
                        site.id = settings$run$site$id,
                        con = query.base.con(settings)){
    hostname <- system("hostname", intern = TRUE)
    site.info <- query.base(paste0("select * from sites where id = ", site.id, ";"), con = con)
    site.exists <- nrow(site.info) == 1
    ## TODO code commented out below, code is not finished
    ##if(site.exists){
    ##  if(abs(lat - site.info$lat) + abs(lon - site.info(lon)))
    ##}
    if(!site.exists){
        query.base(paste0("insert into sites (sitename, lat, lon) values(",
                          vecpaste(c(settings$run$site$name, lat, lon)), ");"), con = con)
    }
    
### TODO the following code should be run during write_configs and the file name passed to the start.runs function
### the next set of code using queries will be passed to a new function called "query.met"
    metfiles <- db.query(paste("select start_date, end_date, hostname, file_name, file_path ",
                               "from inputs join dbfiles on dbfiles.container_id = inputs.file_id ",
                               "join machines on dbfiles.machine_id = machines.id ",
                               "where start_date <= '", start.date, 
                               "' and end_date >= '", end.date, 
                               "' and site_id =", site.id, ";", sep = ""), con = con)
    
    if(nrow(metfiles) == 0){
        metfile.exists <- FALSE
    } else if(nrow(metfiles) >= 1){
        metfile.exists <- TRUE
        ## if > 1, use the last record
        metfile <- with(tail(metfiles, 1),
                        file.path(file_path, file_name))

        if(!file.exists(metfile)){
            metfile.exists <- FALSE
        }
    }

    if(metfile.exists) {
        weather <- read.csv(metfile)
    } else if(!metfile.exists){
        weather.dir <- file.path(settings$run$dbfiles, "met", paste0(abs(lat),
                                                                     ifelse(lat>0,"N", "S"), "x",
                                                                     abs(lon),
                                                                     ifelse(lon>0, "E", "W")))
        weather <- InputForWeach(lat, lon, year(start.date), year(end.date))
        dir.create(weather.dir, recursive = TRUE, showWarnings = FALSE)
        weather.dir <- path.expand(weather.dir)
        write.csv(weather,
                  file = file.path(weather.dir, "weather.csv"), row.names = FALSE)
        machine.id <- db.query(paste0("select id from machines where hostname = '", hostname, "';"), con = con)
        if(nrow(machine.id) == 0){
            machine.id <- db.query("select max(id) + 1 as id from machines;", con = con)
            db.query(paste0("insert into machines (id, hostname, created_at) values(",
                            vecpaste(c(machine.id, hostname, format(Sys.time()))), ");"), con = con)
        }
        file.id <- 1 + db.query(paste0(
            "select 1 + greatest(max(inputs.file_id), max(dbfiles.container_id)) as id ",
            " from inputs right join dbfiles on inputs.file_id =",
            "dbfiles.container_id;"), con = con)
        
        db.query(paste0("insert into dbfiles (file_name, file_path, created_at, machine_id, container_id) ",
                        "values(", vecpaste(c('weather.csv', weather.dir, format(Sys.time()), machine.id, file.id)),");"), con = con)
        db.query(paste0("insert into inputs ",
                        "(notes, created_at, site_id, file_id, start_date, ",
                        "end_date, access_level, format_id) ",
                        "values('downloaded from NCEP', now(),",
                        vecpaste(c(site.id, file.id, start.date, end.date, 4, 28)), ");"), con = con)
        
    }
    query.close(con)
    
    ## Subset Weather based on start / end dates    ## explicitly state UTC primarily for consistency between
    ## weather data and start/end dates;
    ## these can be modified if/when explicit timezones are used  
    weatherdates <- with(weather,
                         as.Date(day-1, origin = paste0(year, "-01-01"),
                                 tz = "UTC"))
    start <- as.Date(start.date, tz = "UTC")
    end   <- as.Date(end.date,   tz = "UTC")
    keep <- (weatherdates >= start & weatherdates <= end)
    weather <- weather[keep,]
    return(weather)
}
