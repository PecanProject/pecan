#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------#
##' 
##' Start biocro model runs on local
##' @title Start run of biocro model
##' @param runid the id of the run (folder in runs) to execute
##' @export
##' @author Rob Kooper, David LeBauer, Deepak Jaiswal
start.runs.BIOCRO <- function(runid) {
  hostname <- system("hostname", intern = TRUE)

  if(settings$run$host$name == "localhost"){
      settings$run$host$name <- hostname
  } else {
      logger.error("BioCro module only configured to run locally")
  }

  rundir <- file.path(settings$run$host$rundir, as.character(runid))
  outdir <- file.path(settings$run$host$outdir, as.character(runid))

  # run model
  # compute/download weather
  lat <- as.numeric(settings$run$site$lat)
  lon <- as.numeric(settings$run$site$lon)
  start.date <- settings$run$start.date
  end.date <- settings$run$end.date
  site.id = settings$run$site$id
  
  con <- query.base.con(settings)
  site.info <- query.base(paste0("select * from sites where id = ", site.id, ";"), con = con)
  site.exists <- nrow(site.info) == 1
  # TODO code commented out below, code is not finished
  #if(site.exists){
  #  if(abs(lat - site.info$lat) + abs(lon - site.info(lon)))
  #}
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
    ## if > 1, use the first record
    metfile <- file.path(metfiles$file_path[1],
                         metfiles$file_name[1])

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
  
  W <- data.table(weachNEW(weather, lati = lat, ts = 1, 
                                  temp.units="Celsius", rh.units="fraction", 
                                  ws.units="mph", pp.units="in"))
  setnames(W, old = c("Temp", "WS"), new = c("DailyTemp.C", "WindSpeed"))

  years <- W[,unique(year)]

  # run model
  
  config <- xmlToList(xmlParse(file.path(rundir, "config.xml")))
  pp.config <- config$pft$photoParms
  pp <- lapply(photoParms(vmax=pp.config$vmax, b0=pp.config$b0, b1 = pp.config$b1,Rd=pp.config$Rd), as.numeric)
  cc <- canopyParms(Sp = as.numeric(config$pft$canopyControl$Sp))

  genus <- config$pft$genus


  if(!(genus %in% c("Saccharum", "Salix", "Miscanthus"))) {
      logger.error("genus", genus, "not supported by PEcAn.BIOCRO module")
  }

  out <- NULL ## could be pre-allocated for speed
  result <- NULL
  for(year in years){
      WetDat <- W[year == year,]
      day1 <- WetDat[,min(doy)]
      dayn <- WetDat[,max(doy)]
      if(genus == "Saccharum"){
          result <- caneGro(WetDat = WetDat, photoControl=pp, canopyControl=cc)
          result[["Grain"]] <- result[["Rhizome"]] <- rep(0, length(result$Hour))
      } else if (genus == "Salix") {
          if(is.null(result)){
              iplant <- iwillowParms(iRhizome=1.0, iStem=1.0, iLeaf=0.0,
                                     iRoot=1.0, ifrRhizome=0.01, ifrStem=0.01,
                                     ifrLeaf = 0.0, ifrRoot = 0.0)
          } else if(!is.null(result)){
              r <- last(result[, list(Rhizome, Stem, Root)])
              iplant$iRhizome <- r$Rhizome
              iplant$iStem <- r$Stem
              iplant$iRoot <- r$Root
          }
          result <- willowGro(WetDat = WetDat, photoControl=pp, canopyControl=cc,
                              day1 = day1, dayn = dayn)
      } else if (genus == "Miscanthus") {
          result <- BioGro(WetDat = WetDat, photoControl = pp, canopyControl = cc)
      }
      result <- with(result,
                 data.table(DayofYear, Hour, ThermalT, Stem, Leaf, Root, Rhizome, Grain, LAI, SoilEvaporation, CanopyTrans))
      if(is.null(out)) {
          out <- result
      } else {
          out <- rbind(out, result)
      }
  }
  result <- out
  if(is.null(result)) logger.error("no output from BioCro")
  
  write.csv(result, file=file.path(outdir, "result.csv"))

  save(result, config, file = file.path(outdir, "result.RData"))

  file.copy(file.path(rundir, "README.txt"), file.path(outdir, "README.txt"))
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
