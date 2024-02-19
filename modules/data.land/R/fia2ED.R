#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' convert x into a table
##'
##' @title fia.to.psscss
##' @param create pss/css files based on data in the fia database
##' @return nothing
##' @export
##' @author Mike Dietze, Rob Kooper, Ryan Kelly
fia.to.psscss <- function(settings, 
                          lat = as.numeric(settings$run$site$lat),
                          lon = as.numeric(settings$run$site$lon),
                          year = lubridate::year(settings$run$start.date),
                          gridres=0.075, 
                          min.year = year - 5,
                          max.year = year + 5,
                          overwrite=FALSE) {

  mimetype    <- "text/plain"
  startdate   <- lubridate::as_date(paste0(year, "-01-01"))
  enddate     <- lubridate::as_date(paste0(year, "-12-31"))
  formatnames <- c("ED2.cohort", "ED2.patch", "ED2.site")
  
  latmax <- lat + gridres
  latmin <- lat - gridres
  lonmax <- lon + gridres
  lonmin <- lon - gridres
  
  ## connect to database
  con <- PEcAn.DB::db.open(settings$database$bety)
  on.exit(PEcAn.DB::db.close(con), add = TRUE)
  
  # Check whether inputs exist already
  if(!overwrite) {
    existing.files <- list()
    for(format in formatnames) {
      existing.files[[format]] <- PEcAn.DB::dbfile.input.check(
        siteid     = settings$run$site$id,
        startdate  = startdate,
        enddate    = enddate,
        mimetype   = mimetype,
        formatname = format,
        parentid   = NA,
        con        = con,
        hostname   = settings$host$name
      )
    }
    
    if (all(sapply(existing.files, function(x) nrow(x) > 0))) {
      file.paths <- lapply(existing.files, function(x) file.path(x$file_path, x$file_name))
      
      settings <- .add.ed2.file.paths.to.settings(settings, 
                                                  css.path = file.paths$ED2.cohort, 
                                                  pss.path = file.paths$ED2.patch, 
                                                  site.path = file.paths$ED2.site)
      
      PEcAn.logger::logger.info("Using existing pss, css, and site files.")
      return(invisible(settings))
    } else {
      PEcAn.logger::logger.info("No existing pss, css, and site files.")
    }
  }
  
  ### collect mapping from spcd to pftid
  query <- NULL
  for (pft in settings$pfts) {
    if (is.null(query)) {
      query <- paste0("SELECT bp.name as pft, bs.spcd FROM pfts as bp INNER JOIN ", 
      "pfts_species AS bps ON bps.pft_id = bp.id INNER JOIN species AS bs ON bs.id = bps.specie_id WHERE ", 
      "bp.name = '", pft$name, "'")
    } else {
      query <- paste0(query, " OR bp.name = '", pft$name, "'")
    }
  }
  pfts <- PEcAn.DB::db.query(query, con = con)
  
  # Convert PFT names to ED2 Numbers
  utils::data(pftmapping)
  for (pft.i in settings$pfts) {
    pft.number <- NULL
    pft.number <- pft.i$constants$num
    if (is.null(pft.number)) {
      pft.number <- pftmapping$ED[which(pftmapping == pft.i$name)]
    }
    if (is.null(pft.number)) {
      PEcAn.logger::logger.severe(paste0("Couldn't find an ED2 PFT number for ", pft.i$name))
    }
    pfts$pft[pfts$pft == pft.i$name] <- pft.number
  }
  
  
  ## Check for NA and duplicate spcds in PFTs
  bad <- length(pfts$spcd %in% c(NA, "0"))
  if (bad > 0) {
    PEcAn.logger::logger.warn(sprintf("There are %d entries with no SPCD (NA or 0). They have been removed.", bad))
    pfts <- pfts[!pfts$spcd %in% c(NA, 0), ]
  }
  
  bad <- pfts$spcd[duplicated(pfts$spcd)]
  if (length(bad) > 0) {
    # Coerce spcds back into species names using data from FIA manual. Makes a more readable warning.
    symbol.table <- PEcAn.DB::db.query("SELECT spcd, \"Symbol\" FROM species where spcd IS NOT NULL", con = con)
    names(symbol.table) <- tolower(names(symbol.table))
    
    # grab the names where we have bad spcds in the symbol.table, exclude NAs
    name.list <- stats::na.omit(symbol.table$symbol[symbol.table$spcd %in% bad])
    
    PEcAn.logger::logger.severe(paste0("The following species are found in multiple PFTs: ", 
                         paste(name.list[1:min(10, length(name.list))], collapse = ", "), 
                         ". Please remove overlapping PFTs."))
  }
  
  ## connect to database
  fia.con <- PEcAn.DB::db.open(settings$database$fia)
  on.exit(PEcAn.DB::db.close(fia.con), add = TRUE)
  
  ##################
  ##              ##
  ##     PSS      ##
  ##              ##
  ##################
  ## query to get PSS info
  query <- paste("SELECT p.cycle, p.statecd, p.measyear as time, p.cn as patch, ", 
                 "MIN(2-c.stdorgcd) as trk, AVG(c.stdage) as age, p.lat, p.lon, p.prev_plt_cn ", 
                 "FROM plot as p LEFT JOIN cond as c on p.cn=c.plt_cn ", 
                 "WHERE p.lon >= ", lonmin, " AND p.lon <= ", lonmax, " AND p.lat >= ", latmin, 
                 " AND p.lat <= ", latmax, " AND p.measyear >= ", min.year, 
                 " AND p.measyear <= ", max.year, " GROUP BY p.cn")
  
  pss <- PEcAn.DB::db.query(query, con = fia.con)
  if (nrow(pss) == 0) {
    PEcAn.logger::logger.severe("No pss data found.")
  }
  
  for (statecd in unique(pss$statecd)) {
    # Count up occurrences of each cycle
    cycle.count <- table(pss$cycle[pss$statecd == statecd])
    
    # Find the best valid cycle, in terms of providing the most records. 
    # In case of ties, which.max will return the first one, which will be the earliest
    best.cycle <- as.numeric(names(cycle.count)[which.max(cycle.count)])
    
    row.keep.ind <- (pss$statecd != statecd) | (pss$cycle == best.cycle)
    
    pss <- pss[row.keep.ind, ]
  }
  
  # as an extra precaution, remove any records that are explicitly remeasurments of the same plot
  pss <- pss[.select.unique.fia.plot.records(pss$patch, pss$prev_plt_cn, pss$time, year), ]
  
  if (nrow(pss) == 0) {
    PEcAn.logger::logger.severe("All pss data were invalid.")
  }
  
  pss$trk[which(is.na(pss$trk))] <- 1
  pss$age[which(is.na(pss$age))] <- 0
  
  n.patch <- nrow(pss)
  
  ## fill missing data w/ defaults
  pss$site <- rep(1, n.patch)
  pss$area <- rep(1 / n.patch, n.patch)
  pss$water <- rep(0, n.patch)
  
  # Reorder columns, dropping unneeded ones
  pss <- pss[, c("site", "time", "patch", "trk", "age", "area", "water")]
  
  # Add soil data
  soil            <- c(1, 5, 5, 0.01, 0, 1, 1)  #soil C & N pools (biogeochem) defaults (fsc,stsc,stsl,ssc,psc,msn,fsn)\t
  soil.dat        <- as.data.frame(matrix(soil, n.patch, 7, byrow = TRUE))
  names(soil.dat) <- c("fsc", "stsc", "stsl", "ssc", "psc", "msn", "fsn")
  pss             <- cbind(pss, soil.dat)
  
  PEcAn.logger::logger.debug(paste0("Found ", nrow(pss), " patches for site ", settings$run$site$id))
  
  ##################
  ##              ##
  ##     CSS      ##
  ##              ##
  ##################
  query <- paste0("SELECT p.measyear as time,p.cycle,p.statecd,p.cn as patch, 
                  ", "CONCAT(CAST(t.subp AS CHAR),CAST(t.tree AS CHAR)) as cohort,t.dia*2.54 as dbh, ", 
                  "t.spcd as spcd, t.tpa_unadj*0.0002471 as n FROM plot as p LEFT JOIN tree as t on p.cn=t.plt_cn ",
                  "WHERE p.lon >= ", lonmin, 
                  " and p.lon < ", lonmax, 
                  " and p.lat >= ", latmin,
                  " and p.lat < ", latmax)
  css <- PEcAn.DB::db.query(query, con = fia.con)
  names(css) <- tolower(names(css))
  if (nrow(css) == 0) {
    PEcAn.logger::logger.severe("No FIA data found.")
  } else {
    PEcAn.logger::logger.debug(paste0(nrow(css), " trees found initially"))
  }
  
  # Remove rows that don't map to any retained patch
  css <- css[which(css$patch %in% pss$patch), ]
  if (nrow(css) == 0) {
    PEcAn.logger::logger.severe("No trees map to previously selected patches.")
  } else {
    PEcAn.logger::logger.debug(paste0(nrow(css), " trees that map to previously selected patches."))
  }
  
  
  ## Remove rows with no dbh, spcd, or n
  notree <- which(is.na(css$dbh) & is.na(css$spcd) & is.na(css$n))
  if (length(notree) > 0) {
    css <- css[-notree, ]
  }
  if (nrow(css) == 0) {
    PEcAn.logger::logger.severe("No trees remain after removing entries with no dbh, spcd, and/or n.")
  } else {
    PEcAn.logger::logger.debug(paste0(nrow(css), " trees remain after removing entries with no dbh, spcd, and/or n."))
  }
  
  # --- Consistency tests between PFTs and FIA
  fia.species <- unique(css$spcd)
  
  # check for species in PFTs which the FIA db doesn't expect
  pft.ind <- which(!(pfts$spcd %in% fia.species))  #vect shows pft's spcds that are confirmed by fia
  pft.only <- pfts$spcd[pft.ind]  #what were the spcds at those indices? 
  
  if (length(pft.only) > 0) {
    if (!exists("symbol.table")) {
      symbol.table <- PEcAn.DB::db.query("SELECT spcd, \"Symbol\" FROM species where spcd IS NOT NULL", con = con)
      names(symbol.table) <- tolower(names(symbol.table))
    }
    name.list <- stats::na.omit(symbol.table$symbol[symbol.table$spcd %in% pft.only])
    PEcAn.logger::logger.warn(paste0("The selected PFTs contain the following species for which the FIA database ",
                       "contains no data at ", lat, " and ", lon, ": ", 
                       paste(name.list[1:min(10, length(name.list))], collapse = ", "), "."))
  }
  
  # check for species expected by FIA which the PFTs don't cover
  fia.ind <- which(!fia.species %in% pfts$spcd)
  fia.only <- fia.species[fia.ind]
  
  if (length(fia.only) > 0) {
    if (!exists("symbol.table")) {
      symbol.table <- PEcAn.DB::db.query("SELECT spcd, \"Symbol\" FROM species where spcd IS NOT NULL", con = con)
      names(symbol.table) <- tolower(names(symbol.table))
    }
    name.list <- stats::na.omit(symbol.table$symbol[symbol.table$spcd %in% fia.only])
    name.list <- name.list[name.list != "DEAD"]
    if (length(name.list) > 0) {
      PEcAn.logger::logger.warn(paste0("The FIA database expects the following species at ", lat, " and ", lon, 
                         " but they are not described by the selected PFTs: ", 
                         paste(name.list, collapse = ", "), 
                         ". You should select additional pfts if you want to include these. "))
    }
  }
  
  css <- css[!(css$spcd %in% fia.only), ]
  if (nrow(css) == 0) {
    PEcAn.logger::logger.severe(paste0("No trees remain for selected PFTs. ",
      "Species that were in FIA data but didn't map to a selected PFT are: ", 
      paste(name.list, collapse = ", "), "."))
  } else {
    PEcAn.logger::logger.debug(paste0(nrow(css), " trees remain for selected PFTs."))
  }


  
  # --- Continue work formatting css now that we've checked for species problems
  n.cohort                      <- nrow(css)
  css$time[is.na(css$time)]     <- 1
  css$cohort[is.na(css$cohort)] <- 1:sum(is.na(css$cohort))
  css$dbh[is.na(css$dbh)]       <- 1  # assign nominal small dbh to missing
  density.median                <- stats::median(css$n[which(css$n > 0)])
  css$n[is.na(css$n) | css$n == 0]    <- density.median
  css$hite <- css$bdead <- css$balive <- css$lai <- rep(0, n.cohort)
  
  ## map spcd to pft
  css <- merge(css, pfts, by = "spcd")
  css <- css[, c("time", "patch", "cohort", "dbh", "hite", "pft", "n", "bdead", "balive", "lai")]
  
  pfts.represented <- sapply(settings$pfts, function(x) x$constants$num) %in% css$pft
  if (!all(pfts.represented)) 
    PEcAn.logger::logger.warn(paste0(
      "The following PFTs listed in settings are not represented in the FIA data: ",
      paste(sapply(settings$pfts, function(x) x$name)[!pfts.represented], collapse = ", ")))
  
  PEcAn.logger::logger.debug(paste0("Found ", nrow(css), " cohorts for site ", settings$run$site$id))
  
  ##################
  ##              ##
  ##     SITE     ##
  ##              ##
  ##################
  # Obviously, this is just a placeholder for now...
  site <- c(
    "nsite 1 file_format 1", 
    "sitenum area TCI elev slope aspect soil",
    "1 1.0 -7 100.0 0.0 0.0 3"
  )
  
  # ----- Write files 
  # Write files locally
  site.string <- paste0(as.numeric(settings$run$site$id)%/%1e+09, "-", 
                        as.numeric(settings$run$site$id)%%1e+09)
  if (settings$host$name == "localhost") {
    out.dir.local <- file.path(settings$database$dbfiles, paste0("FIA_ED2_site_", site.string))
  } else {
    out.dir.local <- "/tmp"
  }
  prefix.psscss <- paste0("siteid", settings$run$site$id, ".fia", year, ".radius", gridres, 
                          get.ed.file.latlon.text(lat, lon, site.style = FALSE))
  prefix.site <- paste0("siteid", settings$run$site$id, ".fia", year, ".radius", gridres, 
                        get.ed.file.latlon.text(lat, lon, site.style = TRUE))
  pss.file.local <- file.path(out.dir.local, paste0(prefix.psscss, ".pss"))
  css.file.local <- file.path(out.dir.local, paste0(prefix.psscss, ".css"))
  site.file.local <- file.path(out.dir.local, paste0(prefix.site, ".site"))
  
  dir.create(out.dir.local, showWarnings = F, recursive = T)
  utils::write.table(pss, pss.file.local, quote = FALSE, row.names = FALSE)
  utils::write.table(css, css.file.local, quote = FALSE, row.names = FALSE)
  
  site.file.con <- file(site.file.local)
  writeLines(site, site.file.con)
  close(site.file.con)
  
  # Copy to remote if needed
  if (settings$host$name == "localhost") {
    files <- c(pss.file.local, css.file.local, site.file.local)
  } else {
    out.dir.remote   <- file.path(settings$host$folder, paste0("FIA_ED2_site_", site.string))
    pss.file.remote  <- file.path(out.dir.remote, paste0(prefix.psscss, ".pss"))
    css.file.remote  <- file.path(out.dir.remote, paste0(prefix.psscss, ".css"))
    site.file.remote <- file.path(out.dir.remote, paste0(prefix.site, ".site"))
    
    PEcAn.remote::remote.execute.cmd(settings$host, "mkdir", c("-p", out.dir.remote))
    PEcAn.remote::remote.copy.to(settings$host, pss.file.local, pss.file.remote)
    PEcAn.remote::remote.copy.to(settings$host, css.file.local, css.file.remote)
    PEcAn.remote::remote.copy.to(settings$host, site.file.local, site.file.remote)
    files <- c(pss.file.remote, css.file.remote, site.file.remote)
  }
  
  # Insert into DB
  for(i in seq_along(files)) {
    PEcAn.DB::dbfile.input.insert(
      in.path    = dirname(files[i]),
      in.prefix  = basename(files[i]),
      siteid     = settings$run$site$id,
      startdate  = startdate,
      enddate    = enddate,
      mimetype   = mimetype,
      formatname = formatnames[i],
      parentid   = NA,
      con        = con,
      hostname   = settings$host$name,
      allow.conflicting.dates = TRUE
    )
  }
  
  # Add file paths to settings
  if (settings$host$name == "localhost") {
    settings <- .add.ed2.file.paths.to.settings(settings,
                                                css.path = css.file.local,
                                                pss.path = pss.file.local,
                                                site.path = site.file.local)
  } else {
    settings <- .add.ed2.file.paths.to.settings(settings, 
                                                css.path = css.file.remote, 
                                                pss.path = pss.file.remote,
                                                site.path = site.file.remote)
  }
  
  return(invisible(settings))
} # fia.to.psscss

.add.ed2.file.paths.to.settings <- function(settings, css.path, pss.path, site.path) {
  settings$run$inputs$css$path <- css.path
  settings$run$inputs$pss$path <- pss.path
  settings$run$inputs$site$path <- site.path
  return(settings)
} # .add.ed2.file.paths.to.settings

# See ed_read_ed10_20_history...
get.ed.file.latlon.text <- function(lat, lon, site.style = FALSE, ed.res = 1) {
  if (site.style) {
    lat <- ifelse(lat >= 0, ed.res * floor(lat / ed.res) + 0.5 * ed.res, -ed.res * floor(-lat / ed.res) - 0.5 * ed.res)
    lon <- ifelse(lon >= 0, ed.res * floor(lon / ed.res) + 0.5 * ed.res, -ed.res * floor(-lon / ed.res) - 0.5 * ed.res)
    return(paste0(".lat", round(lat, 1), "lon", round(lon, 1)))
  } else {
    return(paste0(".lat", round(lat, 4), "lon", round(lon, 4)))
  }
} # get.ed.file.latlon.text


# A function for identifying fia plot records that are remeasurements of one another,
# and upon finding them retaining only the one that is closest to some target year. 
# Since fia.to.psscss currently selects plots from only a single cycle (for a given state)
# it shouldn't be getting remeasurements, and this probably isn't doing anything in the 
# current code. But it could be useful for future updates. 
.select.unique.fia.plot.records <- function(plt_cn, prev_plt_cn, measyear, target.year) {
  if (length(plt_cn) != length(prev_plt_cn)) {
    PEcAn.logger::logger.error("Inputs must have same length!")
    return(NULL)
  }
  
  # Identify records that are part of the same remeasurement sequence
  prev_plt_cn[prev_plt_cn == ""] <- NA
  unique.plot.id <- rep(NA, length(plt_cn))
  for (i in seq_along(plt_cn)) {
    if (!is.na(unique.plot.id[i])) {
      # already been assigned
      next
    } else {
      # assign a new plot id
      unique.plot.id[i] <- i
    }
    
    # Check whether this record is a remeasurement of another one in the list
    if (!is.na(prev_plt_cn[i])) {
      parent.ind <- which(plt_cn == prev_plt_cn[i])
      if (length(parent.ind) > 0) {
        if (!is.na(unique.plot.id[parent.ind])) {
          # if the parent record has already been given an id, assign it to this plot too
          unique.plot.id[i] <- unique.plot.id[parent.ind]
        } else {
          # Otherwise, use this plot's new id
          unique.plot.id[parent.ind] <- unique.plot.id[i]
        }
        next
      }
    }
    
    # Check whether any other record is a remeasurement of this one
    child.ind <- which(prev_plt_cn == plt_cn[i])
    if (length(child.ind) > 0) {
      # As above, ensure both records have the same plot id
      if (!is.na(unique.plot.id[child.ind])) {
        unique.plot.id[i] <- unique.plot.id[child.ind]
      } else {
        unique.plot.id[child.ind] <- unique.plot.id[i]
      }
    }
  }
  
  # For any identified remeasurement sequences, choose to keep the record that is closest to the target year
  ind.keep <- numeric(0)
  for (unique.id in unique(unique.plot.id)) {
    ind.keep.i <- which(unique.plot.id == unique.id)
    if (length(ind.keep.i) > 1) {
      ind.keep.i <- ind.keep.i[which.min(abs(measyear[ind.keep.i] - target.year))]
    }
    ind.keep <- c(ind.keep, ind.keep.i)
  }
  
  return(sort(ind.keep))
} # .select.unique.fia.plot.records
