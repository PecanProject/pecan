##' @name download.FIA
##' @title download.FIA
##' @export
download.FIA <- function(lat, lon, year, gridres = 0.075, con){
  
  gridres  <- 0.075
  lonmin   <- lon - gridres
  lonmax   <- lon + gridres
  latmin   <- lat - gridres
  latmax   <- lat + gridres
  min.year <- year - 5
  max.year <- year + 5
  
  ##################
  ##              ##
  ##     PSS      ##
  ##              ##
  ##################
  ## query to get plot info
  query <- paste("SELECT p.cycle, p.statecd, p.measyear as time, p.cn as patch, ", 
                 "MIN(2-c.stdorgcd) as trk, AVG(c.stdage) as age, p.lat, p.lon, p.prev_plt_cn ", 
                 "FROM plot as p LEFT JOIN cond as c on p.cn=c.plt_cn ", 
                 "WHERE p.lon >= ", lonmin, " AND p.lon <= ", lonmax, " AND p.lat >= ", latmin, 
                 " AND p.lat <= ", latmax, " AND p.measyear >= ", min.year, 
                 " AND p.measyear <= ", max.year, " GROUP BY p.cn")
  
  pss.info <- db.query(query, con = fia.con)
  if (nrow(pss.info) == 0) {
    logger.severe("No plot data found on FIA.")
  }
  
  for (statecd in unique(pss.info$statecd)) {
    # Count up occurrences of each cycle
    cycle.count <- table(pss.info$cycle[pss.info$statecd == statecd])
    
    # Find the best valid cycle, in terms of providing the most records. 
    # In case of ties, which.max will return the first one, which will be the earliest
    best.cycle <- as.numeric(names(cycle.count)[which.max(cycle.count)])
    
    row.keep.ind <- (pss.info$statecd != statecd) | (pss.info$cycle == best.cycle)
    
    pss.info <- pss.info[row.keep.ind, ]
  }
  
  # as an extra precaution, remove any records that are explicitly remeasurments of the same plot
  pss.info <- pss.info[.select.unique.fia.plot.records(pss.info$patch, pss.info$prev_plt_cn, pss.info$time, year), ]
  
  if (nrow(pss.info) == 0) {
    logger.severe("All plot data were invalid.")
  }
  
  
  pss.info$trk[which(is.na(pss.info$trk))] <- 1
  pss.info$age[which(is.na(pss.info$age))] <- 0
  
  n.patch <- nrow(pss.info)
  
  ## fill missing data w/ defaults
  pss.info$site  <- rep(1, n.patch)
  pss.info$area  <- rep(1 / n.patch, n.patch)
  pss.info$water <- rep(0, n.patch)
  
  # Add soil data
  soil            <- c(1, 5, 5, 0.01, 0, 1, 1)  #soil C & N pools (biogeochem) defaults (fsc,stsc,stsl,ssc,psc,msn,fsn)\t
  soil.dat        <- as.data.frame(matrix(soil, n.patch, 7, byrow = TRUE))
  names(soil.dat) <- c("fsc", "stsc", "stsl", "ssc", "psc", "msn", "fsn")
  pss.info       <- cbind(pss.info, soil.dat)
  
  logger.debug(paste0("Found ", nrow(pss), " patches for site ", settings$run$site$id))
  
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
  css.info <- db.query(query, con = fia.con)
  
  names(css.info) <- tolower(names(css.info))
  if (nrow(css.info) == 0) {
    logger.severe("No FIA data found.")
  } else {
    logger.debug(paste0(nrow(css.info), " trees found initially"))
  }
  
  # Remove rows that don't map to any retained patch
  css.info <- css.info[which(css.info$patch %in% pss$patch), ]
  if (nrow(css.info) == 0) {
    logger.severe("No trees map to previously selected patches.")
  } else {
    logger.debug(paste0(nrow(css.info), " trees that map to previously selected patches."))
  }
  
  ## Remove rows with no dbh, spcd, or n
  notree <- which(is.na(css.info$dbh) & is.na(css.info$spcd) & is.na(css.info$n))
  if (length(notree) > 0) {
    css.info <- css.info[-notree, ]
  }
  if (nrow(css.info) == 0) {
    logger.severe("No trees remain after removing entries with no dbh, spcd, and/or n.")
  } else {
    logger.debug(paste0(nrow(css.info), " trees remain after removing entries with no dbh, spcd, and/or n."))
  }
  

  prefix.psscss <- paste0("siteid", runinfo$site$id, ".", inputinfo$source, year, ".radius", gridres, 
                          get.ed.file.latlon.text(lat, lon, site.style = FALSE))
  prefix.site <- paste0("siteid", settings$run$site$id, ".fia", year, ".radius", gridres, 
                        get.ed.file.latlon.text(lat, lon, site.style = TRUE))
  
  return(list(pss = pss.info, css = css.info, prefix.psscss = prefix.psscss, prefix.site = prefix.site))
} # download.FIA


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
    logger.error("Inputs must have same length!")
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