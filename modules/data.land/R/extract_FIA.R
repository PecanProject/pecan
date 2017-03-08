##' @name extract_FIA
##' @title extract_FIA
##' @export
extract_FIA <- function(lon, lat, start_date, gridres = 0.075, dbparms){
  
  #--------------------------------------------------------------------------------------------------#
  # Extract FIA
  fia.info <- list()
  
  fia.con <- PEcAn.DB::db.open(dbparms$fia)
  on.exit(db.close(fia.con), add = T)
  
  lonmin   <- lon - gridres
  lonmax   <- lon + gridres
  latmin   <- lat - gridres
  latmax   <- lat + gridres
  
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  
  min.year <-  start_year - 5
  max.year <-  start_year + 5
  

    
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
  pss.info <- pss.info[.select.unique.fia.plot.records(pss.info$patch, pss.info$prev_plt_cn, pss.info$time, start_year), ]
    
  if (nrow(pss.info) == 0) {
    logger.severe("All plot data were invalid.")
  }
    
  pss.info$trk[is.na(pss.info$trk)] <- 1
  pss.info$age[is.na(pss.info$age)] <- 0
    
  # Dropping unneeded columns
  pss.info <- pss.info[, c("time", "patch", "trk", "age")]
    
  logger.debug(paste0("Found ", nrow(pss.info), " patches for coordinates lat:", lat, " lon:", lon))
  
  fia.info[[1]] <- pss.info
    
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
  css.info <- css.info[which(css.info$patch %in% pss.info$patch), ]
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
    
  fia.info[[2]] <- css.info
  
  return(fia.info)
} # extract_FIA


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