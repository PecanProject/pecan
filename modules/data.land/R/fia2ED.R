#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

library(XML)
library(PEcAn.utils)
library(PEcAn.DB)

##' convert x into a table
##'
##' @title fia.to.psscss
##' @param create pss/css files based on data in the fia database
##' @return nothing
##' @export
##' @author Mike Dietze, Rob Kooper
fia.to.psscss <- function(settings, gridres=0.075) {
	lat     <- as.numeric(settings$run$site$lat)
	lon     <- as.numeric(settings$run$site$lon)
	latmax = lat + gridres
  latmin = lat - gridres
  lonmax = lon + gridres
  lonmin = lon - gridres	
	year <- as.numeric(format(as.Date(settings$run$start.date), '%Y'))


	## connect to database
	con <-  db.open(settings$database$bety)
	
	### collect mapping from spcd to pftid
	query <- NULL
	for (pft in settings$pfts) {
		if (is.null(query)) {
			query <- paste0("SELECT bp.name as pft, bs.spcd FROM pfts as bp INNER JOIN ",
			  "pfts_species AS bps ON bps.pft_id = bp.id INNER JOIN species AS bs ON bs.id = bps.specie_id WHERE ",
			  "bp.name = '", pft$name, "'")
		} else {
			query <- paste(query, " OR bp.name = '", pft$name, "'", sep='')
		}
	}
	pfts <- db.query(query, con=con)
	
	for (pft in settings$pfts) {
		pfts[pfts==pft$name] <- pft$constants$num
	}
	
  	
	## Check for NA and duplicate spcds in PFTs
	bad <- length(pfts$spcd %in% c(NA, "0"))		
	if (bad > 0) {	
		logger.warn(sprintf("There are %d entries with no SPCD (NA or 0). They have been removed.", bad))
		pfts <- pfts[!pfts$spcd %in% c(NA, 0), ]	   
	}
	
	bad <- pfts$spcd[duplicated(pfts$spcd)]
	if (length(bad) > 0) {
  	# format the "and x more." bit if >10 bad species
		over.ten <- ifelse(length(bad) > 10, paste(", and ", length(bad) - 10, " more.", sep=""), ".")		
		
		#Coerce spcds back into species names using data from FIA manual. Makes a more readable warning.
		symbol.table <- db.query('SELECT spcd, "Symbol" FROM species where spcd IS NOT NULL', con=con)
		names(symbol.table) = tolower(names(symbol.table))
		
		# grab the names where we have bad spcds in the symbol.table, exclude NAs
		name.list <- na.omit(symbol.table$symbol[symbol.table$spcd %in% bad]) 		
							
		logger.error(paste0("\nThe following species are found in multiple PFTs: \n",   
		  paste(name.list[1:min(10,length(name.list))], collapse=", "), over.ten, 
		  "\n\tPlease remove overlapping PFTs."))
		
		# Using stop naturally causes an error with the tests - comment stops out for testing.
		stop("Execution stopped due to duplicate species.")			
	}

	## connect to database
	fia.con <- db.open(settings$database$fia)
  
	### select just most current
	query <- paste('SELECT invyr, statecd, stateab, statenm, cycle, subcycle from survey', sep="")
	surv <- db.query(query, con=fia.con)
	names(surv) = tolower(names(surv))
  
	states <- sort(unique(surv$statecd))
	states <- states[states < 72]
	cycle  <- rep(NA,max(states))
	old    <- rep(FALSE,max(states))
	
	## choose the cycle closest to the specified year
	## note:	does not check for COMPLETE cycles
	for(s in states){
		sel <- which(surv$statecd == s)
		cycle[s] <- surv$cycle[sel[which.min(abs(year-surv$invyr[sel]))]]
	}
	
	

  ##################
  ##              ##
  ##     PSS      ##
  ##              ##
  ##################
  ## query to get PSS info
  query <- paste('SELECT p.cycle,p.statecd,p.measyear as time,p.cn as patch,MIN(2-c.stdorgcd) ',
                 'as trk,AVG(c.stdage) as age,p.lat,p.lon FROM plot as p LEFT JOIN cond as c on p.cn=c.plt_cn ',
                 'WHERE p.lon >= ', lonmin, ' and p.lon < ', lonmax,
                 ' and p.lat >= ', latmin, ' and p.lat < ', latmax, ' GROUP BY p.cn')
  pss <- db.query(query, con=fia.con)
  names(pss) = tolower(names(pss))
  pss <- pss[pss$cycle == cycle[pss$statecd],]
  if(length(pss) == 0) logger.error("Couldn't find pss data.")

  pss$trk[which(is.na(pss$trk))] <- 1
  pss$age[which(is.na(pss$age))] <- 0

  n.patch <- nrow(pss)

  ## fill missing data w/ defaults
  pss$site <- rep(1, n.patch)
  pss$area <- rep(1/n.patch, n.patch)
  pss$water <- rep(0, n.patch)
  
  # Reorder columns, dropping unneeded ones
  pss <- pss[, c('site', 'time', 'patch', 'trk', 'age', 'area', 'water')]

  # Add soil data
	soil = c(1.0,5.0,5.0,0.01,0.0,1.0,1.0) #soil C & N pools (biogeochem) defaults (fsc,stsc,stsl,ssc,psc,msn,fsn)	
  soil.dat <- as.data.frame(matrix(soil, n.patch, 7, byrow=TRUE))
  names(soil.dat) <- c('fsc', 'stsc', 'stsl', 'ssc', 'psc', 'msn', 'fsn')
  pss <- cbind(pss, soil.dat)
  


  ##################
  ##              ##
  ##     CSS      ##
  ##              ##
  ##################
  query <- paste0('SELECT p.measyear as time,p.cycle,p.statecd,p.cn as patch, ',
                 'CONCAT(CAST(t.subp AS CHAR),CAST(t.tree AS CHAR)) as cohort,t.dia*2.54 as dbh, ',
                 't.spcd as spcd, t.tpa_unadj*0.0002471 as n FROM plot as p LEFT JOIN tree as t on p.cn=t.plt_cn ',
                 'WHERE p.lon >= ', lonmin, ' and p.lon < ', lonmax, 
                 ' and p.lat >= ', latmin, ' and p.lat < ', latmax)
  css <- db.query(query, con=fia.con)
  names(css) = tolower(names(css))
  css <- css[css$cycle == cycle[css$statecd],]

  ## fill in missing data
  notree <- which(apply(is.na(css[,6:8]),1,sum) == 3)
  if (length(notree) > 0){
    css <- css[-notree,]
  }


  # --- Consistency tests between PFTs and FIA
  fia.species <-unique(css$spcd)

  # check for species in PFTs which the FIA db doesn't expect
  pft.ind <- which(!pfts$spcd %in% fia.species)										#vect shows pft's spcds that are confirmed by fia
  pft.only <- pfts$spcd[pft.ind]														#what were the spcds at those indices? 

  if(length(pft.only) > 0){								
    over.ten <- ifelse(length(pft.only) > 10, paste(", and ", length(pft.only) - 10, " more.", sep=""), ".")
  
    if(!exists("symbol.table")){
      symbol.table <- db.query('SELECT spcd, "Symbol" FROM species where spcd IS NOT NULL', con=con)
      names(symbol.table) = tolower(names(symbol.table))
    }
    name.list <- na.omit(symbol.table$symbol[symbol.table$spcd %in% pft.only]) 
    logger.warn(paste0("\nThe selected PFTs contain the following species for which the FIA database ", 
       "contains no data at ", lat, " and ", lon, "\n", 
       paste(name.list[1:min(10,length(name.list))], collapse=", "),  over.ten))	
  } 

  # check for species expected by FIA which the PFTs don't cover
  fia.ind <- which(!fia.species %in% pfts$spcd)	
  fia.only <- fia.species[fia.ind]						

  if(length(fia.only) > 0){									
    over.ten <- ifelse(length(fia.only) > 30, paste(", and ", length(fia.only) - 30, " more.", sep=""), ".")
  
    if(!exists("symbol.table")){
      symbol.table <- db.query('SELECT spcd, "Symbol" FROM species where spcd IS NOT NULL', con=con)
      names(symbol.table) = tolower(names(symbol.table))
    }
    name.list <- na.omit(symbol.table$symbol[symbol.table$spcd %in% fia.only])
    name.list <- name.list[name.list != "DEAD"]
    if(length(name.list) > 0) {
      logger.error(paste0("\nThe FIA database expects the following species at ", lat," and ", lon, 
        " but they are not described by the selected PFTs: \n", 
        paste(name.list[1:min(30,length(name.list))], collapse=", "), over.ten, "\n\tPlease select additional pfts.")) 
      stop("Execution stopped due to insufficient PFTs.")
    }
  }


  # --- Continue work formatting css now that we've checked for species problems
  n.cohort = nrow(css)
  if(n.cohort == 0)
    logger.warn("No trees found while trying to generate .css from FIA data!")
  
  css$time[is.na(css$time)] <- 1
  css$cohort[is.na(css$cohort)] <- 1:sum(is.na(css$cohort))
  css$dbh[is.na(css$dbh)] <- 1	# assign nominal small dbh to missing
  density.median <- median(css$n[which(css$n > 0)])
  css$n[is.na(css$n)] <- density.median
  css$hite <- css$bdead <- css$balive <- css$lai <- rep(0, n.cohort)
 
  ## map spcd to pft
  css <- merge(css,pfts,by="spcd")
  css <- css[, c('time', 'patch', 'cohort', 'dbh', 'hite', 'pft', 'n', 'bdead', 'balive', 'lai')]

  pfts.represented <- sapply(settings$pfts, function(x) x$constants$num) %in% css$pft
  if(!all(pfts.represented))
    logger.warn(paste0("\nThe following PFTs listed in settings are not represented in the FIA data: ", 
       paste(sapply(settings$pfts, function(x) x$name)[!pfts.represented], collapse=", ")))	



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
  out.dir.local <- file.path(settings$database$dbfiles, "fia")
  prefix.psscss <- paste0("siteid", settings$run$site$id, ".radius", gridres, 
                          get.ed.file.latlon.text(lat, lon, site.style=FALSE))
  prefix.site   <- paste0("siteid", settings$run$site$id, ".radius", gridres, 
                          get.ed.file.latlon.text(lat, lon, site.style=TRUE))
  pss.file.local <- file.path(out.dir.local, paste0(prefix.psscss, ".pss"))
  css.file.local <- file.path(out.dir.local, paste0(prefix.psscss, ".css"))
  site.file.local <- file.path(out.dir.local, paste0(prefix.site, ".site"))

  dir.create(out.dir.local, showWarnings=F, recursive=T)
  write.table(pss, pss.file.local, quote=FALSE, row.names=FALSE)
  write.table(css, css.file.local, quote=FALSE, row.names=FALSE)
  
  site.file.con <- file(site.file.local)
  writeLines(site, site.file.con)
  close(site.file.con)

  
  # Copy to remote if needed
  if(settings$host$name != "localhost") {
    out.dir.remote <- file.path(settings$host$dbfiles, "fia")
    pss.file.remote <- file.path(out.dir.remote, paste0(prefix.psscss, ".pss"))
    css.file.remote <- file.path(out.dir.remote, paste0(prefix.psscss, ".css"))
    site.file.remote <- file.path(out.dir.remote, paste0(prefix.site, ".site"))

    remote.execute.cmd(settings$host, "mkdir", c("-p", out.dir.remote))
    remote.copy.to(settings$host, pss.file.local, pss.file.remote)
    remote.copy.to(settings$host, css.file.local, css.file.remote)
    remote.copy.to(settings$host, site.file.local, site.file.remote)
  }

  # Insert into DB  
  files <- c(pss.file.local, css.file.local, site.file.local)
  formatnames <- c("ED2.patch", "ED2.cohort", "ED2.site")
  for(i in seq_along(files)) {
    dbfile.input.insert(
      in.path    = dirname(files[i]),
      in.prefix  = basename(files[i]),
      siteid     = settings$run$site$id,
      startdate  = format(as.Date(settings$run$start.date), "%Y-%m-%d %H:%M:%S"),
      enddate    = format(as.Date(settings$run$end.date), "%Y-%m-%d %H:%M:%S"),
      mimetype   = 'text/plain',
      formatname = formatnames[i],
      parentid   = NA,
      con        = con,
      hostname   = fqdn()
    )
  }


  # Add file paths to settings
  if(settings$host$name == "localhost") {
  	settings$run$inputs$pss$path <- pss.file.local
  	settings$run$inputs$css$path <- css.file.local
  	settings$run$inputs$site$path <- site.file.local
  } else {
  	settings$run$inputs$pss$path <- pss.file.remote
  	settings$run$inputs$css$path <- css.file.remote
  	settings$run$inputs$site$path <- site.file.remote
  }

  
	## closing the connections to database
	db.close(con)
	db.close(fia.con)
	
	return(settings)
}

# See ed_read_ed10_20_history...
get.ed.file.latlon.text <- function(lat, lon, site.style=FALSE, ed.res=1) {
  if(site.style) {
    lat <- ifelse(lat>=0, ed.res * floor(lat/ed.res) + 0.5 * ed.res, -ed.res * floor(-lat/ed.res) - 0.5 * ed.res)
    lon <- ifelse(lon>=0, ed.res * floor(lon/ed.res) + 0.5 * ed.res, -ed.res * floor(-lon/ed.res) - 0.5 * ed.res)
    return(paste0(".lat", round(lat, 1), "lon", round(lon, 1)))
  } else {
    return(paste0(".lat", round(lat, 4), "lon", round(lon, 4)))  
  }
}