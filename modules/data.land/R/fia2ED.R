#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# make sure indeces exist
# CREATE INDEX spcd ON species (spcd);
# CREATE INDEX tree_spcd on fia5data.TREE (SPCD);
# CREATE INDEX tree_plt_cn on fia5data.TREE (PLT_CN);
# CREATE INDEX plot_cn on fia5data.PLOT (CN);

library(XML)
library(PEcAn.utils)
library(PEcAn.DB)

#fia.database <- "fia5data"

#--------------------------------------------------------------------------------------------------#
# INTERNAL FUNCTIONS DO NOT EXPORT
#--------------------------------------------------------------------------------------------------#

##' convert x into a table
##'
##' @title table.expand
##' @param x first xml list 
##' @return table
##' @author Mike Dietze
table.expand <- function(x){
	n <- max(x[,1],na.rm=T)
	y <- rep(NA,n)
	for(i in 1:n){
		sel <-which(x[,1]==i)
		if(length(sel)>0)
			y[i] <- x[sel,2]
	}
	return(y)
}

#--------------------------------------------------------------------------------------------------#
# EXTERNAL FUNCTIONS
#--------------------------------------------------------------------------------------------------#

##' convert x into a table
##'
##' @title fia.to.psscss
##' @param create pss/css files based on data in the fia database
##' @return nothing
##' @export
##' @author Mike Dietze, Rob Kooper
fia.to.psscss <- function(settings) {
	## spatial info
	POI	    <- TRUE	 ## point or region?
	gridres	<- 0.1
	lat     <- as.numeric(settings$run$site$lat)
	lon     <- as.numeric(settings$run$site$lon)
	
	## output path
	if ("text" %in% names(settings$model$psscss)) {
		path <- settings$model$psscss$text
	} else {
		path <- settings$model$psscss
	}
	
	## time info
	year    <- as.numeric(format(as.Date(settings$run$start.date), '%Y'))
	
	## SOILS
	soil = c(1.0,5.0,5.0,0.01,0.0,1.0,1.0) #soil C & N pools (biogeochem) defaults (fsc,stsc,stsl,ssc,psc,msn,fsn)	
	
	## connect to database
	con <-  db.open(settings$database$bety)
	
	### collect mapping from spcd to pftid
	query <- NULL
	for (pft in settings$pfts) {
		if (is.null(query)) {
			query <- paste("SELECT bp.name as pft, bs.spcd FROM pfts as bp INNER JOIN pfts_species AS bps ON bps.pft_id = bp.id INNER JOIN species AS bs ON bs.id = bps.specie_id WHERE bp.name = '", pft$name, "'", sep='')
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
		over.ten <- ifelse(length(bad) > 10, paste(", and ", length(bad) - 10, " more.", sep=""), ".")		# format the "and x more." bit if >10 bad species
		
		#Coerce spcds back into species names using data from FIA manual. Makes a more readable warning.
		symbol.table <- db.query('SELECT spcd, "Symbol" FROM species where spcd IS NOT NULL', con=con)
		names(symbol.table) = tolower(names(symbol.table))
		name.list <- na.omit(symbol.table$symbol[symbol.table$spcd %in% bad]) 							# grab the names where we have bad spcds in the symbol.table, exclude NAs
		logger.error(paste("\nThe following species are found in multiple PFTs: \n", paste(name.list[1:min(10,length(name.list))], collapse=", "), over.ten, "\n\tPlease remove overlapping PFTs.", sep=""))
		stop("Execution stopped due to duplicate species.")												#Using stop naturally causes an error with the tests - comment stops out for testing.
	}

	## connect to database
  #fia.db.settings <- settings$database$fia
	fia.con <- db.open(settings$database$fia)
	
  
	### select just most current
	query <- paste('SELECT "INVYR", "STATECD", "STATEAB", "STATENM", "CYCLE", "SUBCYCLE" from "SURVEY"', sep="")
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
	
	if(POI){
		latmax = lat + gridres
		latmin = lat - gridres
		lonmax = lon + gridres
		lonmin = lon - gridres
	} else {
		latmin = min(lat)
		latmax = max(lat)
		lonmin = min(lon)
		lonmax = max(lon)
	}
	n.poi = length(latmin)
	
		
	for(r in 1:n.poi) {
		##################
		##              ##
		##     PSS      ##
		##              ##
		##################
		## query to get PSS info
		query <- paste('SELECT p."CYCLE",p."STATECD",p."MEASYEAR" as time,p."CN" as patch,MIN(2-c."STDORGCD") as trk,AVG(c."STDAGE") as age,p."LAT",p."LON" FROM "PLOT" as p LEFT JOIN "COND" as c on p."CN"=c."PLT_CN" WHERE 
             p."LON" >= ',lonmin[r],' and p."LON" < ',lonmax[r],
			      	' and p."LAT" >= ',latmin[r],' and p."LAT" < ',latmax[r],' GROUP BY p."CN"')
		pss <- db.query(query, con=fia.con)
    names(pss) = tolower(names(pss))
		pss <- pss[pss$cycle == cycle[pss$statecd],]
		if(length(pss) == 0) next
		
		pss$trk[which(is.na(pss$trk))] <- 1
		pss$age[which(is.na(pss$age))] <- 0
		
		grid.bin <- nx <- ny <- 1
		if(POI){
			grid.bin <- rep(1,nrow(pss))
		} else {	
			## assign spatial index
			nx <- (lonmax[r]-lonmin[r])/gridres
			ny <- (latmax[r]-latmin[r])/gridres
			grid.bin <- round((floor(pss$lat/gridres)-latmin[r]/gridres)*nx + floor(pss$lon/gridres)-lonmin[r]/gridres + 1)
		}
		
		## fill missing data w/ defaults
		##area.median <- median(pss$area[which(pss$area > 0)])
		##pss$area[which(is.na(pss$area))] <- area.median
		##pss$area[which(pss$area == 0)] <- area.median
		pss$area = rep(1,nrow(pss))
		
		##normalize area
		area.norm <- tapply(pss$area,grid.bin,sum)
		area.norm <- table.expand(cbind(sort(unique(grid.bin)),as.vector(area.norm)))
		area <- pss$area/area.norm[grid.bin]
		
		##write out pss by location
		for(i in 1:max(grid.bin,na.rm=T)){
			sel <- which(grid.bin == i)
			if(length(sel) > 0){
				y <- floor((i-1)/nx)
				x <- i-1-y*nx
				fname <- paste(path,"lat",lat,"lon",lon,".pss",sep="") #filename
				water = rep(0,length(sel))
				write.table(cbind(pss[sel,2+1:4],area[sel],water,matrix(soil,length(sel),7,byrow=TRUE)),file=fname,quote=FALSE,row.names=FALSE)
			}
		}
		
		##################
		##              ##
		##     CSS      ##
		##              ##
		##################
		
    
    query <- paste('SELECT p."MEASYEAR" as time,p."CYCLE",p."STATECD",p."CN" as patch,CONCAT(CAST(t."SUBP" AS CHAR),CAST(t."TREE" AS CHAR)) as cohort,t."DIA"*2.54 as dbh, t."SPCD" as spcd, t."TPA_UNADJ"*0.0002471 as n FROM "PLOT" as p LEFT JOIN "TREE" as t on p."CN"=t."PLT_CN" WHERE 
             p."LON" >= ',lonmin[r],' and p."LON" < ',lonmax[r],
				' and p."LAT" >= ',latmin[r],' and p."LAT" < ',latmax[r], sep='')

		css <- db.query(query, con=fia.con)
		names(css) = tolower(names(css))
		css <- css[css$cycle == cycle[css$statecd],]
		
		## fill in missing data
		notree <- which(apply(is.na(css[,6:8]),1,sum) == 3)
		if (length(notree) > 0){
			css <- css[-notree,]
		}
		
		
		## Consistency tests between PFTs and FIA begin here - done inside loop in case of multiple areas
		fia.species <-unique(css$spcd)
		
		## check for species in PFTs which the FIA db doesn't expect
		pft.ind <- which(!pfts$spcd %in% fia.species)										#vect shows pft's spcds that are confirmed by fia
		pft.only <- pfts$spcd[pft.ind]														#what were the spcds at those indices? 
		
		if(length(pft.only) > 0){								
			over.ten <- ifelse(length(pft.only) > 10, paste(", and ", length(pft.only) - 10, " more.", sep=""), ".")
			
			if(!exists("symbol.table")){
				symbol.table <- db.query('SELECT spcd, "Symbol" FROM species where spcd IS NOT NULL', con=con)
				names(symbol.table) = tolower(names(symbol.table))
			}
			name.list <- na.omit(symbol.table$symbol[symbol.table$spcd %in% pft.only]) 
			logger.warn(paste("\nThe selected PFTs contain the following species for which the FIA database contains no data at ", lat, " and ", lon, "\n", paste(name.list[1:min(10,length(name.list))], collapse=", "), over.ten, "\n\tThese will be populated with zero values in the output.", sep=""))	
		} 
		
		## check for species expected by FIA which the PFTs don't cover
		fia.ind <- which(!fia.species %in% pfts$spcd)	
		fia.only <- fia.species[fia.ind]						
		
		if(length(fia.only) > 0){									
			over.ten <- ifelse(length(fia.only) > 10, paste(", and ", length(fia.only) - 10, " more.", sep=""), ".")
			
			if(!exists("symbol.table")){
				symbol.table <- db.query('SELECT spcd, "Symbol" FROM species where spcd IS NOT NULL', con=con)
				names(symbol.table) = tolower(names(symbol.table))
			}
			name.list <- na.omit(symbol.table$symbol[symbol.table$spcd %in% fia.only])  
			logger.error(paste("\nThe FIA database expects the following species at ", lat," and ", lon, " but they are not described by the selected PFTs: \n", 
							paste(name.list[1:min(10,length(name.list))], collapse=", "), over.ten, "\n\tPlease select additional pfts.", sep="")) 
			stop("Execution stopped due to insufficient PFTs.")
		}
		
		
		# Continue work formatting css now that we've checked for species problems
		if(nrow(css) > 0){
			css$time[is.na(css$time)] <- 1
			css$cohort[is.na(css$cohort)] <- 1:sum(is.na(css$cohort))
			css$dbh[is.na(css$dbh)] <- 1	# assign nominal small dbh to missing
			density.median <- median(css$n[which(css$n > 0)])
			css$n[is.na(css$n)] <- density.median
		} else {
			## no trees at all at this site
		}
		
		## map spcd to pft
		css <- merge(css,pfts,by="spcd")
		
		## write out
		for(i in 1:max(grid.bin,na.rm=T)){
			sel <- which(grid.bin == i)
			if(length(sel) > 0){
				y <- floor((i-1)/nx)
				x <- i-1-y*nx
				cssfile <- file(paste(path,"lat",lat,"lon",lon,".css",sep=""), "w")
				writeLines("time patch cohort dbh hite pft n bdead balive lai",con=cssfile)
				for(j in sel){
					sel2 <- which(as.character(css$patch) == pss$patch[j])
					hite <- rep(0,length(sel2))
					dtemp <- cbind(css[sel2,c("time","patch","cohort","dbh")],hite,css[sel2,c("pft","n")],matrix(0,length(sel2),3))
					write.table(dtemp,file=cssfile,append=T,row.names=F,col.names=F,quote=F)
				}
				close(cssfile)
				##		write.table(cbind(pss[sel,1:4],area[sel],matrix(0,length(sel),8)),file=fname,quote=F,row.names=FALSE)
			}
		}
	}	## end loop over n.poi
  
	## closing the connections to database
	db.close(con)
	db.close(fia.con)
}
