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

fia.database <- "fia5data"

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
	con <-  query.base.con(settings)
	
	### collect mapping from spcd to pftid
	query <- NULL
	for (pft in settings$pfts) {
		if (is.null(query)) {
			query <- paste("SELECT bp.name as pft, bs.spcd FROM bety.pfts as bp INNER JOIN bety.pfts_species AS bps ON bps.pft_id = bp.id INNER JOIN bety.species AS bs ON bs.id = bps.specie_id WHERE bp.name = '", pft$name, "'", sep='')
		} else {
			query <- paste(query, " OR bp.name = '", pft$name, "'", sep='')
		}
	}
	pfts <- query.base(query, con)
	
	for (pft in settings$pfts) {
		pfts[pfts==pft$name] <- pft$constants$num
	}
	
	
	## Check for NA and duplicate spcds
	bad <- length(pfts$spcd %in% c(NA, "0"))		
	if (bad > 0) {	
		logger.warn(sprintf("There are %d entries with no SPCD (NA or 0). They have been removed.", bad))
		pfts <- pfts[!pfts$spcd %in% c(NA, 0), ]	   
	}
	
	bad <- pfts$spcd[duplicated(pfts$spcd)]
	if (length(bad) > 0) {
		over.ten = "."										#if num of bad species is large, we'll only print 10
		if(length(bad) > 10)					
			over.ten = sprintf(", and %d more. ", length(bad) - 10)
			
		#Coerce spcds back into species names using data from FIA manual. Makes a more readable warning.
		name.table <- read.csv("modules/data.land/inst/Tests/species_names.csv", header=TRUE)
		name.list <- name.table[name.table$spcd %in% bad,]
		logger.error(paste("The following species are found in multiple PFTs: \n", paste(na.omit(name.list[["name"]][1:10]), collapse=", "), over.ten, "\n\tPlease remove overlapping PFTs.", sep=""))
		stop("Execution stopped due to duplicate species.")					#Using stop naturally causes an error with the tests - put this line in when everything works.
	}
	
	### select just most current
	query <- paste("SELECT invyr, statecd, stateab, statenm, cycle, subcycle from ", fia.database , ".SURVEY", sep="")
	surv <- query.base(query, con)
	
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
	
	## Query to get CSS info now for FIA species so we only query once
	query <- paste("SELECT p.measyear as time,p.cycle,p.statecd,p.cn as patch, CONCAT(CAST(t.subp AS CHAR),CAST(t.tree AS CHAR)) as cohort,t.dia*2.54 as dbh, t.spcd as spcd, t.tpa_unadj*0.0002471 as n FROM ",
			fia.database, ".PLOT as p LEFT JOIN ",fia.database, ".TREE as t on p.cn=t.plt_cn WHERE p.lon >= ",lonmin[1]," and p.lon < ",lonmax[1],
			" and p.lat >= ",latmin[1]," and p.lat < ",latmax[1],sep='')
	css <- query.base(query, con)
	css <- css[css$cycle == cycle[css$statecd],]
	
	## fill in missing data
	notree <- which(apply(is.na(css[,6:8]),1,sum) == 3)
	if (length(notree) > 0){
		css <- css[-notree,]
	}
	
	## Ensure consistency between PFTs and FIA db
	#############################################
	fia.species <-unique(css$spcd)
	
	## check for species in PFTs which the FIA db doesn't expect
	pft.ind <- which(!pfts$spcd %in% fia.species)			#vect shows pft's spcds that are confirmed by fia
	pft.only <- pfts$spcd[pft.ind]							#what were the spcds at those indices? 
	
	if(length(pft.only) > 0){								
		over.ten = "."										#if num of bad species is large, we'll only print 10
		if(length(pft.only) > 10)					
			over.ten = sprintf(", and %d more. ", length(pft.only) - 10)
		
		if(!exists("name.table"))							# Don't load table again if already loaded
			name.table <- read.csv("modules/data.land/inst/Tests/species_names.csv", header=TRUE)
		name.list <- name.table[name.table$spcd %in% pft.only,] 			#df of records (rows) from name.table df with the spcds we're looking for - grabs their names below
		logger.warn(paste("The selected pfts contain the following species for which the FIA database contains no data at this site: \n", paste(na.omit(name.list[["name"]][1:10]), collapse=", "), over.ten, "\n\tThese will be populated with zero values in the output.", sep=""))	
	}
	
	## check for species expected by FIA which the PFTs don't cover
	fia.ind <- which(!fia.species %in% pfts$spcd)	
	fia.only <- fia.species[fia.ind]						
	
	if(length(fia.only) > 0){									
		over.ten = "."
		if(length(fia.only) > 10)							#print 10
			over.ten = sprintf(", and %d more. ", length(fia.only) - 10)
		
		if(!exists("name.table"))							
			name.table <- read.csv("modules/data.land/inst/Tests/species_names.csv", header=TRUE)
		name.list <- name.table[name.table$spcd %in% fia.only,] 
		logger.error(paste("The FIA database expects the following species in this site, but they are not described by the selected pfts: \n", 
					paste(na.omit(name.list[["name"]][1:10]), collapse=", "), over.ten, "\n\tPlease select additional pfts.", sep="")) 
		stop("Execution stopped due to insufficient PFTs.")
	}

	
	for(r in 1:n.poi) {
		##################
		##              ##
		##     PSS      ##
		##              ##
		##################
		## query to get PSS info
		query <- paste("SELECT p.cycle,p.statecd,p.measyear as time,p.cn as patch,MIN(2-c.stdorgcd) as trk,AVG(c.stdage) as age,p.lat,p.lon FROM ",
				fia.database, ".PLOT as p LEFT JOIN ", fia.database, ".COND as c on p.cn=c.plt_cn WHERE p.lon >= ",lonmin[r]," and p.lon < ",lonmax[r],
				" and p.lat >= ",latmin[r]," and p.lat < ",latmax[r]," GROUP BY p.cn")
		pss <- query.base(query, con)
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
				fname <- paste(path,"lat",(x+0.5)*gridres+latmin[r],"lon",(y+0.5)*gridres+lonmin[r],".pss",sep="") #filename
				water = rep(0,length(sel))			
				write.table(cbind(pss[sel,2+1:4],area[sel],water,matrix(soil,length(sel),7,byrow=TRUE)),file=fname,quote=FALSE,row.names=FALSE)
			}
		}
		
		##################
		##              ##
		##     CSS      ##
		##              ##
		##################
#		## query to get CSS info  **THIS IS DONE ABOVE NOW - less execution wasted in case selected PFTs are poor for the site
#		query <- paste("SELECT p.measyear as time,p.cycle,p.statecd,p.cn as patch, CONCAT(CAST(t.subp AS CHAR),CAST(t.tree AS CHAR)) as cohort,t.dia*2.54 as dbh, t.spcd as spcd, t.tpa_unadj*0.0002471 as n FROM ",
#				fia.database, ".PLOT as p LEFT JOIN ",fia.database, ".TREE as t on p.cn=t.plt_cn WHERE p.lon >= ",lonmin[r]," and p.lon < ",lonmax[r],
#				" and p.lat >= ",latmin[r]," and p.lat < ",latmax[r],sep='')
#		css <- query.base(query, con)
#		css <- css[css$cycle == cycle[css$statecd],]
#		
#		## fill in missing data
#		notree <- which(apply(is.na(css[,6:8]),1,sum) == 3)
#		if (length(notree) > 0){
#			css <- css[-notree,]
#		}
		
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
				cssfile <- file(paste(path,"lat",(y+0.5)*gridres+latmin[r],"lon",(x+0.5)*gridres+lonmin[r],".css",sep=""), "w")
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
}
