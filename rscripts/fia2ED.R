# make sure indeces exist
# CREATE INDEX spcd ON bety.species (spcd);
# CREATE INDEX tree_spcd on fia5data.TREE (SPCD);
# CREATE INDEX tree_plt_cn on fia5data.TREE (PLT_CN);
# CREATE INDEX plot_cn on fia5data.PLOT (CN);

library(XML)

# ----------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------
#settings.file <- Sys.getenv('PECANSETTINGS')
settings.file = list.files(pattern=".xml")
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)

### R code for generating ED2 "restart" files from FIA database

## SETTINGS

## database info
login   <- "mdietze"	#"fia5data_user"
passwd  <- "goillini"	#"v3tXaJ8ACx"
FIADB   <- "fia5data"

## file info
path    <- settings$run$site$psscss		# output path & prefix

## spatial info
POI	    <- TRUE	 ## point or region?
gridres	<- 0.1
lat     <- as.numeric(settings$run$site$lat)
lon     <- as.numeric(settings$run$site$lon)

## time info
year    <- as.numeric(format(as.Date(settings$run$start.date), '%Y'))

## SOILS
soil = c(1.0,5.0,5.0,0.01,0.0,1.0,1.0) #soil C & N pools (biogeochem) defaults (fsc,stsc,stsl,ssc,psc,msn,fsn)

##FUNCTIONS
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


## connect to database
library(RMySQL)
dvr <- dbDriver("MySQL")
con <- dbConnect(dvr,dbname=FIADB,username=login,password=passwd,host="localhost")

### collect mapping from spcd to pftid
query <- NULL
for (pft in settings$pfts) {
	if (is.null(query)) {
		query <- paste("SELECT bp.name as pft, bs.spcd FROM bety.pfts as bp INNER JOIN bety.pfts_species AS bps ON bps.pft_id = bp.id INNER JOIN bety.species AS bs ON bs.id = bps.specie_id WHERE bp.name = '", pft$name, "'", sep='')
	} else {
		query <- paste(query, " OR bp.name = '", pft$name, "'", sep='')
	}
}
q <- dbSendQuery(con,query)
pfts <- fetch(q, n=-1)
ign <- dbClearResult(q)
for (pft in settings$pfts) {
	pfts[pfts==pft$name] <- pft$constants$num
}

### check for duplicate spcd
bad <- nrow(subset(pfts, spcd %in% c(NA, "0")))
if (bad > 0) {
	print(sprintf("WARNING : There are %d entries with no SPCD (NA or 0)", bad))
	pfts <- subset(pfts, !(spcd %in% c(NA, "0")))
}

bad <- pfts$spcd[duplicated(pfts$spcd)]
if (length(bad) > 0) {
	print(sprintf("ERROR	 : There are %d spcd entries that are duplicated", nrows(bad)))
	print(bad)
}

### select just most current
q <- dbSendQuery(con,statement="SELECT invyr, statecd, stateab, statenm, cycle, subcycle from SURVEY")
surv <- fetch(q,n=-1)
ign <- dbClearResult(q)

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
	q <- dbSendQuery(con,paste("SELECT p.cycle,p.statecd,p.measyear as time,p.cn as patch,MIN(2-c.stdorgcd) as trk,AVG(c.stdage) as age,p.lat,p.lon FROM PLOT as p LEFT JOIN COND as c on p.cn=c.plt_cn WHERE p.lon >= ",lonmin[r]," and p.lon < ",lonmax[r]," and p.lat >= ",latmin[r]," and p.lat < ",latmax[r]," GROUP BY p.cn"))
	pss <- fetch(q,n=-1)
	pss <- pss[pss$cycle == cycle[pss$statecd],]
	ign <- dbClearResult(q)
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
	## query to get CSS info
	q <- dbSendQuery(con,paste("SELECT p.measyear as time,p.cycle,p.statecd,p.cn as patch,CONCAT(CAST(t.subp AS CHAR),CAST(t.tree AS CHAR)) as cohort,t.dia*2.54 as dbh, t.spcd as spcd, t.tpa_unadj*0.0002471 as n FROM fia5data.PLOT as p LEFT JOIN fia5data.TREE as t on p.cn=t.plt_cn WHERE p.lon >= ",lonmin[r]," and p.lon < ",lonmax[r]," and p.lat >= ",latmin[r]," and p.lat < ",latmax[r],sep=''))
	css <- fetch(q,n=-1)
	css <- css[css$cycle == cycle[css$statecd],]
	ign <- dbClearResult(q)
	
	## fill in missing data
	notree <- which(apply(is.na(css[,6:8]),1,sum) == 3)
	if (length(notree) > 0){
		css <- css[-notree,]
	}

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

