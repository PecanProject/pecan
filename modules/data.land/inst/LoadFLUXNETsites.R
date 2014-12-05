## Script to load AmeriFLUX and FLUXNET sites into the BETY database
## Mike Dietze
##
## - expected behaviour is to ignore existing records where the FLUXNET code 
## is in the site.sitename
## - not all site attributes are available
## - additional info (e.g. IGBP code) is put in notes

user.id = 1000000001

library("RCurl")
library("XML")

nu <- function(x){as.numeric(as.character(x))}  ## simple function to convert data to numeric


############### OPEN PSQL PORT ON VM  #######
#
# This was tested by running the code on my laptop and writing the entries 
# to my local VM. Here's how I opened that port.
#
# 0) port forwarding: open 5432 as 3232
#
# 1) sudo nano /etc/postgresql/9.3/main/postgresql.con
#     listen_addresses = '*'
#
# 2) sudo nano /etc/postgresql/9.3/main/pg_hba.conf
#  added:
## IPv4 local connections:
#host    all         all         127.0.0.1/32          md5
#host    all         all         192.168.1.0/24        md5
#host    all         all         10.0.2.2/32        md5
#
# 3) sudo service postgresql restart

site.map <- data.frame(FLUX.id=rep(NA,2000),site.id=rep(NA,2000))

###########  AMERIFLUX

# Turns out the AMERIFLUX website is dynamics so the following code doesn't work
# One currently has to download the html from the browser by hand
# and then work with the copy below
#AMERIFLUX_html <- getURL("http://ameriflux.lbl.gov/sites/site-list-and-pages/")  ## grab raw html
#AMERIFLUX_table = readHTMLTable(AMERIFLUX_html)[[1]]    ##grab first table on the webpage

## GET LIST OF AMERIFLUX SITES
AMERIFLUX_html <- getURL("file:///Users/mdietze/Downloads/List of AmeriFlux Sites.html")
AMERIFLUX_table = readHTMLTable(AMERIFLUX_html)[[1]]    ##grab first table on the webpage


###################   PROCESS DOWNLOADED FILE ######################
## open database connection
driver   <- "PostgreSQL"
user     <- "bety"
dbname   <- "bety"
password <- "bety"
host     <- "localhost"
port     <- "3232"
dbparms  <- list(driver=driver, user=user, dbname=dbname, password=password, host=host,port=port)
con      <- db.open(dbparms)

## GET LIST OF PEcAn site names
pecan.sites = db.query("SELECT * from sites",con)

nsite = nrow(AMERIFLUX_table)
for(s in 1:nsite){
    
  ## check whether site exists in BETYdb
  code = as.character(AMERIFLUX_table$SITE_ID[s])
  id = grep(code,pecan.sites$sitename)
  site.id = pecan.sites$id[id]
  
  site.map[which(is.na(site.map[,1]))[1],] <- c(code,site.id)
  
  
  print(c(s,code,id))
  
  ## if site does not exist, insert a new site
  if(length(id) == 0){
    longname = as.character(AMERIFLUX_table$SITE_NAME[s])
      longname = sub("'","",longname) # drop single quotes from name
    sitename = paste0(longname," (",code,")")
    country = substr(code,1,2)
    mat = nu(AMERIFLUX_table$MAT[s])
      if(is.na(mat)) mat = "NULL"
    map = as.integer(nu(AMERIFLUX_table$MAP[s]))
      if(is.na(map)) map = "NULL"
    lat = nu(AMERIFLUX_table$LOCATION_LAT[s])
    lon = nu(AMERIFLUX_table$LOCATION_LONG[s])
    elev = nu(AMERIFLUX_table$LOCATION_ELEV[s])
      if(is.na(elev)) elev = 0
    notes = paste("IGBP =",as.character(AMERIFLUX_table$IGBP[s]),
                  " CLIMATE_KOEPPEN =",as.character(AMERIFLUX_table$CLIMATE_KOEPPEN[s]),
                  " TOWER_BEGAN =",as.character(AMERIFLUX_table$TOWER_BEGAN[s]),
                  " TOWER_END =",as.character(AMERIFLUX_table$TOWER_END[s])
                  )
    InsertString = paste0("INSERT INTO sites(sitename,country,mat,map,notes,geometry,user_id,created_at,updated_at) VALUES(",
                          "'",sitename,"', ",
                          "'",country,"', ",
                          mat,", ",
                          map,", ",
                          "'",notes,"', ",
                          "ST_GeomFromText('POINT(",lon," ",lat," ",elev,")', 4326), ",
                          user.id,
                          ", NOW(), NOW() );")
    db.query(InsertString,con)        
  }
    
}


#  FLUXNET ----------------------------------------------------------------

FLUXNET_html <- getURL("http://fluxnet.ornl.gov/site_status")  ## grab raw html
FLUXNET_table = readHTMLTable(FLUXNET_html)[[1]]    ##grab first table on the webpage
FLUXNET_siteURL = "http://fluxnet.ornl.gov/site/"

## preprocess raw html
raw.rows = strsplit(FLUXNET_html,"<tr class")[[1]]

## GET UPDATED LIST OF PEcAn site names
pecan.sites = db.query("SELECT * from sites",con)

nsite = nrow(FLUXNET_table)
for(s in 1:nsite){
  
  ## check whether site exists in BETYdb
  code = as.character(FLUXNET_table[s,"FLUXNET ID"])
  id = grep(code,pecan.sites$sitename)
  site.id = pecan.sites$id[id]
  
  site.map[which(is.na(site.map[,1]))[1],] <- c(code,site.id)  
  
  print(c(s,code,id))
  
  ## if site does not exist, insert a new site
  if(length(id) == 0){

    longname = as.character(FLUXNET_table[s,"Site Name"])
      longname = gsub("'","",longname) # drop single quotes from name
    sitename = paste0(longname," (",code,")")
    
    raw = raw.rows[grep(code,raw.rows)]
    raw.col = strsplit(raw,"<td")[[1]]
    raw.site = raw.col[grep("site",raw.col)]
    raw.site = strsplit(raw.site,">")[[1]]
    raw.site = raw.site[grep("href",raw.site)]
    raw.site = sub("<a href=\"site/","",raw.site)
    col.id = as.numeric(sub("\"","",raw.site))
    
    site_html <- getURL(paste0(FLUXNET_siteURL,col.id))
    site_table <- readHTMLTable(site_html)
    
    description = as.character(site_table[[1]][2,2])

    country = as.character(site_table[[2]][1,2])
    loc = strsplit(as.character(site_table[[2]][2,2]),",")[[1]]
    lat = as.numeric(loc[1])
    lon = as.numeric(loc[2])
    
    PI = as.character(site_table[[3]][1,2])
    
    y = sapply(site_table,function(x){
      if(length(dim(x))==2){
        y=apply(x,2,as.character)
      }else{
        y = as.character(x)
      }
      return(length(grep("Elevation",paste(y)))>0)
    })
    y[1] = FALSE # if "Elevation" is in the description, skip
    if(sum(y)>0){
      site.tab = which(as.logical(y))[1]
      site.char = apply(site_table[[site.tab]],2,as.character)
      site.char = paste(site.char[,1],site.char[,2],collapse="; ")
      erow = grep("Elevation",as.character(site_table[[site.tab]][,1]))
      elev = as.numeric(sub("m","",as.character(site_table[[site.tab]][erow,2])))
    } else{
      elev = -9999
      site.char = NULL
    }
    
    notes = paste0("PI: ",PI,"; ",site.char,"; FLUXNET DESCRIPTION: ",description)
    notes = gsub("'","",notes) # drop single quotes from notes
    
    InsertString = paste0("INSERT INTO sites(sitename,country,notes,geometry,user_id,created_at,updated_at) VALUES(",
                          "'",sitename,"', ",
                          "'",country,"', ",
                          "'",notes,"', ",
                          "ST_GeomFromText('POINT(",lon," ",lat," ",elev,")', 4326), ",
                          user.id,
                          ", NOW(), NOW() );")
    db.query(InsertString,con)
    
  } ## end IF new site
  
}  ## end loop over sites

db.close(con)

## remove duplicates & save
site.map = na.omit(site.map)
site.map = unique(site.map)
write.csv(site.map,"FLUXNET.sitemap.csv")

