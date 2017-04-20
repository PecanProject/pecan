## Script to load PalEON sites into the BETY database
## Mike Dietze
##

user.id = 1000000001  ## replace with your own personal user.id

library("RCurl")
library("XML")
library(PEcAn.DB)

nu <- function(x){as.numeric(as.character(x))}  ## simple function to convert data to numeric

site.map <- data.frame(FLUX.id=rep(NA,2000),site.id=rep(NA,2000))

## load up PalEON site files
paleon <- read.csv("~/paleon/Paleon_MIP_Phase2_ED_Order_Status.csv",stringsAsFactors = FALSE)
priority <- read.csv("~/paleon/new.ed.mat.csv")
paleon$notes[paleon$num %in% priority$num] <- "priority"
paleon$notes[1:6] <- "site"  ## mark the original 6 Site MIP grid cells
paleon.sitegroups <- unique(paleon$notes)
  paleon.sitegroups <- paleon.sitegroups[-which(paleon.sitegroups=="site")]
# note: code currently assumes sitegroups were created in BETY with the pattern "PalEON_<name>"
  
  
## open database connection
library(RPostgreSQL)
driver   <- "PostgreSQL"
user     <- "bety"
dbname   <- "bety"
password <- "bety"
host     <- "psql-pecan.bu.edu"
dbparms  <- list(driver=driver, user=user, dbname=dbname, password=password, host=host)
con      <- db.open(dbparms)

for(i in seq_along(paleon.sitegroups)){
  
  print(paste("************",paleon.sitegroups[i],"*************"))
  
  ## subset data
  group <- paleon[which(paleon$notes == paleon.sitegroups[i]),]
  
  ## query existing site group and sites
  pecan.sites = db.query("SELECT * from sites",con)
  pecan.sitegroup <- db.query(paste0("SELECT * from sitegroups where name = 'PalEON_",paleon.sitegroups[i],"'"),con)
  if(nrow(pecan.sitegroup) == 0){
    print("SITEGROUP not found")
    break()
  }
  pecan.sgs <- db.query(paste("SELECT * from sitegroups_sites where sitegroup_id =",pecan.sitegroup$id),con)
  
  ## loop over new sites
  for(j in seq_len(nrow(group))){

    ## detect if site exists or not
    code <- paste0("PEcAn_",group$num[j])
    id = grep(code,pecan.sites$sitename)
    site.id = pecan.sites$id[id]
    print(paste(code,id,site.id))
    
    ## if new site add
    if(length(id) == 0){
      sitename = code
      country = "US"
      lat = nu(group$lat[j])
      lon = nu(group$lon[j])
      elev = 0
      InsertString = paste0("INSERT INTO sites(sitename,country,geometry,user_id) VALUES(",
                            "'",sitename,"', ",
                            "'",country,"', ",
                            "ST_GeomFromText('POINT(",lon," ",lat," ",elev,")', 4326), ",
                            user.id,") returning id ;")
      new.id = db.query(InsertString,con)        
    
      ## add to sitegroup
      if(new.id %in% pecan.sgs$site_id){
        print("SITE already in SITEGROUP")
      } else {
        InsertSite = paste0("INSERT INTO sitegroups_sites(sitegroup_id,site_id) VALUES(",
                        pecan.sitegroup$id,", ",
                        new.id,")")      
        db.query(InsertSite,con) 
      }
      
    } ## end new site insert
    
  } ## end site loop
  
    
} ## end sitegroup loop

db.close(con)


