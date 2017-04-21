## Script to load PalEON sites into the BETY database
## Mike Dietze
##

user.id = 1000000001  ## replace with your own personal user.id

library("RCurl")
library("XML")
library(PEcAn.DB)

nu <- function(x){as.numeric(as.character(x))}  ## simple function to convert data to numeric

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
    code <- paste0("PalEON_",group$num[j])
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

### SCRIPT used to manually run met2CF.PalEONregional
### Assumes met has been manually downloaded

in.path <- "/fs/data4/PalEON_Regional"
in.prefix <- ""
outfolder <- paste0(in.path,"_nc")
start_date <- "850-01-01"
end_date <- "2010-12-31"
overwrite <- FALSE
verbose <- FALSE

## After create site and Input records
citation_id <- 1000000012 # Kumar et al 2012

#INSERT INTO sites (sitename,user_id,geometry) VALUES ('PalEON Regional',1000000001,ST_Geomfromtext('POLYGON((-100.0 35 0, -100 50 0, -60 50 0, -60 35 0, -100 35 0))', 4326)) RETURNING id;
site_id <- 1000025661

#INSERT INTO inputs (site_id,start_date,end_date,name,user_id,format_id) VALUES (1000025661,'850-01-01 00:00:00','2010-12-31 23:59:59','PalEON Regional Met',1000000001,33) RETURNING id;
input_id <- 1000011261

################################

## Recode PalEON site names
for(i in seq_along(paleon.sitegroups)){
  
  print(paste("************",paleon.sitegroups[i],"*************"))
  pecan.sitegroup <- db.query(paste0("SELECT * from sitegroups where name = 'PalEON_",paleon.sitegroups[i],"'"),con)
  pecan.sgs <- db.query(paste("SELECT * from sitegroups_sites where sitegroup_id =",pecan.sitegroup$id),con)
  
  ## loop over new sites
  for(j in seq_len(nrow(pecan.sgs))){
    sitename <- db.query(paste0("SELECT sitename from sites where id =",pecan.sgs$site_id),con)
    if(length(grep("PEcAn",sitename))){
      sitename <- sub("PEcAn","PalEON",sitename)
      db.query(paste0("UPDATE sites set sitename = '",sitename,"' where id =",pecan.sgs$site_id),con)
    }
  }    
}

### Create Multisettings
for(i in c(1,6,2,3,4)){
  
  print(paste("************",paleon.sitegroups[i],"*************"))
  pecan.sitegroup <- db.query(paste0("SELECT * from sitegroups where name = 'PalEON_",paleon.sitegroups[i],"'"),con)
  
  template <- PEcAn.settings::read.settings("pecan_DALEC_priority.xml")
  template$run$site <- NULL  ## make sure to zero out template site
  
  multiRunSettings <- createSitegroupMultiSettings(template, sitegroupId = pecan.sitegroup$id)

  PEcAn.settings::write.settings(multiRunSettings, outputfile = paste0("pecan.",paleon.sitegroups[i],".xml"))
  
}

## set paths for remote
##geo
#settings$outdir <- "/projectnb/dietzelab/pecan.data/output"
