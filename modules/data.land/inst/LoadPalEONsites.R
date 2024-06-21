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

#############

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
    sitename <- db.query(paste0("SELECT sitename from sites where id =",pecan.sgs$site_id[j]),con)
    if(length(grep("PEcAn",sitename))>0){
      sitename <- sub("PEcAn","PalEON",sitename)
      db.query(paste0("UPDATE sites set sitename = '",sitename,"' where id =",pecan.sgs$site_id[j]),con)
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

#####################################
##  Set up to run extract.nc on cluster
for(i in seq_along(paleon.sitegroups)){
  
  print(paste("************",paleon.sitegroups[i],"*************"))
  pecan.sitegroup <- db.query(paste0("SELECT * from sitegroups where name = 'PalEON_",paleon.sitegroups[i],"'"),con)
  pecan.sgs <- db.query(paste("SELECT * from sitegroups_sites where sitegroup_id =",pecan.sitegroup$id),con)
  
  ## loop over sites
  site.info <- NULL
  
  for(j in seq_len(nrow(pecan.sgs))){
    site_id <- as.numeric(pecan.sgs$site_id[j])
    sitename <- db.query(paste0("SELECT sitename from sites where id =",site_id),con)
    str_ns <- paste0(site_id %/% 1e+09, "-", site_id %% 1e+09)
    outfile <- paste0("PalEONregional_CF_site_", str_ns)
    latlon <- PEcAn.DB::query.site(site$id, con = con)[c("lat", "lon")] 
    site.info <- rbind(site.info,data.frame(id = site_id, 
                           lat = latlon$lat, 
                           lon = latlon$lon,
                           str_ns = str_ns,
                           outfile = outfile))
  }    
  save(site.info,file=paste0("PalEON_siteInfo_",paleon.sitegroups[i],".RData"))
}


#####################################
##  Pull extracted met back from cluster
##  and insert in database.
##  Create list of error sites

## establish remote tunnel
library(getPass)
host <- list(name="geo.bu.edu",tunnel="~/.pecan/tunnel/")
is.open <- PEcAn.remote::open_tunnel(host$name,host$tunnel)
if(!is.open){
  print("Could not open remote tunnel")
} else {
  host$tunnel <- file.path(host$tunnel,"tunnel")
}

## get db entry of parent met
parent.input <- db.query("SELECT * from inputs where id = 1000011261",con)

local.prefix <- "/fs/data1/pecan.data/dbfiles/PalEONregional_CF_site_"
remote.prefix <- "/projectnb/dietzelab/pecan.data/input/PalEONregional_CF_site_"
## remote parent: /projectnb/dietzelab/pecan.data/input/PalEON_Regional_nc/
## local parent: /fs/data4/PalEON_Regional_nc
start_date <- lubridate::force_tz(lubridate::as_date("0850-01-01 00:00:00"), 'UTC')
end_date <- lubridate::force_tz(lubridate::as_date("2010-12-31 23:59:59"), 'UTC')
years <- 850:2010
format_id <- 33


paleon.site.errors <- list()
for(i in c(1:5,7)){
  paleon.site.errors[[i]] <- list()
  
  print(paste("************",paleon.sitegroups[i],"*************"))
  pecan.sitegroup <- db.query(paste0("SELECT * from sitegroups where name = 'PalEON_",paleon.sitegroups[i],"'"),con)
  pecan.sgs <- db.query(paste("SELECT * from sitegroups_sites where sitegroup_id =",pecan.sitegroup$id),con)
  
  ## load site info
  load(paste0("PalEON_siteInfo_",paleon.sitegroups[i],".RData"))
  
  
  for(j in seq_len(nrow(pecan.sgs))){
    print(c(i,j))
    
    ## make local folder
    local.dir <- paste0(local.prefix,site.info$str_ns[j],"/")
    remote.dir <- paste0(remote.prefix,site.info$str_ns[j],"/")
    dir.create(local.dir)
    
    ## copy from remote to local
    PEcAn.utils::remote.copy.from(host,remote.dir,local.dir)
    #rsync -avz -e 'ssh -o ControlPath="~/.pecan/tunnel/tunnel"' geo.bu.edu:/projectnb/dietzelab/pecan.data/input/PalEONregional_CF_site_1-24043/ /fs/data1/pecan.data/dbfiles/PalEONregional_CF_site_1-24043/
    
    ## check if all files exist
    local.files <- dir(local.dir,".nc")
    local.years <- as.numeric(sub(".nc","",local.files,fixed = TRUE))
    if(all(years %in% local.years)){
      ## check for input
      input.check <- db.query(paste0("SELECT * FROM inputs where site_id = ",site.info$id[j]," AND format_id = 33"),con)
      if(nrow(input.check) == 0 | length(grep("PalEON",input.check$name))==0){
        ## create new input
        cmd <- paste0("INSERT INTO inputs (site_id, format_id, start_date, end_date, name, parent_id) VALUES (",
                    site.info$id[j], ", ", format_id, ", '",start_date, "', '", end_date,"','", site.info$outfile[j], "',",parent.input$id,") RETURNING id")
        site.input.id <- db.query(cmd, con)
      } else {
        ## use existing input
        sel <- grep("PalEON",input.check$name)
        site.input.id <- input.check$id[sel]
      }
      
      ## create remote dbfile
      dbfile.insert(in.path = remote.dir,in.prefix = "",type = "Input",id = site.input.id,
                    con = con,hostname = host$name)
      
      ## create local dbfile
      dbfile.insert(in.path = local.dir,in.prefix = "",type = "Input",id = site.input.id, con = con)
    } else {
      ## else add to error list 
      k = length(paleon.site.errors[[i]]) + 1
      paleon.site.errors[[i]] <- rbind(paleon.site.errors[[i]],cbind(site.info,min(local.years),max(local.years)))
      save(paleon.site.errors,file="PalEON_siteInfo_errors.RData")
    }
    
  }   
  save(paleon.site.errors,file="PalEON_siteInfo_errors.RData")
  
}

PEcAn.remote::kill.tunnel(list(host=host))

##################################
### merge in CO2 into met data

merge.file <- "~/paleon/paleon_monthly_co2.nc"
start_date <- "0850-01-01"
end_date   <- "2010-12-31"
local.prefix <- "/fs/data1/pecan.data/dbfiles/PalEONregional_CF_site_"

for(i in seq_along(paleon.sitegroups)){
  
  print(paste("************",paleon.sitegroups[i],"*************"))
  pecan.sitegroup <- db.query(paste0("SELECT * from sitegroups where name = 'PalEON_",paleon.sitegroups[i],"'"),con)
  pecan.sgs <- db.query(paste("SELECT * from sitegroups_sites where sitegroup_id =",pecan.sitegroup$id),con)
  load(paste0("PalEON_siteInfo_",paleon.sitegroups[i],".RData"))
  
  for(j in seq_len(nrow(pecan.sgs))){
    print(c(i,j))
    
    ## local folder
    local.dir <- paste0(local.prefix,site.info$str_ns[j],"/")
    if(!file.exists(local.dir) | length(dir(local.dir))==0) next
      
    merge_met_variable(local.dir,in.prefix,start_date,end_date,merge.file)
  }
}

## NOTE: don't forget to delete existing model met data before rerunning models


#######  RENUMBER MET FILE YEARS
## need to have files numbered 0850.nc not 850.nc for JULES
## moving forward, has been fixed in extract.nc
start_year <- 850
end_year   <- 999
in.prefix <- ""
local.prefix <- "/fs/data1/pecan.data/dbfiles/PalEONregional_CF_site_"
for(i in seq_along(paleon.sitegroups)){
  
  print(paste("************",paleon.sitegroups[i],"*************"))
  pecan.sitegroup <- db.query(paste0("SELECT * from sitegroups where name = 'PalEON_",paleon.sitegroups[i],"'"),con)
  pecan.sgs <- db.query(paste("SELECT * from sitegroups_sites where sitegroup_id =",pecan.sitegroup$id),con)
  load(paste0("PalEON_siteInfo_",paleon.sitegroups[i],".RData"))
  
  for(j in seq_len(nrow(pecan.sgs))){
    print(c(i,j))
    
    ## local folder
    local.dir <- paste0(local.prefix,site.info$str_ns[j],"/")
    if(!file.exists(local.dir) | length(dir(local.dir))==0) next

    for (year in start_year:end_year) {
      year_txt <- formatC(year, width = 4, format = "d", flag = "0")
      infile <- file.path(local.dir, paste0(in.prefix, year, ".nc"))
      outfile <- file.path(local.dir, paste0(in.prefix, year_txt, ".nc"))
      if(file.exists(infile)) file.rename(infile,outfile)
    }  
    for(year in -150:849){
      ## remove symbolic links
      year_txt <- formatC(year, width = 4, format = "d", flag = "0")
      infile <- file.path(local.dir, paste0(in.prefix, year, ".nc"))
      system2("rm",infile)
    }
  }
}

### Add eastward_wind and northward_wind
## need to go back to met2CF.PalEONregional and fix
start_date <- "0850-01-01"
end_date   <- "2010-12-31"
local.prefix <- "/fs/data1/pecan.data/dbfiles/PalEONregional_CF_site_"
in.prefix <- ""
for(i in seq_along(paleon.sitegroups)){
  
  print(paste("************",paleon.sitegroups[i],"*************"))
  pecan.sitegroup <- db.query(paste0("SELECT * from sitegroups where name = 'PalEON_",paleon.sitegroups[i],"'"),con)
  pecan.sgs <- db.query(paste("SELECT * from sitegroups_sites where sitegroup_id =",pecan.sitegroup$id),con)
  load(paste0("PalEON_siteInfo_",paleon.sitegroups[i],".RData"))
  
  for(j in seq_len(nrow(pecan.sgs))){
    print(c(i,j))
    
    ## local folder
    local.dir <- paste0(local.prefix,site.info$str_ns[j],"/")
    if(!file.exists(local.dir) | length(dir(local.dir))==0) next
    
    PEcAn.data.atmosphere::split_wind(local.dir,in.prefix,start_date,end_date)
  }
}






