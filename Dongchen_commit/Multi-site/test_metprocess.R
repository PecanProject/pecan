#DEBUGGING MET PROCESS
#go through met.process
library(PEcAn.all)
library(PEcAn.data.atmosphere)
library(xts)
setwd("/projectnb/dietzelab/dongchen/Multi-site/")
settings <-PEcAn.settings::read.settings("/projectnb/dietzelab/dongchen/Multi-site/pecan.SDA.4sites.xml")
site_1 <- settings[[1]]
site <- site_1$run$site 
input_met <- list(source="ERA5", output="SIPNET")#site_1$run$inputs$met
start_date <- as.Date(paste0(2008,"-01-01"), tz="UTC")
end_date <- as.Date(paste0(2010,"-12-31"), tz="UTC")
model <- site_1$model$type
host <- settings$host
host$folder <- "/projectnb/dietzelab/dongchen/Multi-site/ERA5"
dbparms <- site_1$database$bety
dir <- "/projectnb/dietzelab/dongchen/Multi-site/ERA5/output1"
browndog = NULL
spin=NULL
overwrite = FALSE

#go through .extract.nc.module function
i=1
cf.id = list(input.id = cf.id$container_id[i], dbfile.id = cf.id$id[i])
register = register
dir = dir 
met = met
str_ns = str_ns 
site = site
new.site = new.site 
con = con 
start_date = start_date
end_date = end_date 
host = host
overwrite = overwrite$standardize

#go through PEcAn.utils::convert.input function
input.id = input.id
outfolder = outfolder 
formatname = formatname 
mimetype = mimetype 
site.id = site$id 
start_date = start_date
end_date = end_date
pkg = pkg 
fcn = fcn 
con = con
host = host
browndog = NULL 
write = TRUE 
slat = new.site$lat
slon = new.site$lon
newsite = new.site$id
overwrite = overwrite
exact.dates = FALSE 
forecast = FALSE
ensemble = register$ensemble %>% as.numeric()
#test break points
testfunction <- function(input.id,
                         outfolder,
                         formatname,
                         mimetype,
                         site.id,
                         start_date,
                         end_date,
                         pkg,
                         fcn,
                         con = con,
                         host,
                         browndog,
                         write = TRUE,
                         format.vars,
                         overwrite = FALSE,
                         exact.dates = FALSE,
                         allow.conflicting.dates = TRUE,
                         insert.new.file = FALSE,
                         pattern = NULL,
                         forecast = FALSE,
                         ensemble = FALSE,
                         ensemble_name = NULL,
                         dbparms=NULL,
                         ...){
  input.args <- list(...)
}
input.args <- testfunction(input.id = input.id, 
                           outfolder = outfolder, 
                           formatname = formatname, 
                           mimetype = mimetype, 
                           site.id = site$id, 
                           start_date = start_date,
                           end_date = end_date,
                           pkg = pkg, 
                           fcn = fcn, 
                           con = con, host = host, browndog = NULL, 
                           write = TRUE, 
                           slat = new.site$lat, slon = new.site$lon,
                           newsite = new.site$id, 
                           overwrite = overwrite,
                           exact.dates = FALSE, 
                           ensemble = register$ensemble %>% as.numeric())







###
library(PEcAn.utils)
source("/projectnb/dietzelab/dongchen/dongchenpecan/base/utils/R/convert.input.R")
ready.id <- convert.input(input.id = input.id, 
                                       outfolder = outfolder, 
                                       formatname = formatname, 
                                       mimetype = mimetype, 
                                       site.id = site$id, 
                                       start_date = start_date,
                                       end_date = end_date,
                                       pkg = pkg, 
                                       fcn = fcn, 
                                       con = con, host = host, browndog = NULL, 
                                       write = TRUE, 
                                       slat = new.site$lat, slon = new.site$lon,
                                       newsite = new.site$id, 
                                       overwrite = overwrite,
                                       exact.dates = FALSE, 
                                       ensemble = register$ensemble %>% as.numeric())
remotefunc <- function() {PEcAn.data.atmosphere::extract.nc.ERA5(slat=40.6658, 
                                                                 slon=-77.9041, 
                                                                 newsite=1000025731, 
                                                                 overwrite=FALSE, 
                                                                 in.path='/projectnb/dietzelab/dongchen/Multi-site/ERA5/', 
                                                                 in.prefix='ERA5_', 
                                                                 outfolder='/projectnb/dietzelab/dongchen/Multi-site/ERA5/output1/ERA5_CF_site_1-25731/', 
                                                                 start_date='2008-01-01', 
                                                                 end_date='2010-12-31')}
remotefunc()

###problem
script = cmdFcn
host
user = NA
verbose = TRUE
R = Rbinary
scratchdir = outfolder

system2("/usr/lib/R/bin/R", "--no-save","--no-restore", stdout = verbose, stderr = verbose,
        input = input)

system2("/usr/lib/R/bin/R",input = "R.home()",stdout = TRUE)







.First <- function(){
  #Sys.setenv(R_USER = "~")
  Sys.setenv("R_LIBS_USER" = "/home/zhangdc/R/x86_64-pc-linux-gnu-library/4.0")
}








