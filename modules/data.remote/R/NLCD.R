##' @title download.NLCD
##' @name  download.NLCD
##' 
##' @author Mike Dietze
##' @export
##' 
##' @param outdir  Directory to download NLCD to
##' @param year    which NLCD year to download
##' @param con     Optional database connection. If specified then the code will check to see if the file already exists in PEcAn before downloading, and will also create a database entry for new downloads
##' 
##' @description  Downloads and unzips the National Land Cover Database http://www.mrlc.gov/nlcd2011.php. Will automatically insert into PEcAn database if database connection provided.
download.NLCD <- function(outdir,year=2011,con=NULL){
  
  if(year == 2001){
    url = "http://gisdata.usgs.gov/TDDS/DownloadFile.php?TYPE=nlcd2001v2&FNAME=nlcd_2001_landcover_2011_edition_2014_10_10.zip"
    input.id = 1000000482
  } else if (year == 2011){
    url = "http://gisdata.usgs.gov/TDDS/DownloadFile.php?TYPE=nlcd2011&FNAME=nlcd_2011_landcover_2011_edition_2014_10_10.zip"
    input.id = 1000000483
  } else {
    print(paste("Year not yet supported: ",year))
  }
  
  ## before downloading, check if the file already exists on this host
  if(!is.null(con)){
    require(PEcAn.DB)
    chk = dbfile.check(type = "Input",id = input.id,con = con)
    if(nrow(chk)>0){
      machines = db.query(paste("SELECT * from machines where id in (",paste(chk$machine_id,sep=","),")"),con)
      if(fqdn() %in% machines$hostname){
        ## record already exists on this host
        return(chk$id[fqdn() == machines$hostname])
      }
    }
  }
    
  ## Download the data
  dir.create(outdir, showWarnings = FALSE)
  destfile = file.path(outdir,paste0("nlcd",year,".zip"))
  download.file(url,destfile=destfile)
  status = system(paste("(cd",outdir,"; unzip",destfile,")"))
  #  unzip(destfile,exdir = outdir)  ## unzip command produced corrupted file!
  file.remove(destfile) ## cleanup raw zip file
  
  ## Insert the database record
  data_dir = file.path(outdir,paste0("nlcd_",year,"_landcover_2011_edition_2014_10_10"))
  if(!is.null(con)){  
    prefix = table(sapply(strsplit(dir(data_dir),".",fixed=TRUE),function(x){x[1]}))
    prefix = names(which.max(prefix))
    site.id =  1000000676
    return(dbfile.insert(data_dir,in.prefix = prefix,type = "Input",input.id,con,reuse=TRUE))
  }
  return(data_dir)
}

##' @title extract.NLCD
##' @author Mike Dietze
##' @export
##' 
##' @param buffer  search radius (meters)
##' @param coords  data frame containing elements 'long' and 'lat'. Currently just supports single point extraction.
##' @param data_dir  directory where input data is located. Can be NUL if con is specified
##' @param con       connection to PEcAn database. Can be NULL if data_dir is specified
##' 
##' @return dataframe of fractional cover of different cover classes
##' 
##' @description Based on codes from Christy Rollinson and from Max Joseph (http://mbjoseph.github.io/2014/11/08/nlcd.html)
extract_NLCD <- function(buffer, coords,
                          data_dir=NULL,con=NULL,year=2011){
  require(raster)
  require(rgdal)
  require(stringr)
  
  if(!is.null(con)){
    require(PEcAn.DB)
    if(year == 2001){
      input.id = 1000000482
    } else if (year == 2011){
      input.id = 1000000483
    } else {
      print(paste("Year not yet supported: ",year))
    }    
    chk = dbfile.check(type = "Input",id = input.id,con = con)
    if(nrow(chk)>0){
      machines = db.query(paste("SELECT * from machines where id in (",paste(chk$machine_id,sep=","),")"),con)
      if(fqdn() %in% machines$hostname){
        ## record already exists on this host
        data_dir = chk$file_path[fqdn() == machines$hostname]
      } else{
        print(paste0("File not found on localhost, please check database input.id ",input.id,". You may need to run download.NLCD"))
        return(list(chk=chk,machines=machines,localhost=fqdn()))
      }
    } else {
      print(paste("No files found for input.id",input.id))
      return(NA)
    }
  }
  
  # load cover data
  filename <- file.path(data_dir, paste0("nlcd_",year,"_landcover_2011_edition_2014_10_10.img"))
  if(!file.exists(filename)){print(paste("File not found:",filename)); return(NULL)}
  nlcd <- raster(filename)
  
  # transform points
  sites <- SpatialPoints(coords=coords, proj4string=CRS("+proj=longlat +datum=WGS84"))
  sites <- spTransform(sites, crs(nlcd))
  
  # extract
  summ = prop.table(table(extract(nlcd, sites,buffer=buffer)))
  mydf <- data.frame(cover = names(summ),percent = as.vector(summ))
  
  #land cover number to name conversions
  cover.table <- nlcd@data@attributes[[1]]
  cover.names <- cover.table[as.numeric(as.character(mydf$cover)) + 1,grep("Land",names(cover.table))]
  mydf$cover.name <- cover.names
  
  return(mydf)

}