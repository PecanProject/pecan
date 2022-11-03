SMAP_GEE2CSV <- function(SMAP_dir, Site_Info){
  #check if SMAP.csv exists.
  if(!file.exists(file.path(SMAP_dir, "SMAP.csv"))){
    PEcAn.logger::logger.info("Please Provide SMAP dir that contains the SMAP.csv file!")
    return(0)
  }
  
  SMAP_csv <- read.csv(file.path(SMAP_dir, "SMAP.csv"))
  
  
}