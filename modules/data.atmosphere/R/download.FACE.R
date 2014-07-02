download.FACE <- function(outfolder,start_year,end_year){
  
  # Download Raw FACE data from the internet
  
  if(!file.exists(outfolder)) dir.create(outfolder)
  
  # vlist <- c("DUKE","KSCO","KSCO_chamber","NDFF","ORNL","PHAC","RHIN")
  vlist <- c("KSCO","KSCO_chamber","NDFF","PHAC","RHIN")
  
  for (v in vlist){
    
    system(paste("wget -c ftp://cdiac.ornl.gov/.private/eCO2_Modelling/Site_Data/",v,"/", v,"_forcing_h.nc -O ",outfolder,"FACE.", v,"_forcing_h.nc",sep=""))
    
  }
} 
