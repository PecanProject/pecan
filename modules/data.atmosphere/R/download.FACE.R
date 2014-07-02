download.FACE <- function(outfolder){
  
  # Download Raw FACE data from the internet
  
  if(!file.exists(outfolder)) dir.create(outfolder)
  
  vlist <- c("DUKE","KSCO","KSCO_chamber","NDFF","ORNL","PHAC","RHIN")
  
  for (v in vlist){
    
    system(paste("wget -c -P ", outfolder ," ftp://cdiac.ornl.gov/.private/eCO2_Modelling/Site_Data/",v,"/", v,"_forcing_h.nc -O FACE.", v,"_forcing_h.nc",sep=""))
    
  }
} 
