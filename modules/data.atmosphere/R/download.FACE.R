<<<<<<< HEAD
download.FACE <- function(outfolder,start_year,end_year){
=======
download.FACE <- function(outfolder){
>>>>>>> bc85831d643cbfef44a9d25825f8a9d0f9423fe3
  
  # Download Raw FACE data from the internet
  
  if(!file.exists(outfolder)) dir.create(outfolder)
  
<<<<<<< HEAD
  # vlist <- c("DUKE","KSCO","KSCO_chamber","NDFF","ORNL","PHAC","RHIN")
  vlist <- c("KSCO","KSCO_chamber","NDFF","PHAC","RHIN")
  
  for (v in vlist){
    
    system(paste("wget -c ftp://cdiac.ornl.gov/.private/eCO2_Modelling/Site_Data/",v,"/", v,"_forcing_h.nc -O ",outfolder,"FACE.", v,"_forcing_h.nc",sep=""))
=======
  vlist <- c("DUKE","KSCO","KSCO_chamber","NDFF","ORNL","PHAC","RHIN")
  
  for (v in vlist){
    
    system(paste("wget -c -P ", outfolder ," ftp://cdiac.ornl.gov/.private/eCO2_Modelling/Site_Data/",v,"/", v,"_forcing_h.nc -O FACE.", v,"_forcing_h.nc",sep=""))
>>>>>>> bc85831d643cbfef44a9d25825f8a9d0f9423fe3
    
  }
} 
