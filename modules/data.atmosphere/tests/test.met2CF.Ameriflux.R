if(FALSE){

in.path = "/Users/josh/Downloads/"
in.prefix = "AMF_USDk2"

setwd(in.path)
system2("wget","ftp://cdiac.ornl.gov/pub/ameriflux/data/Level2/Sites_ByName/Missouri_Ozark/with_gaps/AMF_USMOz_2006_L2_WG_V004.nc")
system2("wget","ftp://cdiac.ornl.gov/pub/ameriflux/data/Level2/Sites_ByName/Howland_Forest_Main/with_gaps/AMF_USHo1_2000_L2_WG_V003.nc")
system2("wget","ftp://cdiac.ornl.gov/pub/ameriflux/data/Level2/Sites_ByName/Harvard_Forest/with_gaps/AMF_USHa1_2008_L2_WG_V007.nc")
system2("wget","ftp://cdiac.ornl.gov/pub/ameriflux/data/Level2/Sites_ByName/Duke_Forest_Hardwoods/with_gaps/AMF_USDk2_2002_L2_WG_V003.nc")
outfolder = "/Users/josh/temp/"

#rh2rv from metutils must be loaded for this function to work
rh2rv <- function(rh, T){
  rh*2.541e6*exp(-5415.0/T)*18/29
}

met2CF.Ameriflux(in.path,in.prefix,outfolder)

}