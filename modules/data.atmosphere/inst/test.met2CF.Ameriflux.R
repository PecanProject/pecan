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

# SH still hasn't been calculated
# cannot delete variables using ncdf4
# can build new files, but need to recreate the dimensions, variables and then create file
# can we just delete what we don't need using nco?
# can use nco to do the conversion. do this conversion before
# importing ncdf to R
#     nc.new <- nc_create(filename=paste(in.prefix,02004,sep="."),
#               vars=c(ncvar_get(nc=nc,varid='air_temperature'),
#                      ncvar_get(nc=nc,varid='wind_speed'),
#                      ncvar_get(nc=nc,varid='air_pressure'),
#                      ncvar_get(nc=nc,varid='surface_downwelling_shortwave_flux'),
#                      ncvar_get(nc=nc,varid='surface_downwelling_longwave_flux'),
#                      ncvar_get(nc=nc,varid='precipitation_flux'),
#                      ncvar_get(nc=nc,varid='YEAR'),
#                      ncvar_get(nc=nc,varid='GAP'),
#                      ncvar_get(nc=nc,varid='DTIME'),
#                      ncvar_get(nc=nc,varid='DOY'),
#                      ncvar_get(nc=nc,varid='HRMIN'),
#                      ncvar_get(nc=nc,varid='UST'),
#                      ncvar_get(nc=nc,varid='WD'),
#                      ncvar_get(nc=nc,varid='TS1'),
#                      ncvar_get(nc=nc,varid='TSdepth1'),
#                      ncvar_get(nc=nc,varid='TS2'),
#                      ncvar_get(nc=nc,varid='TSdepth2'),
#                      ncvar_get(nc=nc,varid='RH'),
#                      ncvar_get(nc=nc,varid='VPD'),
#                      ncvar_get(nc=nc,varid='SWC1'),
#                      ncvar_get(nc=nc,varid='SWC2'),
#                      ncvar_get(nc=nc,varid='PAR')
#                      ))