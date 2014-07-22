##' Get MODIS data by date and location
##'
##' @name call_MODIS
##' @title call_MODIS
##' @export
##' @param start  first date in year and day-of-year. For example May 1 2010 would be 2010121
##' @param end    laste date in year and day-of-year. For example May 1 2010 would be 2010121
##' @param lat    Latitude of the pixel
##' @param lon    Longitude of the pixel
call_MODIS <- function(outfolder=".",fname='m_data.nc',start, end, lat, lon, size=0.0, product = 'MOD15A2', band = 'Lai_1km',qc_band=NA,sd_band=NA)  {

	   library(rPython)

	   # The name of the netCDF file. I've here given a constant name, but it can easily be changed to be an input
	   fname <- paste0(outfolder,'/',fname)

	   # Distance of the are both east-west and north-south from the center of the pixel. Similarly to the file name, I've left it also easily inputtable.
	   kmNS <- size
	   kmWE <- size

	   # Here it assigns the run directory and given variables values within python
	   python.assign('cwd', getwd())

     python.assign('start', as.integer(start))
	   if(python.get('start') != start)  stop("call_MODIS start date sent incorrectly")   
	   
	   python.assign('end', as.integer(end))
	   if(python.get('end') != end)  stop("call_MODIS end date sent incorrectly")   
	   
	   python.assign('lat', lat)
	   python.assign('lon', lon)
	   python.assign('kmNS',kmNS)
	   python.assign('kmWE',kmWE)
	   python.assign('fn', fname)
	   python.assign('product', product)
	   python.assign('band', band)
	   python.assign('qcband', qc_band)
	   python.assign('sdband', sd_band)
     
	   # Here we import the MODIS python script as a module for the python. That way we can run the routines within the script as independent commands.
     script.path = dirname(system.file("modisWSDL.py",package = "PEcAn.data.remote"))
	   python.exec(paste0('import sys; sys.path.append("',script.path,'")'))
	   python.exec('import modisWSDL')
	   # This is overkill if you are not editting modisWSDL, but 
	   # if you are developing this will refresh the definition of
	   # the module
	   python.exec('reload(modisWSDL)')
	   # And here we execute the main MODIS run. Although it should be noted that while we get values of the run here, the script also does write a netCDF output file.
	   python.exec('m, k, date = modisWSDL.run_main(start_date=start, end_date=end,la=lat,lo=lon,kmAB=kmNS,kmLR=kmWE,fname=fn,product=product,band=band,qcband=qcband,sdband=sdband)')	
	   
	   # m = The MODIS observed LAI for the given pixel
	   # k = The standard deviation of the MODIS LAI. Be careful with this as it is at times very low
	   # date = Year and day-of-year of the observation
	   m <- python.get('[ map(float, x) for x in m.data ]')
     if(!is.na(sd_band)){
	     k <- python.get('[ map(float, x) for x in k.data ]')
     } else {
       k = NA
     }
	   date <- python.get('date')
     
     invisible(list(m=m,k=k,date=date))
     
} 	