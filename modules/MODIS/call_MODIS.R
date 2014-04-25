
call_MODIS <- function(start, end, lat, lon)  {
	   # start = start date in year and day-of-year. For example May 1 2010 would be 2010121
	   # end = end date in year and day-of-year. For example May 1 2010 would be 2010121
	   # lat = Latitude of the pixel
	   # lon = Longitude of the pixel

	   library(rPython)

	   # The name of the netCDF file. I've here given a constant name, but it can easily be changed to be an input
	   fname <- 'm_data.nc'

	   # Distance of the are both east-west and north-south from the center of the pixel. Similarly to the file name, I've left it also easily inputtable.
	   kmNS <- 0.
	   kmWE <- 0.

	   # Here it assigns the run directory and given variables values within python
	   python.assign('cwd', getwd())
	   python.assign('start', start)
	   python.assign('end', end)
	   python.assign('lat', lat)
	   python.assign('lon', lon)
	   python.assign('kmNS',kmNS)
	   python.assign('kmWE',kmWE)
	   python.assign('fn', fname)
	   # Here we import the MODIS python script as a module for the python. That way we can run the routines within the script as independent commands.
	   python.exec('import sys; sys.path.append(cwd)')
	   python.exec('import modisWSDL')
	   # This is overkill if you are not editting modisWSDL, but 
	   # if you are developing this will refresh the definition of
	   # the module
	   python.exec('reload(modisWSDL)')
	   # And here we execute the main MODIS run. Although it should be noted that while we get values of the run here, the script also does write a netCDF output file.
	   python.exec('m, k, date = modisWSDL.run_main(start_date=start, end_date=end,la=lat,lo=lon,kmAB=kmNS,kmLR=kmWE,fname=fn)')	
	   
	   # m = The MODIS observed LAI for the given pixel
	   # k = The standard deviation of the MODIS LAI. Be careful with this as it is at times very low
	   # date = Year and day-of-year of the observation
	   m <- python.get('[ map(float, x) for x in m.data ]')
	   k <- python.get('[ map(float, x) for x in k.data ]')
	   date <- python.get('date')
	   } 	