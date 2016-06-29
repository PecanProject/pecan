#This script reads CRU NCEP MsTMIP variables using Opendap
#By: James Simkins (jsimkins2@wisc.edu)

############################## BEGIN CODE #############################################
import sys
print(sys.argv)

lat_desired = sys.argv[1]
lon_desired = sys.argv[2]
year_desired = sys.argv[3]
outfolder = sys.argv[4]

###################################
from netCDF4 import Dataset
import numpy as np
from numpy import arange, dtype
import math
import datetime
import calendar

#Define function to get variables
def get_var(sds, name):
    return np.array(sds.variables[name])

#Define custom round function to round lat/lons to .25 or .75 for CRU NCEP
def customRound(num, d = [.75, .25]):
     dec = num%1
     r = num - dec
     round_dec = min([(abs(i - dec),i) for i in d])[1]
     return r + round_dec

#Account extra 4 data points during leap years

year_desired = int(year_desired)
lat_desired = float(lat_desired)
lon_desired = float(lon_desired)

if calendar.isleap(year_desired):
    time_url = '1:1463'
else : time_url = '1:1459'

if lat_desired > 89.75:
    sys.exit("Latitude out of range")

if lat_desired < -89.75:
    sys.exit("Latitude out of range")

if lon_desired > 179.75:
    sys.exit("Longitude out of range")

if lon_desired < -179.75:
    sys.exit("Longitude is out of range")

#Round lat/lon desired to .25 or .75, add .25 for the algebra below
#lat_round = customRound(lat_desired+.25)
#lon_round = customRound(lon_desired+.25)

#Use alebra to translate given lat/lon to MSTMIP array value
#lat_target = np.abs((lat_round)*2 - 179.5)
#lon_target = np.abs((lon_round)*2 + 359.5)

#Truncate the value above to make value an integer instead of ending in .0
#lat_trunc = math.trunc(lat_target)
#lon_trunc = math.trunc(lon_target)

lat_trunc = int(2*(90-lat_desired))
lon_trunc = int(2*(lon_desired+180))

#Create strings out of calculated array values to enter into url string
lat_url = str(lat_trunc)
lon_url = str(lon_trunc)
year_url = str(year_desired)

if calendar.isleap(year_desired):
    timerange = range(0,1463)
else : timerange = range(0,1459)

lat = 0
lon = 0

#Get TA which is 6 hour temperature 
air_temperature_path ='http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/mstmip_driver_global_hd_climate_tair_'
air_temperature_indices = year_url+'_v1.nc4?tair['+time_url+']['+lat_url+']['+lon_url+']'
print(air_temperature_path+air_temperature_indices)
air_temperature_filehandle = Dataset(air_temperature_path+air_temperature_indices,'r',format="NETCDF4")
air_temperature_opendap = get_var(air_temperature_filehandle, 'tair')[timerange, lat, lon]

#Get Incoming Longwave Radiation - W/m2
surface_downwelling_longwave_flux_in_air_path = 'http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/mstmip_driver_global_hd_climate_lwdown_'
surface_downwelling_longwave_flux_in_air_indices = year_url+'_v1.nc4?lwdown['+time_url+']['+lat_url+']['+lon_url+']'
print(surface_downwelling_longwave_flux_in_air_path+surface_downwelling_longwave_flux_in_air_indices)
surface_downwelling_longwave_flux_in_air_filehandle = Dataset(surface_downwelling_longwave_flux_in_air_path+surface_downwelling_longwave_flux_in_air_indices, 'r', format="NETCDF4")
surface_downwelling_longwave_flux_in_air_opendap = get_var(surface_downwelling_longwave_flux_in_air_filehandle, 'lwdown')[timerange, lat, lon]

#Get Barometric Pressure (assuming pression from MsTMIP is barometric pressure) note units are Pa not kPa like ameriflux
air_pressure_path = 'http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/mstmip_driver_global_hd_climate_press_'
air_pressure_indices = year_url+'_v1.nc4?press['+time_url+']['+lat_url+']['+lon_url+']'
print(air_pressure_path+air_pressure_indices)
air_pressure_filehandle = Dataset(air_pressure_path+air_pressure_indices, 'r', format="NETCDF4")
air_pressure_opendap = get_var(air_pressure_filehandle, 'press')[timerange, lat, lon]

#Get Incoming_Short_Wave_Radiation
surface_downwelling_shortwave_flux_in_air_path = 'http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/mstmip_driver_global_hd_climate_swdown_'
surface_downwelling_shortwave_flux_in_air_indices = year_url+'_v1.nc4?swdown['+time_url+']['+lat_url+']['+lon_url+']'
print(surface_downwelling_shortwave_flux_in_air_path+surface_downwelling_shortwave_flux_in_air_indices)
surface_downwelling_shortwave_flux_in_air_filehandle = Dataset(surface_downwelling_shortwave_flux_in_air_path+surface_downwelling_shortwave_flux_in_air_indices, 'r', format="NETCDF4")
surface_downwelling_shortwave_flux_in_air_opendap = get_var(surface_downwelling_shortwave_flux_in_air_filehandle, 'swdown')[timerange, lat, lon]

eastward_wind_path = 'http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/mstmip_driver_global_hd_climate_uwind_'
eastward_wind_indices = year_url+'_v1.nc4?uwind['+time_url+']['+lat_url+']['+lon_url+']'
print(eastward_wind_path+eastward_wind_indices)
eastward_wind_filehandle = Dataset(eastward_wind_path+eastward_wind_indices, 'r', format = "NETCDF4")
eastward_wind_opendap = get_var(eastward_wind_filehandle, 'uwind')[timerange, lat, lon]

northward_wind_path = 'http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/mstmip_driver_global_hd_climate_vwind_'
northward_wind_indices = year_url+'_v1.nc4?vwind['+time_url+']['+lat_url+']['+lon_url+']'
print(northward_wind_path+northward_wind_indices)
northward_wind_filehandle = Dataset(northward_wind_path+northward_wind_indices, 'r', format = "NETCDF4")
northward_wind_opendap = get_var(northward_wind_filehandle, 'vwind')[timerange, lat, lon]

specific_humidity_path = 'http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/mstmip_driver_global_hd_climate_qair_'
specific_humidity_indices = year_url+'_v1.nc4?qair['+time_url+']['+lat_url+']['+lon_url+']'
print(specific_humidity_path+specific_humidity_indices)
specific_humidity_filehandle = Dataset(specific_humidity_path+specific_humidity_indices, 'r', format = "NETCDF4")
specific_humidity_opendap = get_var(specific_humidity_filehandle, 'qair')[timerange, lat, lon]

precip_path = 'http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/mstmip_driver_global_hd_climate_rain_'
precip_indices = year_url+'_v1.nc4?rain['+time_url+']['+lat_url+']['+lon_url+']'
print(precip_path+precip_indices)
precip_filehandle = Dataset(precip_path+precip_indices, 'r', format = "NETCDF4")
precip_opendap = get_var(precip_filehandle, 'rain')[timerange, lat, lon]

#Calculate Incoming Photosynthetically Active Radiation units: umol/m2/s
surface_downwelling_photosynthetic_photon_flux_in_air= (surface_downwelling_shortwave_flux_in_air_opendap)*2.1


###################Write the new NetCDF file#############################################
ncname = outfolder+'/CRUNCEP.'+year_url+'.nc'
print(ncname)
ncfile = Dataset(ncname,'w')

if calendar.isleap(year_desired):
    ntime = 1463
else : ntime = 1459

ntime = ntime
#Reshape
n_air_temperature = np.reshape(air_temperature_opendap,(ntime,1,1))
n_surface_downwelling_longwave_flux_in_air = np.reshape(surface_downwelling_longwave_flux_in_air_opendap,(ntime,1,1))
n_surface_downwelling_shortwave_flux_in_air = np.reshape(surface_downwelling_shortwave_flux_in_air_opendap,(ntime,1,1))
n_surface_downwelling_photosynthetic_photon_flux_in_air = np.reshape(surface_downwelling_photosynthetic_photon_flux_in_air,(ntime,1,1))
n_air_pressure = np.reshape(air_pressure_opendap,(ntime,1,1))
n_eastward_wind = np.reshape(eastward_wind_opendap,(ntime,1,1))
n_northward_wind = np.reshape(northward_wind_opendap,(ntime,1,1))
n_specific_humidity = np.reshape(specific_humidity_opendap,(ntime,1,1))
n_precip = np.reshape(precip_opendap,(ntime,1,1))

# Assign the dimension data to the new NetCDF file.
ntime = int(ntime)
ncfile.createDimension('time',ntime)
ncfile.createDimension('latitude',1)
ncfile.createDimension('longitude',1)

air_temperature = ncfile.createVariable('air_temperature',dtype('float32').char,('time','latitude','longitude'))
surface_downwelling_longwave_flux_in_air = ncfile.createVariable('surface_downwelling_longwave_flux_in_air',dtype('float32').char,('time','latitude','longitude'))
surface_downwelling_shortwave_flux_in_air = ncfile.createVariable('surface_downwelling_shortwave_flux_in_air',dtype('float32').char,('time','latitude','longitude'))
air_pressure = ncfile.createVariable('air_pressure',dtype('float32').char,('time','latitude','longitude'))
eastward_wind = ncfile.createVariable('eastward_wind',dtype('float32').char,('time','latitude','longitude'))
northward_wind = ncfile.createVariable('northward_wind',dtype('float32').char,('time','latitude','longitude'))
specific_humidity = ncfile.createVariable('specific_humidity',dtype('float32').char,('time','latitude','longitude'))
precipitation_flux = ncfile.createVariable('precipitation_flux',dtype('float32').char,('time','latitude','longitude'))
surface_downwelling_photosynthetic_photon_flux_in_air = ncfile.createVariable('surface_downwelling_photosynthetic_photon_flux_in_air', dtype('float32').char,('time','latitude','longitude'))
time = ncfile.createVariable('time', 'd', ('time',))
latitude = ncfile.createVariable('latitude','f4',('latitude',))
longitude = ncfile.createVariable('longitude','f4',('longitude',))

time.units = "sec"
nseconds = ntime*21600
time[:]= np.arange(0,nseconds,21600)

latitude.units = "degrees north"
longitude.units = "degrees east"
latitude[:] = lat_trunc
longitude[:] = lon_trunc

air_temperature[:] = n_air_temperature
air_temperature.long_name = "Air Temperature"
air_temperature.units = "Kelvin"
air_temperature.missing_value = "-999.0"
air_temperature.fill_value = "-999.0"

surface_downwelling_longwave_flux_in_air[:] = n_surface_downwelling_longwave_flux_in_air
surface_downwelling_longwave_flux_in_air.long_name = "Incoming Longwave Radiation"
surface_downwelling_longwave_flux_in_air.units = "W/m2"
surface_downwelling_longwave_flux_in_air.missing_value = "-999.0"
surface_downwelling_longwave_flux_in_air.fill_value = "-999.0"

surface_downwelling_shortwave_flux_in_air[:] = n_surface_downwelling_shortwave_flux_in_air
surface_downwelling_shortwave_flux_in_air.long_name = "Incoming Global Solar Radiation"
surface_downwelling_shortwave_flux_in_air.units = "W/m2"
surface_downwelling_shortwave_flux_in_air.missing_value = "-999.0"
surface_downwelling_shortwave_flux_in_air.fill_value = "-999.0"

surface_downwelling_photosynthetic_photon_flux_in_air[:] = n_surface_downwelling_photosynthetic_photon_flux_in_air
surface_downwelling_photosynthetic_photon_flux_in_air.long_name = "Incoming Photosynthetically Active Radiation"
surface_downwelling_photosynthetic_photon_flux_in_air.units = "mol m-2 s-1"
surface_downwelling_photosynthetic_photon_flux_in_air.missing_value = "-2097.9"
surface_downwelling_photosynthetic_photon_flux_in_air.fill_value = "-2097.9"

air_pressure[:] = n_air_pressure
air_pressure.long_name = "Barometric Pressure"
air_pressure.units = "Pascal"
air_pressure.missing_value = "-999.0"
air_pressure.fill_value = "-999.0"

eastward_wind[:] = n_eastward_wind
eastward_wind.long_name = "U_wind_component"
eastward_wind.units = "m/s"
eastward_wind.missing_value = "-999.0"
eastward_wind.fill_value = "-999.0"

northward_wind[:] = n_northward_wind
northward_wind.long_name = "V_wind_component"
northward_wind.units = "m/s"
northward_wind.missing_value = "-999.0"
northward_wind.fill_value = "-999.0"

specific_humidity[:] = n_specific_humidity
specific_humidity.long_name = "Air_Specific_Humidity"
specific_humidity.units = "g/g"
specific_humidity.missing_value = "-999.0"
specific_humidity.fill_value = "-999.0"

precipitation_flux[:] = n_precip
precipitation_flux.long_name = "Total_Precipitation"
precipitation_flux.units = "mm/6h"
precipitation_flux.missing_value = "-999.0"
precipitation_flux.fill_value = "-999.0"

ncfile.close()
print(ncname)
