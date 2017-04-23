import json
import numpy as np
from netCDF4 import Dataset
from datetime import date, datetime

_UNITS = {u'air_temperature': 'K',
          u'relative_humidity': '%',
          u'wind_speed': 'm/s',
          u'precipitation_rate': 'mm/s',
          u'air_pressure': 'Pa',
          u'surface_downwelling_photosynthetic_photon_flux_in_air': 'mol m-2 s-1',
          u'northward_wind': 'degrees',
          u'eastward_wind': 'degrees'}

_UNIX_BASETIME = date(year=1970, month=1, day=1)

def JSONHandler(fileloc):
    # Write JSON file to a Python list wih standard JSON module
    with open(fileloc, 'r') as infile:
        return json.load(infile)

def translateTime(timeString):
    timeUnpack = datetime.strptime(timeString, "%Y-%m-%dT%H:%M:%SZ").timetuple()
    timeSplit = date(year=timeUnpack.tm_year, month=timeUnpack.tm_mon, day=timeUnpack.tm_mday) - _UNIX_BASETIME

    return (timeSplit.total_seconds() + timeUnpack.tm_hour * 3600.0 - timeUnpack.tm_min * 60.0)/(3600.0*24.)

def json2netcdf(jsondata, outfile):
    with Dataset(outfile, 'w') as netCDFHandler:
        time = {}
        time['start_time'] = [d['start_time'] for d in jsondata if 'start_time' in d]
        time['end_time'] = [d['start_time'] for d in jsondata if 'end_time' in d]
        latitude = [d['geometry']["coordinates"][0] for d in jsondata]
        longitude = [d['geometry']["coordinates"][1] for d in jsondata]
        datapoints = {}
        for key in jsondata[0]['properties'].keys():
            if key != 'source_file':
                datapoints[key] = [d['properties'][key] for d in jsondata]

        netCDFHandler.createDimension("latitude", 1)
        netCDFHandler.createDimension("longitude", 1)
        netCDFHandler.createDimension("time", None)

        timeVar = netCDFHandler.createVariable("time", "f8", ("time",))
        timeVar[:] = [translateTime(d) for d in time['start_time']]
        
        latitudeVar = netCDFHandler.createVariable("latitude", 'f8', ("latitude",))
        latitudeVar[:] = latitude[0]
        longitudeVar = netCDFHandler.createVariable("longitude", 'f8', ("longitude",))
        longitudeVar[:] = longitude[0]

        for var in datapoints.keys():
            valueVariable = netCDFHandler.createVariable(var, 'f8', ("latitude", "longitude", "time"))

            if u'NaN' in datapoints[var]:
                for i in range(len(datapoints[var])):
                    if datapoints[var][i] == u'NaN':
                        datapoints[var][i] = 0.0
            values = np.zeros((1, 1, len(datapoints[var])))
            values[:, :, :] = datapoints[var]
            valueVariable[:, :, :] = values
            setattr(valueVariable, "unit", _UNITS[var])
            setattr(valueVariable, "standard_name", var)

          
          
def met2cf_geostreams(input_dat_file, output_netcdf_file):
          data = JSONHandler(input_dat_file)
          json2netcdf(data, output_netcdf_file)
          
if __name__ == "__main__":
    met2cf_geostreams("results.dat", "netcdf.nc")
