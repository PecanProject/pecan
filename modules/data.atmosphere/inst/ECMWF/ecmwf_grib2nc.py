# !pip install xarray
# !pip install cfgrib

def grib2nc_ecmwf(grib_fname):
  import xarray
  import cfgrib
  
  grib_fname = str(grib_fname)
  
  dataset = xarray.open_dataset(grib_fname, engine='cfgrib')
           
  fname = str(grib_fname[:-6])
  
  for i in range(50):
    
    ens = dataset.isel(number=i)
    
    fname = fname+"ens_"+str(i+1)+".nc"
  
    ens.to_netcdf(fname)
    
    fname = str(grib_fname[:-6])
