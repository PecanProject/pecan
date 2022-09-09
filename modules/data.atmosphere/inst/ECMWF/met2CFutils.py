#The functions contained in this script help in extraction of CF and PF data from GRIB2 files and conversion to CF standard
#Author: Swarnalee Mazumder

def all_filenames_3h(filename):
    files_3 = []
  
    for i in range(0, 142,3):
        files_3.append(filename+"_"+str(i)+".grib2")
    
    return files_3

def all_filenames_6h(filename):
    files_6 = []
    for j in range(144, 366, 6):
            files_6.append(filename+"_"+str(j)+".grib2")
            
    return files_6
        
def combine_files_cf(files, lat_in, lon_in, out_filename):
    
    """
    Converts ECMWF Open Data 15 day controlled forecast data from Grib2 into PEcAn standard NetCDF
    
    Parameters
    ----------
    in_filename (str) -- Grib2 file consisting of forecast data
    
    outfolder (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.
    
    out_filename (str) -- filename of output NetCDF file
    
    lat_in (numeric) -- Site coordinates, decimal degrees
    
    lon_in (numeric) -- Site coordinates, decimal degrees
    
    Returns
    -------
    output netCDF files are saved in the specified directory.
    """
    
    import os
    import xarray
    import cfgrib
    
    out_filename = str(out_filename)
    
    lat_in = float(lat_in)
    lon_in = float(lon_in)
    
    if not os.path.exists(outfolder):
        os.makedirs(outfolder, exist_ok=True)
    
    cf_u10 = xarray.open_mfdataset(files, engine = 'cfgrib', concat_dim="valid_time",combine="nested",
                              backend_kwargs={'filter_by_keys': {'dataType': 'cf', 'shortName':'10u'}})

    cf_v10 = xarray.open_mfdataset(files, engine = 'cfgrib', concat_dim="valid_time",combine="nested",
                                  backend_kwargs={'filter_by_keys': {'dataType': 'cf', 'shortName':'10v'}})

    cf_t2m = xarray.open_mfdataset(files, engine = 'cfgrib', concat_dim="valid_time",combine="nested",
                                  backend_kwargs={'filter_by_keys': {'dataType': 'cf', 'shortName':'2t'}})

    cf_sp = xarray.open_mfdataset(files, engine = 'cfgrib', concat_dim="valid_time",combine="nested",
                                  backend_kwargs={'filter_by_keys': {'dataType': 'cf', 'shortName':'sp'}})

    cf_tp = xarray.open_mfdataset(files, engine = 'cfgrib', concat_dim="valid_time",combine="nested",
                                  backend_kwargs={'filter_by_keys': {'dataType': 'cf', 'shortName':'tp'}})

    cf = xarray.merge([cf_u10, cf_v10, cf_t2m, cf_sp, cf_tp], 
                          compat = 'override')
    
    # total precipitation unit as per CF standard
    # all other variables already in CF standard units
    cf.tp.attrs.update({'GRIB_units': 'kg m**-2 s**-1', 'units': 'kg m**-2 s**-1'})
    attrs_tp_cf = cf.tp.attrs


    # 1 m = 1/86.4 kg/m2/s 
    cf['tp'] = cf.tp / 86.4
    cf.tp.attrs = attrs_tp_cf

    
    cf_lat_lon = cf.sel(latitude = lat_in, longitude = lon_in, method = "nearest")
    
    
    cf_save_path = os.path.join(
        outfolder,
        out_filename
        + "_cf"
        + ".nc"
      )
  
    cf_lat_lon.to_netcdf(cf_save_path)
    
    return cf_save_path

def combine_files_pf(files, lat_in, lon_in, out_filename):
    
    """
    Converts ECMWF Open Data 15 day perturbed forecast from Grib2 into PEcAn standard NetCDF
    
    Parameters
    ----------
    in_filename (str) -- Grib2 file consisting of forecast data
    
    outfolder (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.
    
    out_filename (str) -- filename of output NetCDF file
    
    lat_in (numeric) -- Site coordinates, decimal degrees
    
    lon_in (numeric) -- Site coordinates, decimal degrees
    
    Returns
    -------
    output netCDF files are saved in the specified directory.
    """
    import os
    import xarray
    import cfgrib
    
    out_filename = str(out_filename)
    
    lat_in = float(lat_in)
    lon_in = float(lon_in)
        
    if not os.path.exists(outfolder):
        os.makedirs(outfolder, exist_ok=True)
        
    ######### Perturbed Forecast
    # 10 metre U wind component
    pf_u10 = xarray.open_mfdataset(files, engine='cfgrib', concat_dim="valid_time",combine="nested",
                    backend_kwargs={'filter_by_keys': {'dataType': 'pf', 'shortName':'10u'}})

    # 10 metre V wind component
    pf_v10 = xarray.open_mfdataset(files, engine='cfgrib', concat_dim="valid_time",combine="nested",
                    backend_kwargs={'filter_by_keys': {'dataType': 'pf', 'shortName':'10v'}})

    # 2 metre temperature
    pf_t2m = xarray.open_mfdataset(files, engine='cfgrib', concat_dim="valid_time",combine="nested",
                    backend_kwargs={'filter_by_keys': {'dataType': 'pf', 'shortName':'2t'}})
                    
    # Surface pressure
    pf_sp = xarray.open_mfdataset(files, engine='cfgrib', concat_dim="valid_time",combine="nested",
                    backend_kwargs={'filter_by_keys': {'dataType': 'pf', 'shortName':'sp'}})

    # total precipitation
    pf_tp = xarray.open_mfdataset(files, engine='cfgrib', concat_dim="valid_time",combine="nested",
                    backend_kwargs={'filter_by_keys': {'dataType': 'pf', 'shortName':'tp'}})
    
    
    pf = xarray.merge([pf_u10, pf_v10, pf_t2m, pf_sp, pf_tp], 
                          compat = 'override')
    
      
    pf.tp.attrs.update({'GRIB_units': 'kg m**-2 s**-1', 'units': 'kg m**-2 s**-1'})
    attrs_tp_pf = pf.tp.attrs

    # 1 m = 1/86.4 kg/m2/s 
    pf['tp'] = pf.tp / 86.4
    pf.tp.attrs = attrs_tp_pf

    
    pf_lat_lon = pf.sel(latitude = lat_in, longitude = lon_in, method = "nearest")
    
    
    pf_save_path = os.path.join(
        outfolder,
        out_filename
        + "_cf"
        + ".nc"
      )

    pf_lat_lon.to_netcdf(out_filename)
    
    return pf_save_path
