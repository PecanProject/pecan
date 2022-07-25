def grib2nc_ecmwf(in_filename, outfolder, out_filename, lat_in, lon_in):
    
    """
    Converts ECMWF Open Data 15 day forecast from Grib2 into PEcAn standard NetCDF
    
    Parameters
    ----------
    in_filename (str) -- Grib2 file consisting of forecast data
    
    outfolder (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.
    
    out_filename (str) -- filename of output NetCDF file
    
    lat_in (numeric) -- Site coordinates, decimal degrees
    
    lon_in (numeric) -- Site coordinates, decimal degrees 
    
    Returns
    -------
    output netCDF in CF Format which is saved in the specified directory.
    
    
    Example Usage:
      /don't run
      
      grib2nc_ecmwf(in_filename,
                    outfolder,
                    out_filename,
                    0,
                    180)
    """
    
    import os
    import xarray
    import cfgrib
    
    in_filename = str(in_filename)
    
    out_filename = str(out_filename)
    
    lat_in = float(lat_in)
    lon_in = float(lon_in)
    
    
    # if specified output directory does not exist create it
    if not os.path.exists(outfolder):
        os.makedirs(outfolder, exist_ok=True)
        
    
    # "dataType" needs to be defined to correctly open the grib file 
    # https://github.com/ecmwf/cfgrib/issues/285
    
    ######### Controlled Forecast
    
    # 10 metre U wind component
    cf_u10 = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'cf', 'shortName':'10u'}})

    # 10 metre V wind component
    cf_v10 = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'cf', 'shortName':'10v'}})

    # 2 metre temperature
    cf_t2m = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'cf', 'shortName':'2t'}})

    # total precipitation
    cf_tp = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'cf', 'shortName':'tp'}})

    # Surface pressure
    cf_sp = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'cf', 'shortName':'sp'}})

    # Specific humidity
    cf_q = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'cf', 'shortName':'q'}})

    # Relative humidity
    cf_r = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'cf', 'shortName':'r'}})


    # https://docs.xarray.dev/en/stable/generated/xarray.merge.html
    # “override”: skip comparing and copy attrs from the first dataset to the result
    cf_allvar = xarray.merge([cf_u10, cf_v10, cf_t2m, cf_tp, cf_sp, cf_q, cf_r], 
                                      compat='override')
    

    # total precipitation unit as per CF standard
    # all other variables already in CF standard units
    cf_allvar.tp.attrs.update({'GRIB_units': 'kg m**-2 s**-1', 'units': 'kg m**-2 s**-1'})
    attrs_tp_cf = cf_allvar.tp.attrs
    
    
    # 1 m = 1/86.4 kg/m2/s 
    cf_allvar['tp'] = cf_allvar.tp / 86.4
    cf_allvar.tp.attrs = attrs_tp_cf
    
    
    # variable name as per CF standard
    cf_allvar_CF_name_standard = cf_allvar.rename(name_dict={'u10':'eastward_wind',
                                                             'v10':'northward_wind',
                                                             't2m':'air_temperature',
                                                             'tp':'precipitation_flux',
                                                             'sp':'air_pressure',
                                                             'q':'specific_humidity',
                                                             'r':'relative_humidity'})
    
    
    cf_allvar_CF_lat_lon = cf_allvar_CF_name_standard.sel(latitude=lat_in, longitude=lon_in, method="nearest")
   

    cf_save_path = os.path.join(
        outfolder,
        out_filename
        + "_cf"
        + ".nc"
      )
    
   
    # Creates netCDF file corresponding to "dataType":"cf"
    cf_nc = cf_allvar_CF_lat_lon.to_netcdf(cf_save_path)
    

    ######### Perturbed Forecast
    
    # 10 metre U wind component
    pf_u10 = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'pf', 'shortName':'10u'}})

    # 10 metre V wind component
    pf_v10 = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'pf', 'shortName':'10v'}})

    # 2 metre temperature
    pf_t2m = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'pf', 'shortName':'2t'}})

    # total precipitation
    pf_tp = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'pf', 'shortName':'tp'}})

    # Surface pressure
    pf_sp = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'pf', 'shortName':'sp'}})

    # Specific humidity
    pf_q = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'pf', 'shortName':'q'}})

    # Relative humidity
    pf_r = xarray.open_dataset(in_filename, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': {'dataType': 'pf', 'shortName':'r'}})


    # https://docs.xarray.dev/en/stable/generated/xarray.merge.html
    # “override”: skip comparing and copy attrs from the first dataset to the result
    pf_allvar = xarray.merge([pf_u10, pf_v10, pf_t2m, pf_tp, pf_sp, pf_q, pf_r], 
                                      compat='override')
    
    
    # total precipitation unit as per CF standard 
    # all other variables already in CF standard units
    pf_allvar.tp.attrs.update({'GRIB_units': 'kg m**-2 s**-1', 'units': 'kg m**-2 s**-1'})
    attrs_tp_pf = pf_allvar.tp.attrs
    
    
    # 1 m = 1/86.4 kg/m2/s 
    pf_allvar['tp'] = pf_allvar.tp / 86.4
    pf_allvar.tp.attrs = attrs_tp_pf
    
    
    # variable name as per CF standard
    pf_allvar_CF_name_standard = pf_allvar.rename(name_dict={'u10':'eastward_wind',
                                                             'v10':'northward_wind',
                                                             't2m':'air_temperature',
                                                             'tp':'precipitation_flux',
                                                             'sp':'air_pressure',
                                                             'q':'specific_humidity',
                                                             'r':'relative_humidity'})
    
    
    
    # Creates 50 netCDF files corresponding to 50 ensemble members in "dataType":"pf" (perturbed forecast)
    for i in range(50):
      
        pf_allvar_ens = pf_allvar_CF_name_standard.isel(number=i)

        pf_allvar_lat_lon = pf_allvar_ens.sel(latitude=lat_in, longitude=lon_in, method="nearest")

        pf_save_path = os.path.join(
            outfolder,
            out_filename
            + "_pf_ens_"
            + str(i+1)
            + ".nc"
        )
    
        pf_allvar_lat_lon.to_netcdf(pf_save_path)
