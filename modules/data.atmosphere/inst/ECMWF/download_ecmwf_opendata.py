def ecmwflatest(time, step, stream, type, params):
  
    """
    Fetches latest ECMWF Open Data forecast based on specified forecast time, step, stream, parameters
    
    Parameters
    ----------
    time (numeric) -- reference time of the forecasts. Values are 0 (00), 12. 
    
    step (numeric) -- forecast time step. Current value: 360
    
    stream (str) -- forecasting system that produces the data. Current value: `enfo`
    
    lat_in (numeric) -- Site coordinates, decimal degrees
    
    lon_in (numeric) -- Site coordinates, decimal degrees 
    
    Returns
    -------
    Latest date for which data is available.
    """
    
    from ecmwf.opendata import Client
    
    client = Client("ecmwf", beta=True)
    
    if stream == "mmsf":
      latest= client.latest(
        time = int(time),
        step= step,
        stream= stream,
        type= type,
        param= params,
        )
    
    elif stream == "enfo":
      latest= client.latest(
        time = int(time),
        step= int(step),
        stream= stream,
        type= type,
        param= params,
        )
    
    return latest


def ecmwfdownload(date, time, step, stream, type, params, filename):
  
    """
    Downloads latest ECMWF Open Data forecast based on specified forecast time, step, stream, parameters
    
    Parameters
    ----------
    date (numeric) -- reference date of the forecasts. Values are 0 (today), -1 (yesterday), -2 (day before yesterday)
    
    time (numeric) -- reference time of the forecasts. Values are 0 (00), 12. 
    
    step (numeric) -- forecast time step. Current value: 360
    
    stream (str) -- forecasting system that produces the data. Current value: `enfo`
    
    type (str) -- the type of data. Current values: `cf`, `pf`
    
    params (str) -- the meteorological parameters, such as wind, pressure or humidity. Current parameters are "2t", "tp", "10u", "10v", "q", "r", "sp".
    
    filename (str) -- output filename for the Grib2 file
    
    Returns
    -------
    Downloads ECMWF Open Data forecast for the latest date for which forecast data is available.
    """
    
    from ecmwf.opendata import Client
    
    client = Client("ecmwf", beta=True)
    
    file = client.retrieve(
      date= int(date),
      time= int(time),
      step= int(step),
      stream= stream,
      type= type,
      param= params,
      target= filename
    )
    
    return file
