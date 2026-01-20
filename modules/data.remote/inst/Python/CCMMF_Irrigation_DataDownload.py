#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 23 14:42:41 2025

@author: krein21
"""
# %% Import modules

import requests
import numpy as np
import pandas as pd
from netCDF4 import Dataset, num2date
import CCMMF_Irrigation_CalcVis
import os
import ee

ee.Initialize()


# %% Download GEE OPEN ET Data

def GEEOpenET(START_DATE, END_DATE, LAT, LON):
    
    # Access OpenET dataset
    collection = ee.ImageCollection("OpenET/ENSEMBLE/CONUS/GRIDMET/MONTHLY/v2_0") \
    .filterDate(START_DATE, END_DATE) \
    .filterBounds(ee.Geometry.Point([LON, LAT]))

    # Extract et time series
    def extract_et(img):
        date = img.date().format()
        et = img.reduceRegion(ee.Reducer.first(), ee.Geometry.Point([LON, LAT]), 1000).get('et_ensemble_mad')
        return ee.Feature(None, {'time': date, 'et': et})

    et_series = collection.map(extract_et)
    
    # Convert data to df
    et_series = et_series.getInfo()  # Convert from ee.List to Python list
    et_series = et_series['features'] # Select just the features dictionary
    open_et_df = pd.DataFrame(et_series) # Turn dictionary into dataframe
    open_et_df = open_et_df['properties'].apply(pd.Series) # Select properties and turn dictionary into dataframe
    open_et_df['time'] = pd.to_datetime(open_et_df['time'])
    
    return open_et_df

# %% Request OPEN ET Data (from website)

def OpenETData(START_DATE, END_DATE, LAT, LON):
    
    # Set directory
    working_dir = '/projectnb/dietzelab/ccmmf/management/irrigation/'
    os.chdir(working_dir)
    
    # Read in API Key
    with open('OpenETAPIKey.txt', 'r') as file:
        api_key = file.readline()

    header = {"Authorization": api_key}
    
    # endpoint arguments
    args = {
      "date_range": [START_DATE, END_DATE],
      "interval": "daily",
      "geometry": [LON,LAT],
      "model": "Ensemble",
      "variable": "ET",
      "reference_et": "gridMET",
      "units": "mm",
      "file_format": "JSON"
    }
    
    # query the api 
    resp = requests.post(
        headers=header,
        json=args,
        url="https://openet-api.org/raster/timeseries/point"
    )
    
    # Parse the JSON response
    et_data = resp.json()
    
    open_et_df = pd.DataFrame(et_data)
    open_et_df['time'] = pd.to_datetime(open_et_df['time'])
    
    return open_et_df

# %% Download CHIRPS Data

def CHIRPSData(YEAR, LAT, LON):
    
    # Set URL and file name
    url = f'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/netcdf/p05/chirps-v2.0.{YEAR}.days_p05.nc'
    destfile = f'/projectnb/dietzelab/ccmmf/management/irrigation/chirps-v2.0.{YEAR}.days_p05.nc'
    
    # Check if the file already exists before downloading
    if not os.path.exists(destfile):
        print(f"{destfile} not found. Downloading now...")
        response = requests.get(url, timeout=600)
        
        with open(destfile, 'wb') as f:
            f.write(response.content)
    
    # Open the NetCDF file
    nc_data = Dataset(destfile, 'r')
    
    # Print metadata for precipitation
    #precip_variable = nc_data.variables['precip']
    #print(precip_variable)
    
    # Extract coordinate and time variables
    lon = nc_data.variables['longitude'][:]
    lat = nc_data.variables['latitude'][:]
    time = nc_data.variables['time']
    
    # Find the nearest lat/lon index
    lon_idx = np.abs(lon - LON).argmin()
    lat_idx = np.abs(lat - LAT).argmin()
    
    # Extract the data just for that lat lon
    precip_data = nc_data.variables['precip'][:, lat_idx, lon_idx]

    # Convert time to standard datetime
    dates = num2date(time[:], units=time.units, calendar=time.calendar)
    dates = [pd.Timestamp(date.isoformat()) for date in dates]
    
    # Close the NetCDF file when done
    nc_data.close()
    
    # Clean data
    precip_data = precip_data.filled(np.nan)
    precip_data_df = pd.DataFrame({
        'time': dates,
        'precip': precip_data
    })
    
    return precip_data_df

# %% Calculate and visualize new data for the API downloded data

def new_data_entry_API(LAT, LON, years, csv_folder, START_DATE = None, END_DATE = None):
    print(f'{LAT} {LON} {years}')
    
    # Define start and end date
    if START_DATE == None or END_DATE == None:
        START_DATE = f'{years[0]}-01-01'
        END_DATE = f'{years[-1]}-12-31'
    
    # Download open et data
    et_df = OpenETData(START_DATE, END_DATE, LAT, LON)
    
    # Download CHIRPS data year by year and concatenate
    precip_data = pd.DataFrame()
    for year in years:
        precip_data_year = CHIRPSData(year, LAT, LON)
        precip_data = pd.concat([precip_data, precip_data_year], ignore_index=True)
    
    # Organize and water balance
    df_water_balance = et_df
    df_water_balance['precip'] = precip_data['precip']
    df_water_balance = CCMMF_Irrigation_CalcVis.water_balance(df_water_balance, LAT, LON)
    
    # Graph
    df_water_balance['time'] = pd.to_datetime(df_water_balance['time'])
    for year in years:
        CCMMF_Irrigation_CalcVis.timeseries_graphs_API(df_water_balance[df_water_balance['time'].dt.year == year], LAT, LON, year)
    
    # Save to csv to ensure data is stored
    filename = f'{csv_folder}CCMMR_Water_Balance_{LAT}_{LON}.csv'
    df_water_balance.to_csv(filename, index=False)
    return df_water_balance

# %% Calculate and visualize new data for the Google Earth Engine downloded data

def new_data_entry_GEE(LAT, LON, years, csv_folder, START_DATE = None, END_DATE = None):
    print(f'{LAT} {LON} {years}')
    
    # Define start and end date
    if START_DATE == None or END_DATE == None:
        START_DATE = f'{years[0]}-01-01'
        END_DATE = f'{years[-1]}-12-31'
    
    # Download open et data
    et_df = GEEOpenET(START_DATE, END_DATE, LAT, LON)
    
    # Download CHIRPS data year by year and concatenate
    precip_data = pd.DataFrame()
    for year in years:
        precip_data_year = CHIRPSData(year, LAT, LON)
        precip_data = pd.concat([precip_data, precip_data_year], ignore_index=True)
    
    # Interpolate et data to daily
    # Find average daily et for each month
    et_df['time'] = pd.to_datetime(et_df['time'])
    et_df['days_in_month'] = et_df['time'].dt.days_in_month
    et_df['avg_et'] = et_df['et'] / et_df['days_in_month']
    et_df.set_index('time', inplace = True)
    
    # Expand average to daily dataframe
    end_of_month = et_df.index.max() + pd.offsets.MonthEnd(0) # extend end to the end of the last month
    daily_index = pd.date_range(start = et_df.index.min(), end = end_of_month, freq = 'D') # find all days in range
    daily_et_df = et_df.reindex(daily_index) # Expand dataframe to include all days
    
    daily_et_df['avg_et'] = daily_et_df['avg_et'].ffill() # Fill in all missing values with the starting value
    #daily_et_df['avg_et'] = daily_et_df['avg_et'].interpolate(method='time') # linear interpolation
    daily_et_df = daily_et_df[['avg_et']] # select just the avegarged data
    daily_et_df = daily_et_df.rename(columns={'avg_et': 'et'})
    
    # Merge precip and et data
    precip_data['time'] = pd.to_datetime(precip_data['time'])
    precip_data.set_index('time', inplace = True)
    df_water_balance = daily_et_df.join(precip_data, how='inner') # merge with et data (only keeping values from both)
    df_water_balance = df_water_balance.reset_index().rename(columns={'index': 'time'}) # reset index so theirs a time column back
    
    # Oragaize and water balacne
    df_water_balance = CCMMF_Irrigation_CalcVis.water_balance(df_water_balance, LAT, LON)
    
    # Graph
    df_water_balance['time'] = pd.to_datetime(df_water_balance['time'])
    years = df_water_balance['time'].dt.year.unique()
    years.sort()
    
    for year in years:
        CCMMF_Irrigation_CalcVis.timeseries_graphs_GEE(df_water_balance[df_water_balance['time'].dt.year == year], LAT, LON, year)
    
    # Save to csv to ensure data is stored
    filename = f'{csv_folder}CCMMR_Water_Balance_{LAT}_{LON}_GEE.csv'
    df_water_balance.to_csv(filename, index=False)
    return df_water_balance