#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar  6 13:59:07 2025

@author: katherineanne
"""
# %% Import modules

import requests
import numpy as np
from netCDF4 import Dataset, num2date
import matplotlib.pyplot as plt
import pandas as pd
import os
from datetime import datetime, date, timedelta
import pyarrow as pa
import pyarrow.parquet as pq
import pyarrow.dataset as ds
import ee
import CCMMF_Irrigation_DataDownload
import CCMMF_Irrigation_CalcVis
import CCMMF_Irrigation_Events

ee.Initialize()

# %% Define multi use variables

# Define years to look at
years = list(range(2016, 2026))

# Define main folder
main_folder = '/projectnb/dietzelab/ccmmf/management/irrigation/'

# Define folder name for csv files
csv_folder = main_folder + 'WaterBalanceCSV_GEE/'

# Define the name of the parquet filename
pq_filename = main_folder + 'CCMMF_Irrigation_Parquet_GEE'

# %% Loading data

# Read in parquet file
# Load the full dataset
dataset = ds.dataset(pq_filename, format="parquet", partitioning = 'hive')
table = dataset.to_table()
parquet_df = table.to_pandas()
days_to_download = 0

# Group by the location column and convert to dictionary
data_dict = {location: location_df for location, location_df in parquet_df.groupby("location")}

# %% Check current date with most current downloaded data

# Delete the current CHIRPS file for this year
# This will ensure we read in the new data for the current date
# We only do this if the data is not up to date
cur_year = datetime.now().year
today = datetime.now().date()
chirps_filename = f'{main_folder}chirps-v2.0.{cur_year}.days_p05.nc'

if os.path.exists(chirps_filename):
    with Dataset(chirps_filename, 'r') as nc:
        
        time_var = nc.variables['time']
        dates = num2date(time_var[:], units=time_var.units)
        most_recent = max(dates)
        most_recent_date = date(most_recent.year, most_recent.month, most_recent.day)

        if most_recent_date != today:
            print('Deleted')
            days_to_download = (today - most_recent_date).days
            os.remove(chirps_filename)

# %% Define locations

# Read in all lat lons
df_lat_lon = pd.read_csv(f'{main_folder}design_points.csv')

# Handle duplicates
df_lat_lon = df_lat_lon.drop_duplicates()

# %% Iterate through locations and download data for each

for row_number in range(5):
    
    # Load location data
    latitude = df_lat_lon['lat'].iloc[row_number]
    longitude = df_lat_lon['lon'].iloc[row_number]
    location = df_lat_lon['id'].iloc[row_number]
    
    # Create CSV name
    csv_filename = f'{csv_folder}CCMMR_Water_Balance_{latitude}_{longitude}_GEE.csv'

    if location in data_dict:
        
        df = data_dict[location]
        
        # If we have not downloaded data for today yet...
        if days_to_download != 0:
            # Download new data
            start_date = today - timedelta(days=days_to_download)
            new_df = CCMMF_Irrigation_DataDownload.new_data_entry_GEE(latitude, longitude, 
                                                                      [start_date.year, cur_year],
                                                                      csv_folder, start_date, today)
            
            # Concatenate with already saved data
            old_df = data_dict[location]
            df = pd.concat([new_df, old_df], ignore_index=True)
            df = df.sort_values(by='time')
            data_dict[location] = df
            
            # Save data
            df.to_csv(csv_filename, index=False)
            
        # Check that all years have been read in
        df['time'] = pd.to_datetime(df['time'])
        df_years = df['time'].dt.year.unique().tolist()
        
        if set(df_years) != set(years):
            
            # Years in what years we want but not in saved data
            # Does not care if there are values in saved data that are not in wanted years
            not_saved_years = set(years) - set(df_years)
            not_saved_years = list(not_saved_years)
            
            # Download data and calculate for new years
            new_df = CCMMF_Irrigation_DataDownload.new_data_entry_GEE(latitude, longitude,
                                                                      not_saved_years, csv_folder)
            
            # Concatenate with already saved data
            old_df = data_dict[location]
            df = pd.concat([new_df, old_df], ignore_index=True)
            df = df.sort_values(by='time')
            data_dict[location] = df
            
            # Save data
            df.to_csv(csv_filename, index=False)
    
    # The location is not in the saved dictionary
    else:
        # Download and calculate if it doesn't exist
        df = CCMMF_Irrigation_DataDownload.new_data_entry_GEE(latitude, longitude,
                                                              years, csv_folder)
        data_dict[location] = df
        
        # Save data
        df.to_csv(csv_filename, index=False)

# %% Create Event Files

#CCMMF_Irrigation_Events.file_creation(data_dict)

# %% Write to parquet

for location, loc_df in data_dict.items():
    loc_df['location'] = location
    table = pa.Table.from_pandas(loc_df)
    pq.write_to_dataset(table, root_path = pq_filename, partition_cols = ['location', 'year'])