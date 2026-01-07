#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May 15 21:48:42 2025

@author: krein21
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
from sklearn.metrics import mean_squared_error, r2_score
import seaborn as sns

# %% Define multi use variables

# Define years to look at
years = list(range(2016, 2026))

# Define main folder
main_folder = '/projectnb/dietzelab/ccmmf/management/irrigation/'

# Define the name of the parquet filename for API
API_pq_filename = main_folder + 'CCMMF_Irrigation_Parquet'

# Define the name of the parquet filename for GEE
GEE_pq_filename = main_folder + 'CCMMF_Irrigation_Parquet_GEE'

# %% Loading data

# GEE Data Download

# Read in parquet file
# Load the full dataset
GEE_dataset = ds.dataset(GEE_pq_filename, format="parquet", partitioning = 'hive')
GEE_table = GEE_dataset.to_table()
GEE_parquet_df = GEE_table.to_pandas()

# Group by the location column and convert to dictionary
GEE_data_dict = {location: location_df for location, location_df in GEE_parquet_df.groupby("location")}

# API Data Download

# Read in parquet file
# Load the full dataset
API_dataset = ds.dataset(API_pq_filename, format="parquet", partitioning = 'hive')
API_table = API_dataset.to_table()
API_parquet_df = API_table.to_pandas()

# Group by the location column and convert to dictionary
API_data_dict = {location: location_df for location, location_df in API_parquet_df.groupby("location")}

# %% Merge API and GEE data

merged_data_dict = {}

for key in GEE_data_dict.keys():
    
    if key in API_data_dict:
        print(key)
        
        # Select both dataframes
        df_gee = GEE_data_dict[key].copy()
        df_api = API_data_dict[key].copy()
        
        # Merge dataframes
        # Use time as the connecting feature
        # Only save data if both dataframes have it
        # Specify suffixes
        merged_df = pd.merge(df_gee, df_api, on = 'time', how = 'inner', suffixes = ('_GEE', '_API'))
        
        # Add to dictionary
        merged_data_dict[key] = merged_df
        

# %% Aggregate weekly 

# Create weekly irrigation, precipitation, et data_dict
merged_data_dict_weekly = {}

for key, df in merged_data_dict.items():
    
    # Calculate new units for irrigation et and precip(cm)
    df['irr_GEE'] = df['irr_GEE'] * 0.1
    df['irr_API'] = df['irr_API'] * 0.1
    df['et_GEE'] = df['et_GEE'] * 0.1
    df['et_API'] = df['et_API'] * 0.1
    df['precip_GEE'] = df['precip_GEE'] * 0.1
    
    # Add changed units to data dict
    #merged_data_dict[key] = df
    
    # Aggregate by week
    # Sum irrigation
    weekly_df = df.groupby(['year_GEE', 'week_GEE'], as_index = False).agg({
        'time': 'first',
        'et_GEE': 'sum',
        'precip_GEE': 'sum',
        'irr_GEE': 'sum',
        'et_API': 'sum',
        'precip_API': 'sum',
        'irr_API': 'sum'
    })
    
    # Remove week column
    weekly_df = weekly_df.drop('week_GEE', axis = 1)
    
    # Add to weekly data_dict
    merged_data_dict_weekly[key] = weekly_df

# %% Predicted Observed Plots

# One of all locations

# Flatten all data into one dataframe
flattened_df = pd.concat(
    [df.assign(id = key) for key, df in merged_data_dict_weekly.items()],
    ignore_index = True
)

# Clean data
flattened_df = flattened_df.dropna(subset=['irr_API', 'irr_GEE'])

# X - weekly irrigation from API (sum weekly)
# Y -  weekly irrigation from GEE (sum weekly)
# Delineate location by color
sns.scatterplot(data = flattened_df, x = 'irr_API', y = 'irr_GEE', hue = 'id',
                s =  10, legend = False)

# 1:1 line
min_val = 0
max_val = max(max(flattened_df['irr_API']), max(flattened_df['irr_GEE']))
plt.plot([min_val, max_val], [min_val, max_val], 'r--', label='1:1 line')


# RMSE/R^2
rmse = np.sqrt(mean_squared_error(flattened_df['irr_API'], flattened_df['irr_GEE']))
r2 = r2_score(flattened_df['irr_API'], flattened_df['irr_GEE'])

# Labels
plt.xlabel('Daily Sampled Evapotranspiration Data')
plt.ylabel('Monthly Sampled Evapotranspiration Data')
plt.suptitle('Impact of Interpolation on Irrigation (cm) Calculation')
plt.title(f'RMSE = {rmse:.2f} $R^2$ = {r2:.2f}')
plt.grid(True)
plt.tight_layout()
plt.show()

# One for each location
for key, df in merged_data_dict_weekly.items():
    
    # Clean data
    df = df.dropna(subset=['irr_API', 'irr_GEE'])
    
    # Scatterplot
    sns.scatterplot(data = df, x = 'irr_API', y = 'irr_GEE', s =  10, legend = False)
    
    # 1:1 line
    min_val = 0
    max_val = max(max(df['irr_API']), max(df['irr_GEE']))
    plt.plot([min_val, max_val], [min_val, max_val], 'r--', label='1:1 line')


    # RMSE/R^2
    rmse = np.sqrt(mean_squared_error(df['irr_API'], df['irr_GEE']))
    r2 = r2_score(df['irr_API'], df['irr_GEE'])

    # Labels
    plt.xlabel('Daily Sampled Evapotranspiration Data')
    plt.ylabel('Monthly Sampled Evapotranspiration Data')
    plt.suptitle(f'Impact of Interpolation on Irrigation (cm) Calculation for {key}')
    plt.title(f'RMSE = {rmse:.2f} $R^2$ = {r2:.2f}')
    plt.grid(True)
    plt.tight_layout()
    plt.show()
    
# %% Time Series Plots

for key, df in merged_data_dict_weekly.items():
    
    # Sort by time
    df = df.sort_values(by='time')
    
    # Create cumulative sum columns
    df['irr_API_cumsum'] = df['irr_API'].cumsum()
    df['irr_GEE_cumsum'] = df['irr_GEE'].cumsum()
    df['precip_GEE_cumsum'] = df['precip_GEE'].cumsum()
    df['et_API_cumsum'] = df['et_API'].cumsum()
    df['et_GEE_cumsum'] = df['et_GEE'].cumsum()

    # Plot time series
    plt.figure(figsize=(10, 5))
    plt.plot(df['time'], df['irr_API_cumsum'], linestyle = 'dotted', lw = 2.5, color = 'royalblue', label = 'API Irrigation')
    plt.plot(df['time'], df['irr_GEE_cumsum'], linestyle = 'dotted', lw = 2.5, color = 'yellowgreen', label = 'GEE Irrigation')
    plt.plot(df['time'], df['precip_GEE_cumsum'], linestyle = 'solid', lw = 2.5, color = 'mediumpurple', label = 'Precipitation')
    plt.plot(df['time'], df['et_API_cumsum'], linestyle = 'solid', lw = 2.5, color = 'royalblue', label = 'API Evapotranspiration')
    plt.plot(df['time'], df['et_GEE_cumsum'], linestyle = 'solid', lw = 2.5, color = 'yellowgreen', label = 'GEE Evapotranspiration')

    plt.xlabel('Date')
    plt.ylabel('Cumulative Sum of Evapotransipiration, \nPrecipitation, and Irrigation (cm)')
    plt.title(f'Timeseries Impact of Interpolation on Irrigation Calculation for {key}')
    plt.legend()
    plt.grid()
    plt.show()
