#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 23 14:46:51 2025

@author: krein21
"""
# %% Import modules

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# %% Turn raw data into usable data

def water_balance(df_water_balance, LAT, LON):
    print(f'{LAT} {LON}')

    # Handle NAs
    df_water_balance['et'] = df_water_balance['et'].fillna(0)
    df_water_balance['precip'] = df_water_balance['precip'].fillna(0)
    
    # Constants
    WHC = 500 # units: mm
    W_min = 0.15 * WHC
    field_capacity = WHC/2
    
    # Water Balance Equation
    df_water_balance['W_t'] = field_capacity
    
    for row_number in range(1,len(df_water_balance)):
        
        # Pull all data
        W_tminusone = df_water_balance['W_t'].iloc[row_number - 1]
        precip = df_water_balance['precip'].iloc[row_number] 
        et = df_water_balance['et'].iloc[row_number]
        
        # Calculate initial W_t
        # W_t = W_t-1 + P_t - ET_t
        W_t_initial = W_tminusone + precip - et
        
        # Calculate irrigation
        # Irr_t = max(Wmin - W_t, 0)
        irr = max(W_min - W_t_initial, 0)
        
        # Calculate runoff
        # Qt = max(Wt - WHC, 0)
        runoff = max(W_t_initial - WHC, 0)
        
        # Calculate final W_t
        # W_t = W_t-1 + P_t + Irr_t - ET_t - Q_t
        W_t = W_tminusone + precip + irr - et - runoff
        
        # Add values to dataframe
        df_water_balance.loc[row_number, 'W_t'] = W_t
        df_water_balance.loc[row_number, 'irr'] = irr
        df_water_balance.loc[row_number, 'runoff'] = runoff
        
    # Add year, day and week values
    df_water_balance['time'] = pd.to_datetime(df_water_balance['time'])
    df_water_balance['year'] = df_water_balance['time'].dt.year
    df_water_balance['week'] = df_water_balance['time'].dt.isocalendar().week
    df_water_balance['day_of_year'] = df_water_balance['time'].dt.dayofyear

    return df_water_balance


# %% Time Series

def timeseries_graphs_API(df_water_balance, LAT, LON, YEAR):
    
    # Slicing warning if not copied
    df_water_balance = df_water_balance.copy()

    # Create cumulative sum columns
    df_water_balance['et_cumsum'] = df_water_balance['et'].cumsum()
    df_water_balance['precip_cumsum'] = df_water_balance['precip'].cumsum()
    df_water_balance['irr_cumsum'] = df_water_balance['irr'].cumsum()
    
    # Ensure time is dates
    df_water_balance['time'] = pd.to_datetime(df_water_balance['time'])
    
    # Plot time series
    plt.figure(figsize=(10, 5))
    plt.plot(df_water_balance['time'], df_water_balance['et_cumsum'], linestyle = 'dotted', lw = 2.5, label = 'Evapotranspiration')
    plt.plot(df_water_balance['time'], df_water_balance['precip_cumsum'], linestyle = 'dashed', lw = 2.5, label = 'Precipitation')
    plt.plot(df_water_balance['time'], df_water_balance['irr_cumsum'], linestyle = 'dashdot', lw = 2.5, label = 'Irrigation')
    plt.plot(df_water_balance['time'], df_water_balance['runoff'], linestyle = 'solid', lw = 2.5, label = 'Runoff')
    
    plt.xlabel('Date')
    plt.ylabel('Cumulative Sum of Evapotransipiration, \nPrecipitation, and Irrigation (mm)')
    plt.suptitle('Evapotransipiration and Precipitation Time Series in Central Valley CA')
    plt.title(f'(Lat: {LAT}, Lon: {LON})')
    plt.legend()
    plt.grid()
    
    # Save plot
    filename = f'/projectnb/dietzelab/ccmmf/management/irrigation/TimeseriesPNG/CCMMR_et_precip_irr_cumsum_{YEAR}_{LAT}_{LON}.png'
    plt.savefig(filename)
    
    plt.show()
    
def timeseries_graphs_GEE(df_water_balance, LAT, LON, YEAR):
    
    # Slicing warning if not copied
    df_water_balance = df_water_balance.copy()

    # Create cumulative sum columns
    df_water_balance['et_cumsum'] = df_water_balance['et'].cumsum()
    df_water_balance['precip_cumsum'] = df_water_balance['precip'].cumsum()
    df_water_balance['irr_cumsum'] = df_water_balance['irr'].cumsum()
    
    # Ensure time is dates
    df_water_balance['time'] = pd.to_datetime(df_water_balance['time'])
    
    # Plot time series
    plt.figure(figsize=(10, 5))
    plt.plot(df_water_balance['time'], df_water_balance['et_cumsum'], linestyle = 'dotted', lw = 2.5, label = 'Evapotranspiration')
    plt.plot(df_water_balance['time'], df_water_balance['precip_cumsum'], linestyle = 'dashed', lw = 2.5, label = 'Precipitation')
    plt.plot(df_water_balance['time'], df_water_balance['irr_cumsum'], linestyle = 'dashdot', lw = 2.5, label = 'Irrigation')
    plt.plot(df_water_balance['time'], df_water_balance['runoff'], linestyle = 'solid', lw = 2.5, label = 'Runoff')
    
    plt.xlabel('Date')
    plt.ylabel('Monthly Cumulative Sum of Evapotransipiration, \nPrecipitation, and Irrigation (mm)')
    plt.suptitle('Evapotransipiration and Precipitation Time Series in Central Valley CA')
    plt.title(f'(Lat: {LAT}, Lon: {LON})')
    plt.legend()
    plt.grid()
    
    # Save plot
    filename = f'/projectnb/dietzelab/ccmmf/management/irrigation/TimeseriesPNG_GEE/CCMMR_GEE_cumsum_{YEAR}_{LAT}_{LON}.png'
    plt.savefig(filename)
    
    plt.show()