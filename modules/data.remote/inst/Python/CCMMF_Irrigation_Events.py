#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 24 12:15:39 2025

@author: krein21

Columns:
    - loc: spatial location index (starts at 0)
    - year: year of start of this timestep 
    - day: day of start of this timestep (1 - 366)
    - event_type: type of event
    - amount_added (cm/day)
    - type: (0 = canopy, 1 = soil, 2 = flood)

"""

# %% Import modules

import pandas as pd


# %% Create event file

def file_creation(data_dict):
    
    # Create an event file for each location
    for key, df in data_dict.items():
        
        # Add columns
        df['event_type'] = 'irrig'
        df['loc'] = 0
        df['type'] = 1
        
        # Calculate new units for irrigation
        df['irr'] = df['irr'] * 0.1
        
        # Aggregate by week
        # Sum irrigation
        eventfile_df = df.groupby(['year', 'week'], as_index = False).agg({
            'loc': 'first',
            'year': 'first',
            'day_of_year': 'first',
            'event_type': 'first',
            'irr': 'sum',
            'type': 'first'
        })
        
        # Remove week column
        eventfile_df = eventfile_df.drop('week', axis = 1)
        
        # Drop all 0 irrigation rows
        eventfile_df = eventfile_df[eventfile_df['irr'] != 0]
        
        # Write to file(s)
        folder_name = '/projectnb/dietzelab/ccmmf/management/irrigation/CCMMF_Irrigation_EventFiles/'
        filename = f'{folder_name}irrigation_eventfile_{key}.txt'
        eventfile_df.to_csv(filename, sep = ' ', index = False, header = False)

    

