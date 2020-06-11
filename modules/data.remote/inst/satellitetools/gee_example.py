#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jun  2 16:30:39 2020

@author: Olli Nevalainen (olli.nevalainen@fmi.fi),
 Finnish Meteorological Institute)

Example of GEE usage
"""
import satellitetools.gee as gee
import satellitetools.biophys_xarray as bio
import geopandas as gpd
import os

#%%
out_dir = "output/"
if not os.path.exists(out_dir):
    os.makedirs(out_dir, exist_ok=True)

geomfile = "geometry-files/ruukki_blocks_wgs84.shp"

datestart = "2019-01-01"
dateend = "2019-12-31"
qi_threshold = 0.02

# the example geometry file has eight different polygons
df = gpd.read_file(geomfile)

request = gee.S2RequestParams(datestart, dateend)


areas = []
for block in df.itertuples(index=True, name="Block"):
    aoi = gee.AOI(block.Name, block.geometry)
    areas.append(aoi)

print('Computing S2 quality information...')
gee.ee_get_s2_quality_info(areas, request)
print('Retrieving S2 data...')
gee.ee_get_s2_data(areas, request, qi_threshold=qi_threshold)

#%%
for aoi in areas:
    gee.s2_data_to_xarray(aoi, request)
    # compute NDVI, fapar and LAI
    aoi.data = bio.compute_ndvi(aoi.data)
    aoi.data = bio.run_snap_biophys(aoi.data, 'LAI')
    aoi.data = bio.run_snap_biophys(aoi.data, 'FAPAR')

#%%
timeseries = {}
timeseries_variables = ['lai', 'fapar', 'ndvi']
for aoi in areas:
    aoi.data.to_netcdf(os.path.join(out_dir, aoi.name + 'example_netcdf.nc'))
    # timeseries
    timeseries[aoi.name] = gee.xr_dataset_to_timeseries(
        aoi.data, timeseries_variables)
