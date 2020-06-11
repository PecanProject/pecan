#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Dec 10 10:43:18 2019

@author: nevalaio
"""
import ee
import time
import datetime
import satelliteTools as st
import pandas as pd
from geetools import batch, tools
import numpy as np
ee.Initialize()

#----------------- Sentinel-2 -------------------------------------
def S2_getBandData_within_bbox_single_feature(S2_timseries_dataframe, aoi_shp, AOI_id_property,AOI_id, bufferdist, datestart, dateend):
    
    bands= ['B3', 'B4','B5','B6','B7','B8A','B11','B12'] #]
 #   properties = ['cos(View_Zenith)',	'cos(Sun_Zenith)',	'cos(Rel_Azimuth)']
    start = time.time()
    image_list = {}
    crs_list = {}
    key = AOI_id
    full_assetids = "COPERNICUS/S2_SR/" + S2_timseries_dataframe[key]['assetid']
    image_list[key] = [ee.Image(a) for a in full_assetids]
    crs_list[key] = [crs for crs in S2_timseries_dataframe[key]['crs']][0]
    

    attributes = st.getShapeAtrrtibutesWithIdentifier(aoi_shp, AOI_id_property)
    feature = ee.Feature(ee.Geometry.Polygon(st.wkt2coordinates(attributes[key]['geometry'])),\
                         {'name':key, 'image_list':image_list[key], 'crs':crs_list[key]})
    
    
    if bufferdist:
        bbox = ee.Feature(feature.geometry().buffer(bufferdist).bounds(0.1,feature.get("crs")))
    else:
        bbox = ee.Feature(feature.geometry().bounds(0.1,feature.get("crs")))
    
    imageCollection = ee.ImageCollection.fromImages(feature.get("image_list"))\
                                            .filterBounds(bbox.geometry())\
                                            .filterDate(datestart,dateend)\
                                            .select(bands) 
#        imageCollection = imageCollection.map(S2_addNDVI) #lisää ja laske indeksejä tässä?
    def S2_getBandData_image_single_feature(img):
        img = img.clip(bbox.geometry())
        productid = img.get('PRODUCT_ID')
        assetid = img.get('assetid')
        tileid = img.get('MGRS_TILE')
        system_index = img.get('system:index')
        sun_azimuth = img.get('MEAN_SOLAR_AZIMUTH_ANGLE')
        sun_zenith = img.get('MEAN_SOLAR_ZENITH_ANGLE')           
        view_azimuth = ee.Array([img.get('MEAN_INCIDENCE_AZIMUTH_ANGLE_%s'%b) for b in bands]).reduce(ee.Reducer.mean(), [0]).get([0])
        view_zenith = ee.Array([img.get('MEAN_INCIDENCE_ZENITH_ANGLE_%s'%b) for b in bands]).reduce(ee.Reducer.mean(), [0]).get([0])
        
        img = img.resample('bilinear').reproject(crs=feature.get("crs"), scale=10)
         
        # get the lat lon and add the ndvi
        image_grid = ee.Image.pixelCoordinates(ee.Projection(feature.get("crs")))\
        .addBands([img.select(b) for b in bands])

        # apply reducer to list
        image_grid = image_grid.reduceRegion(
                          reducer=ee.Reducer.toList(),
                          geometry=bbox.geometry(),
                          maxPixels=1e8,
                          scale=10)
                
        # get data into arrays
        x_coords = ee.Array(image_grid.get("x"))
        y_coords = ee.Array(image_grid.get("y"))
#            band_data = []
#            [band_data.extend(b,ee.Array(image_grid.get("%s"%b))) for b in bands[:-1]]
        band_data = {b:ee.Array(image_grid.get("%s"%b)) for b in bands}
#            NDVI_array = ee.Array(image_grid.get("NDVI"))
#            B6_array = ee.Array(image_grid.get("B6"))
        
        # perform LAI et al. computation possibly here!
        
        tmpfeature = ee.Feature(ee.Geometry.Point([0,0]))\
            .set('productid', productid)\
            .set('system_index',system_index)\
            .set('assetid', assetid)\
            .set('tileid', tileid)\
            .set('crs', feature.get("crs"))\
            .set('sun_zenith',sun_zenith)\
            .set('sun_azimuth',sun_azimuth)\
            .set('view_zenith',view_zenith)\
            .set('view_azimuth',view_azimuth)\
            .set('x_coords', x_coords)\
            .set('y_coords', y_coords)\
            .set(band_data)
        return tmpfeature
        
    S2_single_feature_data = imageCollection.map(S2_getBandData_image_single_feature).getInfo()
    end = time.time()
    total_time = end - start
    print ("Processsing time in seconds %s"%total_time)
    return S2_single_feature_data

def S2_getBandData_within_aoi_single_feature(S2_timseries_dataframe, aoi_shp, AOI_id_property,AOI_id, datestart, dateend):
    
    bands= ['B3', 'B4','B5','B6','B7','B8A','B11','B12'] #]
 #   properties = ['cos(View_Zenith)',	'cos(Sun_Zenith)',	'cos(Rel_Azimuth)']
    start = time.time()
    image_list = {}
    crs_list = {}
    key = AOI_id
    full_assetids = "COPERNICUS/S2_SR/" + S2_timseries_dataframe[key]['assetid']
    image_list[key] = [ee.Image(a) for a in full_assetids]
    crs_list[key] = [crs for crs in S2_timseries_dataframe[key]['crs']][0]
    

    attributes = st.getShapeAtrrtibutesWithIdentifier(aoi_shp, AOI_id_property)
    feature = ee.Feature(ee.Geometry.Polygon(st.wkt2coordinates(attributes[key]['geometry'])),\
                         {'name':key, 'image_list':image_list[key], 'crs':crs_list[key]})
    
    

    geom = feature.geometry(0.1,feature.get("crs"))
    
    imageCollection = ee.ImageCollection.fromImages(feature.get("image_list"))\
                                            .filterBounds(geom)\
                                            .filterDate(datestart,dateend)\
                                            .select(bands) 
#        imageCollection = imageCollection.map(S2_addNDVI) #lisää ja laske indeksejä tässä?
    def S2_getBandData_image_single_feature(img):
        img = img.clip(geom)
        productid = img.get('PRODUCT_ID')
        assetid = img.get('assetid')
        tileid = img.get('MGRS_TILE')
        system_index = img.get('system:index')
        sun_azimuth = img.get('MEAN_SOLAR_AZIMUTH_ANGLE')
        sun_zenith = img.get('MEAN_SOLAR_ZENITH_ANGLE')           
        view_azimuth = ee.Array([img.get('MEAN_INCIDENCE_AZIMUTH_ANGLE_%s'%b) for b in bands]).reduce(ee.Reducer.mean(), [0]).get([0])
        view_zenith = ee.Array([img.get('MEAN_INCIDENCE_ZENITH_ANGLE_%s'%b) for b in bands]).reduce(ee.Reducer.mean(), [0]).get([0])
        
        img = img.resample('bilinear').reproject(crs=feature.get("crs"), scale=10)
         
        # get the lat lon and add the ndvi
        image_grid = ee.Image.pixelCoordinates(ee.Projection(feature.get("crs")))\
        .addBands([img.select(b) for b in bands])

        # apply reducer to list
        image_grid = image_grid.reduceRegion(
                          reducer=ee.Reducer.toList(),
                          geometry=geom,
                          maxPixels=1e8,
                          scale=10)
                
        # get data into arrays
        x_coords = ee.Array(image_grid.get("x"))
        y_coords = ee.Array(image_grid.get("y"))
#            band_data = []
#            [band_data.extend(b,ee.Array(image_grid.get("%s"%b))) for b in bands[:-1]]
        band_data = {b:ee.Array(image_grid.get("%s"%b)) for b in bands}
#            NDVI_array = ee.Array(image_grid.get("NDVI"))
#            B6_array = ee.Array(image_grid.get("B6"))
        
        # perform LAI et al. computation possibly here!
        
        tmpfeature = ee.Feature(ee.Geometry.Point([0,0]))\
            .set('productid', productid)\
            .set('system_index',system_index)\
            .set('assetid', assetid)\
            .set('tileid', tileid)\
            .set('crs', feature.get("crs"))\
            .set('sun_zenith',sun_zenith)\
            .set('sun_azimuth',sun_azimuth)\
            .set('view_zenith',view_zenith)\
            .set('view_azimuth',view_azimuth)\
            .set('x_coords', x_coords)\
            .set('y_coords', y_coords)\
            .set(band_data)
        return tmpfeature
        
    S2_single_feature_data = imageCollection.map(S2_getBandData_image_single_feature).getInfo()
    end = time.time()
    total_time = end - start
    print ("Processsing time in seconds %s"%total_time)
    return S2_single_feature_data

def S2_getBandData_single_feature_to_dict(featureDict):
    featureCollection_dict = {}
          
    for farm, featureCollection in featureDict.items():
        featureCollection_dict[farm]= {'Date': []}
        featureCollection_dict[farm].update({prop:[] for prop in featureCollection['features'][0]['properties'].keys()})
        for featnum in range(len(featureCollection['features'])):
            productid = featureCollection['features'][featnum]['properties']['productid']
            date = st.sentinelTitle2Datetime(productid)
            featureCollection_dict[farm]['Date'].append(date)
            for prop in featureCollection['features'][featnum]['properties'].keys():
                if prop is not 'Date':
                    featureCollection_dict[farm][prop].append(featureCollection['features'][featnum]['properties'][prop]) 
    return featureCollection_dict

def featureCollection_dict_to_dataframes(featureCollection_dict,props):
    dataframes = {}
    for key, item in featureCollection_dict.items():
#        dataframes[key] = pd.DataFrame({'Date': item['Date'],
#                           'lai': list(np.mean(np.array(item['lai']), axis = 1)) ,
#                           'lai_std': list(np.std(np.array(item['lai']), axis = 1)) })
        dataframes[key] = pd.DataFrame({'Date': item['Date']})
        for prop in props:
            dataframes[key][prop] = list(np.mean(np.array(item[prop]), axis = 1))
            dataframes[key][prop+'_std'] = list(np.std(np.array(item['lai']), axis = 1))        
    
    return dataframes
     
def S2_getBandData(S2_timseries_dataframe, aoi_shp, AOI_id_property, bufferdist, datestart, dateend):
    
    bands= ['B3', 'B4','B5','B6','B7','B8A','B11','B12'] #]
 #   properties = ['cos(View_Zenith)',	'cos(Sun_Zenith)',	'cos(Rel_Azimuth)']
    start = time.time()
    image_list = {}
    crs_list = {}
    
    for key, item in S2_timseries_dataframe.items():
        full_assetids = "COPERNICUS/S2_SR/" + item['assetid']
        image_list[key] = [ee.Image(a) for a in full_assetids]
        crs_list[key] = [crs for crs in item['crs']][0]
        
    attributes = st.getShapeAtrrtibutesWithIdentifier(aoi_shp, AOI_id_property)
    features = [ee.Feature(ee.Geometry.Polygon(st.wkt2coordinates(attributes[key]['geometry'])),\
                           {'name':key, 'image_list':image_list[key], 'crs':crs_list[key]}) for key,item in S2_timseries_dataframe.items()]
    featureCollection = ee.FeatureCollection(features)
    
    
    def S2_getBandData_feature(feature):
        if bufferdist:
            bbox = ee.Feature(feature.geometry().buffer(bufferdist).bounds(0.1,feature.get("crs")))
        else:
            bbox = ee.Feature(feature.geometry().bounds(0.1,feature.get("crs")))
        
        imageCollection = ee.ImageCollection.fromImages(feature.get("image_list"))\
                                                .filterBounds(bbox.geometry())\
                                                .filterDate(datestart,dateend)\
                                                .select(bands) 
#        imageCollection = imageCollection.map(S2_addNDVI) #lisää ja laske indeksejä tässä?
    
        def S2_getBandData_image(img):
            img = img.clip(bbox.geometry())
            productid = img.get('PRODUCT_ID')
            assetid = img.get('assetid')
            tileid = img.get('MGRS_TILE')
            system_index = img.get('system:index')
            sun_azimuth = img.get('MEAN_SOLAR_AZIMUTH_ANGLE')
            sun_zenith = img.get('MEAN_SOLAR_ZENITH_ANGLE')           
            view_azimuth = ee.Array([img.get('MEAN_INCIDENCE_AZIMUTH_ANGLE_%s'%b) for b in bands]).reduce(ee.Reducer.mean(), [0])
            view_zenith = ee.Array([img.get('MEAN_INCIDENCE_ZENITH_ANGLE_%s'%b) for b in bands]).reduce(ee.Reducer.mean(), [0])
            
            img = img.resample('bilinear').reproject(crs=feature.get("crs"), scale=10)
             
            # get the lat lon and add the ndvi
            image_grid = ee.Image.pixelCoordinates(ee.Projection(feature.get("crs")))\
            .addBands([img.select(b) for b in bands])

            # apply reducer to list
            image_grid = image_grid.reduceRegion(
                              reducer=ee.Reducer.toList(),
                              geometry=bbox.geometry(),
                              maxPixels=1e8,
                              scale=10)
                    
            # get data into arrays
            x_coords = ee.Array(image_grid.get("x"))
            y_coords = ee.Array(image_grid.get("y"))
#            band_data = []
#            [band_data.extend(b,ee.Array(image_grid.get("%s"%b))) for b in bands[:-1]]
            band_data = {b:ee.Array(image_grid.get("%s"%b)) for b in bands}
#            NDVI_array = ee.Array(image_grid.get("NDVI"))
#            B6_array = ee.Array(image_grid.get("B6"))
            
            # perform LAI et al. computation possibly here!
            
            tmpfeature = ee.Feature(ee.Geometry.Point([0,0]))\
                .set('productid', productid)\
                .set('system_index',system_index)\
                .set('assetid', assetid)\
                .set('tileid', tileid)\
                .set('crs', feature.get("crs"))\
                .set('sun_zenith',sun_zenith)\
                .set('sun_azimuth',sun_azimuth)\
                .set('view_zenith',view_zenith)\
                .set('view_azimuth',view_azimuth)\
                .set('x_coords', x_coords)\
                .set('y_coords', y_coords)\
                .set(band_data)
            return tmpfeature
            
        S2_image_data = imageCollection.map(S2_getBandData_image)
            
        return feature.set('productid',S2_image_data.aggregate_array('productid'))\
                .set('system_index', S2_image_data.aggregate_array('system_index'))\
                .set('assetid',S2_image_data.aggregate_array('assetid'))\
                .set('tileid',S2_image_data.aggregate_array('tileid'))\
                .set('crs',S2_image_data.aggregate_array('crs'))\
                .set('x_coords',S2_image_data.aggregate_array('x_coords'))\
                .set('y_coords',S2_image_data.aggregate_array('y_coords'))\
                .set('sun_zenith',S2_image_data.aggregate_array('sun_zenith'))\
                .set('sun_azimuth',S2_image_data.aggregate_array('sun_azimuth'))\
                .set('view_zenith',S2_image_data.aggregate_array('view_zenith'))\
                .set('view_azimuth',S2_image_data.aggregate_array('view_azimuth'))\
                .set({b:S2_image_data.aggregate_array(b) for b in bands})
        
    featureCollection = featureCollection.map(S2_getBandData_feature).getInfo()
    end = time.time()
    total_time = end - start
    print ("Processsing time in seconds %s"%total_time)
    return featureCollection

def S2_addNDVI(image):
  ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI')
  return image.addBands(ndvi)


def S2_computeNDVItimeseries(AOI_shp,AOI_id_property,datestart, dateend):
    start = time.time()
    #aoi_shp = "/home/nevalaio/Dropbox/Työura/FMI/CARBO/analysis/ruukki_blocks_wgs84.shp"
    
    attributes = st.getShapeAtrrtibutesWithIdentifier(AOI_shp,AOI_id_property)
    
    features = [ee.Feature(ee.Geometry.Polygon(st.wkt2coordinates(item['geometry'])), {'name':key}) for key,item in attributes.items()]
    featureCollection = ee.FeatureCollection(features)
    
    def S2_computeNDVItimeseries_feature(feature):
    
        area = feature.geometry()
        collection = ee.ImageCollection("COPERNICUS/S2_SR").filterBounds(area)\
                                              .filterDate(datestart,dateend)\
                                              .select(['B8','B4','SCL'])
        collection = collection.map(S2_addNDVI)
        
        def S2_computeNDVItimeseries_image(img):
#            ndvi = ee.Image(img.select(['NDVI']))
#            scl = ee.Image(img.select(['SCL']))
            productid = img.get('PRODUCT_ID')
            assetid = img.id()
            tileid = img.get('MGRS_TILE')
            system_index = img.get('system:index')
            proj = img.select("B8").projection()
            # get the lat lon and add the ndvi
#            latlon = ee.Image.pixelLonLat().addBands([scl,ndvi])
             
            # apply reducer to list
            img = img.reduceRegion(
              reducer=ee.Reducer.toList(),
              geometry=area,
              maxPixels=1e8,
              scale=10)
                    
            # get data into arrays
            classdata = ee.Array(ee.Algorithms.If(img.get("SCL"),ee.Array(img.get("SCL")),ee.Array([0])))
            ndvidata = ee.Array(ee.Algorithms.If(img.get("NDVI"),ee.Array(img.get("NDVI")),ee.Array([-9999])))
            
            classmask = classdata.eq(0).add(classdata.eq(1).add(classdata.eq(3).add(classdata.eq(7)\
                                                        .add(classdata.eq(8).add(classdata.eq(9)\
                                                        .add(classdata.eq(10).add(classdata.eq(11)\
                                                                          )))))))
            badcount = classmask.reduce(ee.Reducer.sum(),[0])
            totalcount = classmask.length()
            goodcount = totalcount.get([0]) 
    #        ndvidata_masked = ndvidata.mask(classmask.Not())
            mean = ndvidata.reduce(ee.Reducer.mean(),[0]).get([0])
            std = ndvidata.reduce(ee.Reducer.stdDev(),[0]).get([0])
            qualityUncertainty = badcount.divide(totalcount).get([0])
            
            tmpfeature = ee.Feature(ee.Geometry.Point([0,0]))\
                .set('productid', productid)\
                .set('system_index',system_index)\
                .set('assetid', assetid)\
                .set('tileid', tileid)\
                .set('projection', proj)\
                .set('sample_n', goodcount)\
                .set('ndvi_mean',mean)\
                .set('ndvi_std',std)\
                .set('quality_uncertainty',qualityUncertainty)
            return tmpfeature
    
        ndvi_timeseries = collection.map(S2_computeNDVItimeseries_image)
        
        return feature.set('productid',ndvi_timeseries.aggregate_array('productid'))\
                .set('system_index', ndvi_timeseries.aggregate_array('system_index'))\
                .set('assetid',ndvi_timeseries.aggregate_array('assetid'))\
                .set('tileid',ndvi_timeseries.aggregate_array('tileid'))\
                .set('projection',ndvi_timeseries.aggregate_array('projection'))\
                .set('sample_n',ndvi_timeseries.aggregate_array('sample_n'))\
                .set('ndvi_mean',ndvi_timeseries.aggregate_array('ndvi_mean'))\
                .set('ndvi_std',ndvi_timeseries.aggregate_array('ndvi_std'))\
                .set('quality_uncertainty',ndvi_timeseries.aggregate_array('quality_uncertainty'))   
    
    featureCollection = featureCollection.map(S2_computeNDVItimeseries_feature).getInfo()
    
    end = time.time()
    
    total_time = end - start
    print ("Processsing time in seconds %s"%total_time)
     
    return featureCollection

def S2_getTimeseriesQualityInformation(AOI_shp,AOI_id_property,datestart, dateend):
    start = time.time()
    
    attributes = st.getShapeAtrrtibutesWithIdentifier(AOI_shp,AOI_id_property)
    features = [ee.Feature(ee.Geometry.Polygon(st.wkt2coordinates(item['geometry'])), {'name':key}) for key,item in attributes.items()]
    featureCollection = ee.FeatureCollection(features)
    
    def S2_getTimeseriesQualityInformation_feature(feature):
    
        area = feature.geometry()
        collection = ee.ImageCollection("COPERNICUS/S2_SR").filterBounds(area)\
                                              .filterDate(datestart,dateend)\
                                              .select(['SCL'])
        
        def S2_getTimeseriesQualityInformation_image(img):
            productid = img.get('PRODUCT_ID')
            assetid = img.id()
            tileid = img.get('MGRS_TILE')
            system_index = img.get('system:index')
            proj = img.select("SCL").projection()
             
            # apply reducer to list
            img = img.reduceRegion(
              reducer=ee.Reducer.toList(),
              geometry=area,
              maxPixels=1e8,
              scale=10)
                    
            # get data into arrays
            classdata = ee.Array(ee.Algorithms.If(img.get("SCL"),ee.Array(img.get("SCL")),ee.Array([0])))
            
            classmask = classdata.eq(0).add(classdata.eq(1).add(classdata.eq(3).add(classdata.eq(7)\
                                                        .add(classdata.eq(8).add(classdata.eq(9)\
                                                        .add(classdata.eq(10).add(classdata.eq(11)\
                                                                          )))))))
            badcount = classmask.reduce(ee.Reducer.sum(),[0])
            totalcount = classmask.length()
            goodcount = totalcount.get([0]) 
            qualityUncertainty = badcount.divide(totalcount).get([0])
            
            tmpfeature = ee.Feature(ee.Geometry.Point([0,0]))\
                .set('productid', productid)\
                .set('system_index',system_index)\
                .set('assetid', assetid)\
                .set('tileid', tileid)\
                .set('projection', proj)\
                .set('sample_n', goodcount)\
                .set('quality_uncertainty',qualityUncertainty)
            return tmpfeature
    
        QI_timeseries = collection.map(S2_getTimeseriesQualityInformation_image)
        
        return feature.set('productid',QI_timeseries.aggregate_array('productid'))\
                        .set('system_index', QI_timeseries.aggregate_array('system_index'))\
                        .set('assetid',QI_timeseries.aggregate_array('assetid'))\
                        .set('tileid',QI_timeseries.aggregate_array('tileid'))\
                        .set('projection',QI_timeseries.aggregate_array('projection'))\
                        .set('sample_n',QI_timeseries.aggregate_array('sample_n'))\
                        .set('quality_uncertainty',QI_timeseries.aggregate_array('quality_uncertainty'))   
    
    featureCollection = featureCollection.map(S2_getTimeseriesQualityInformation_feature).getInfo()
    end = time.time()
    total_time = end - start
    print ("Processsing time in seconds %s"%total_time)   
    return featureCollection

def S2_featureCollection2Dataframe(featureCollection):
    dataframes = {}
    
    for featnum in range(len(featureCollection['features'])):
        featureCollection_dict = {}
        key = featureCollection['features'][featnum]['properties']['name']
        productid = featureCollection['features'][featnum]['properties']['productid']
        projections = featureCollection['features'][featnum]['properties']['projection']
        crs = [d['crs'] for d in projections]
        
        dates = [st.sentinelTitle2Datetime(d) for d in productid]
        
        featureCollection_dict.update({'Date': dates, 'crs': crs})
        for prop, data in featureCollection['features'][featnum]['properties'].items():
            if prop not in ['Date','crs','projection']:
                featureCollection_dict.update({prop: data})        
        dataframes[key] = pd.DataFrame(featureCollection_dict)
    return dataframes

def S2_NDVIfeatureCollection2Dataframe(featureCollection):
    dataframes = {}
    
    for featnum in range(len(featureCollection['features'])):
        key = featureCollection['features'][featnum]['properties']['name']
        productid = featureCollection['features'][featnum]['properties']['productid']
#        dates = [datetime.datetime.strptime(d.split('_')[1], '%Y%m%dT%H%M%S') for d in dataid]
        projections = featureCollection['features'][featnum]['properties']['projection']
        crs = [d['crs'] for d in projections]
        
        dates = [st.sentinelTitle2Datetime(d) for d in productid]

        featureCollection_dict= {'Date': dates,
                                'productid': productid,
                                'system_index': featureCollection['features'][featnum]['properties']['system_index'],
                                'assetid': featureCollection['features'][featnum]['properties']['assetid'],
                                'tileid': featureCollection['features'][featnum]['properties']['tileid'],
                                'crs': crs,                                
                                'sample_n': featureCollection['features'][featnum]['properties']['sample_n'],                                    
                                'ndvi_mean': featureCollection['features'][featnum]['properties']['ndvi_mean'],
                                'ndvi_std': featureCollection['features'][featnum]['properties']['ndvi_std'],
                                'quality_uncertainty': featureCollection['features'][featnum]['properties']['quality_uncertainty']       
                                }
        dataframes[key] = pd.DataFrame(featureCollection_dict, columns= ['Date','productid','system_index','assetid','tileid','crs','sample_n','ndvi_mean','ndvi_std','quality_uncertainty'])
    return dataframes


def S2_exportImageCollection(assetIDs, aoi):
    assetIDs = ["COPERNICUS/S2_SR/" + a for a in assetIDs]
    images = [ee.Image(assetid) for assetid in assetIDs]
    imageCollection = ee.ImageCollection(images)
    aoi = ee.Feature(ee.Geometry.Polygon(st.wkt2coordinates(aoi)))
    batch.imagecollection.toDrive(imageCollection, 'FOLDER', region=tools.geometry.getRegion(aoi),  scale=10, verbose=True) 
#----------------- LANDSAT-8 -------------------------------------

def L8_addNDVI(image):
  ndvi = image.normalizedDifference(['B5', 'B4']).rename('NDVI')
  return image.addBands(ndvi)


def L8_computeNDVItimeseries(AOI_shp,AOI_id_property,datestart, dateend):
    
    start = time.time()
    attributes = st.getShapeAtrrtibutesWithIdentifier(AOI_shp,AOI_id_property)
    features = [ee.Feature(ee.Geometry.Polygon(st.wkt2coordinates(item['geometry'])), {'name':key}) for key,item in attributes.items()]
    featureCollection = ee.FeatureCollection(features)
    
    def L8_comuteNDVItimeseries_feature(feature):
    
        area = feature.geometry()
        
        collection = ee.ImageCollection("LANDSAT/LC08/C01/T1_SR").filterBounds(area)\
                                              .filterDate(datestart,dateend)\
                                              .select(['B5','B4','pixel_qa'])
                                              
        collection = collection.map(L8_addNDVI)
        
        def L8_computeNDVItimeseries_image(img):
#            ndvi = ee.Image(img.select(['NDVI']))
            dataid = img.get('LANDSAT_ID')
            sensingtime = img.get('SENSING_TIME')
#            qa = ee.Image(img.select(['pixel_qa']))
             
            # get the lat lon and add the ndvi
#            latlon = ee.Image.pixelLonLat().addBands([qa, ndvi])
             
            # apply reducer to list
            img = img.reduceRegion(
              reducer=ee.Reducer.toList(),
              geometry=area,
              maxPixels=1e8,
              scale=30);
            # get data into arrays
            classdata = ee.Array(ee.Algorithms.If(img.get("pixel_qa"),ee.Array(img.get("pixel_qa")),ee.Array([0])))
            ndvidata = ee.Array(ee.Algorithms.If(img.get("NDVI"),ee.Array(img.get("NDVI")),ee.Array([-9999])))
            
#            classdata = ee.Array(latlon.get("pixel_qa"))
#            ndvidata = ee.Array(latlon.get("NDVI"))
            
            mean = ndvidata.reduce(ee.Reducer.mean(),[0]).get([0])
            std = ndvidata.reduce(ee.Reducer.stdDev(),[0]).get([0])
            classmask = classdata.eq(322).Or(classdata.eq(386)).Not()
            badcount = classmask.reduce(ee.Reducer.sum(),[0])
            totalcount = classmask.length()
            qualityUncertainty = badcount.divide(totalcount).get([0])
            
            tmpfeature = ee.Feature(ee.Geometry.Point([0,0]))\
                .set('dataid',dataid)\
                .set('sensing_time', sensingtime)\
                .set('ndvi_mean',mean)\
                .set('ndvi_std',std)\
                .set('quality_uncertainty',qualityUncertainty)
            return tmpfeature
    
        ndvi_timeseries = collection.map(L8_computeNDVItimeseries_image)
        
        return feature.set('dataid',ndvi_timeseries.aggregate_array('dataid'))\
                .set('sensing_time',ndvi_timeseries.aggregate_array('sensing_time'))\
                .set('ndvi_mean',ndvi_timeseries.aggregate_array('ndvi_mean'))\
                .set('ndvi_std',ndvi_timeseries.aggregate_array('ndvi_std'))\
                .set('quality_uncertainty',ndvi_timeseries.aggregate_array('quality_uncertainty'))   
    
    featureCollection = featureCollection.map(L8_comuteNDVItimeseries_feature).getInfo()
    
    end = time.time()
    
    total_time = end - start
    print ("Processsing time in seconds %s"%total_time)
    return featureCollection


def L8_featureCollection2Dataframe(L8_featureCollection):
    dataframes = {}
    
    for featnum in range(len(L8_featureCollection['features'])):
    
        key = L8_featureCollection['features'][featnum]['properties']['name']   
        dataid = L8_featureCollection['features'][featnum]['properties']['dataid']
        dates = [datetime.datetime.strptime(d.split('.')[0], '%Y-%m-%dT%H:%M:%S') for d in L8_featureCollection['features'][featnum]['properties']['sensing_time']]
        
        featureCollection_dict= {'Date': dates,
                                        'dataid':dataid,
                                        'ndvi_mean':L8_featureCollection['features'][featnum]['properties']['ndvi_mean'],
                                        'ndvi_std':L8_featureCollection['features'][featnum]['properties']['ndvi_std'],
                                        'quality_uncertainty':L8_featureCollection['features'][featnum]['properties']['quality_uncertainty']
                                        }
        
        dataframes[key] = pd.DataFrame(featureCollection_dict, columns= ['Date','dataid','ndvi_mean','ndvi_std','quality_uncertainty'])
    return dataframes




