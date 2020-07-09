#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb  6 15:24:12 2020

Module to retrieve Sentinel-2 data from Google Earth Engine (GEE).

@author: Olli Nevalainen (olli.nevalainen@fmi.fi),
 Finnish Meteorological Institute)


"""
import sys
import os
import ee
import datetime
import pandas as pd
import geopandas as gpd
import numpy as np
import xarray as xr
from functools import reduce
from gee_utils import calc_ndvi

ee.Initialize()


NO_DATA = -99999
S2_REFL_TRANS = 10000
# ----------------- Sentinel-2 -------------------------------------
s2_qi_labels = [
    "NODATA",
    "SATURATED_DEFECTIVE",
    "DARK_FEATURE_SHADOW",
    "CLOUD_SHADOW",
    "VEGETATION",
    "NOT_VEGETATED",
    "WATER",
    "UNCLASSIFIED",
    "CLOUD_MEDIUM_PROBA",
    "CLOUD_HIGH_PROBA",
    "THIN_CIRRUS",
    "SNOW_ICE",
]

s2_filter1 = [
    "NODATA",
    "SATURATED_DEFECTIVE",
    "CLOUD_SHADOW",
    "UNCLASSIFIED",
    "CLOUD_MEDIUM_PROBA",
    "CLOUD_HIGH_PROBA",
    "THIN_CIRRUS",
    "SNOW_ICE",
]


class S2RequestParams:
    """S2 data request paramaters.

    Attributes
    ----------
    datestart : str
        Starting date for data request in form "2019-01-01".
    dateend : str
        Starting date for data request in form "2019-12-31".
    scale : int
        pixel resolution
    bands : list, optional
        List of strings with band name.
        the default is ['B3', 'B4', 'B5',
        'B6', 'B7', 'B8A', 'B11', 'B12'].
    """

    def __init__(self, datestart, dateend, scale, bands=None):
        """.

        Parameters
        ----------
        datestart : str
            Starting date for data request in form "2019-01-01".
        dateend : str
            Starting date for data request in form "2019-12-31".
        scale : int
            pixel resolution
        bands : list, optional
            List of strings with band name.

        Returns
        -------
        None.

        """
        default_bands = [
            "B1",
            "B2",
            "B3",
            "B4",
            "B5",
            "B6",
            "B7",
            "B8",
            "B8A",
            "B9",
            "B11",
            "B12",
        ]

        self.datestart = datestart
        self.dateend = dateend
        self.scale = scale
        self.bands = bands if bands else default_bands


class AOI:
    """Area of interest for area info and data.

    Attributes
    ----------
    name : str
        Name of the area.
    geometry : str
        Geometry of the area of interest e.g. from geopandas.
        Currently only polygons tested. The default is None.
    coordinate_list : list, optional
        List of coordinates of a polygon
        (loop should be closed). Computed from geometry if not
        provided. The default is None.
    tile : str, optional
        Tile id as string for the data. Used to keep the data in
        same crs because an area can be in multiple tiles with
        different crs. The default is None.
    qi : pandas dataframe
        Dataframe with quality information about available imagery for the AOI.
        qi is empty at init and can be computed with
        ee_get_s2_quality_info function.
    data : pandas dataframe or xarray
        Dataframe holding data retrieved from GEE. Data can be computed using
        function
        qi is empty at init and can be computed with ee_get_s2_data and
        converted to xarray using s2_data_to_xarray function.

    Methods
    -------
    __init__
    """

    def __init__(self, name, geometry=None, coordinate_list=None, tile=None):
        """.

        Parameters
        ----------
        name : str
            Name of the area.
        geometry : geometry in wkt, optional
            Geometry of the area of interest e.g. from geopandas.
            Currently only polygons tested. The default is None.
        coordinate_list : list, optional
            List of coordinates of a polygon
            (loop should be closed). Computed from geometry if not
            provided. The default is None.
        tile : str, optional
            Tile id as string for the data. Used to keep the data in
            same crs because an area can be in multiple tiles with
            different crs. The default is None.

        Returns
        -------
        None.

        """
        if not geometry and not coordinate_list:
            sys.exit("AOI has to get either geometry or coordinates as list!")
        elif geometry and not coordinate_list:
            coordinate_list = list(geometry.exterior.coords)
        elif coordinate_list and not geometry:
            geometry = None

        self.name = name
        self.geometry = geometry
        self.coordinate_list = coordinate_list
        self.qi = None
        self.data = None
        self.tile = tile


def ee_get_s2_quality_info(AOIs, req_params):
    """Get S2 quality information from GEE.

    Parameters
    ----------
    AOIs : list or AOI instance
        List of AOI instances or single AOI instance. If multiple AOIs
        proviveded the computation in GEE server is parallellized.
        If too many areas with long time range is provided, user might
        hit GEE memory limits. Then you should call this function
        sequentally to all AOIs.
    req_params : S2RequestParams instance
        S2RequestParams instance with request details.

    Returns
    -------
    Nothing:
        Computes qi attribute for the given AOI instances.

    """
    # if single AOI instance, make a list
    if isinstance(AOIs, AOI):
        AOIs = list([AOIs])

    features = [
        ee.Feature(ee.Geometry.Polygon(a.coordinate_list), {"name": a.name})
        for a in AOIs
    ]
    feature_collection = ee.FeatureCollection(features)

    def ee_get_s2_quality_info_feature(feature):

        area = feature.geometry()
        image_collection = (
            ee.ImageCollection("COPERNICUS/S2_SR")
            .filterBounds(area)
            .filterDate(req_params.datestart, req_params.dateend)
            .select(["SCL"])
        )

        def ee_get_s2_quality_info_image(img):
            productid = img.get("PRODUCT_ID")
            assetid = img.id()
            tileid = img.get("MGRS_TILE")
            system_index = img.get("system:index")
            proj = img.select("SCL").projection()

            # apply reducer to list
            img = img.reduceRegion(
                reducer=ee.Reducer.toList(),
                geometry=area,
                maxPixels=1e8,
                scale=req_params.scale,
            )

            # get data into arrays
            classdata = ee.Array(
                ee.Algorithms.If(
                    img.get("SCL"), ee.Array(img.get("SCL")), ee.Array([0])
                )
            )

            totalcount = classdata.length()
            classpercentages = {
                key: classdata.eq(i)
                .reduce(ee.Reducer.sum(), [0])
                .divide(totalcount)
                .get([0])
                for i, key in enumerate(s2_qi_labels)
            }

            tmpfeature = (
                ee.Feature(ee.Geometry.Point([0, 0]))
                .set("productid", productid)
                .set("system_index", system_index)
                .set("assetid", assetid)
                .set("tileid", tileid)
                .set("projection", proj)
                .set(classpercentages)
            )
            return tmpfeature

        s2_qi_image_collection = image_collection.map(ee_get_s2_quality_info_image)

        return (
            feature.set(
                "productid", s2_qi_image_collection.aggregate_array("productid")
            )
            .set("system_index", s2_qi_image_collection.aggregate_array("system_index"))
            .set("assetid", s2_qi_image_collection.aggregate_array("assetid"))
            .set("tileid", s2_qi_image_collection.aggregate_array("tileid"))
            .set("projection", s2_qi_image_collection.aggregate_array("projection"))
            .set(
                {
                    key: s2_qi_image_collection.aggregate_array(key)
                    for key in s2_qi_labels
                }
            )
        )

    s2_qi_feature_collection = feature_collection.map(
        ee_get_s2_quality_info_feature
    ).getInfo()

    s2_qi = s2_feature_collection_to_dataframes(s2_qi_feature_collection)

    for a in AOIs:
        name = a.name
        a.qi = s2_qi[name]


def ee_get_s2_data(AOIs, req_params, qi_threshold=0, qi_filter=s2_filter1):
    """Get S2 data (level L2A, bottom of atmosphere data) from GEE.

    Parameters
    ----------
    AOIs : list or AOI instance
        List of AOI instances or single AOI instance. If multiple AOIs
        proviveded the computation in GEE server is parallellized.
        If too many areas with long time range is provided, user might
        hit GEE memory limits. Then you should call this function
        sequentally to all AOIs. AOIs should have qi attribute computed first.
    req_params : S2RequestParams instance
        S2RequestParams instance with request details.
    qi_threshold : float
        Threshold value to filter images based on used qi filter.
        qi filter holds labels of classes whose percentages within the AOI
        is summed. If the sum is larger then the qi_threhold, data will not be
        retrieved for that date/image. The default is 1, meaning all data is
        retrieved.
    qi_filter : list
        List of strings with class labels (of unwanted classes) used to compute qi value,
        see qi_threhold. The default is s2_filter1 = ['NODATA',
              'SATURATED_DEFECTIVE',
              'CLOUD_SHADOW',
              'UNCLASSIFIED',
              'CLOUD_MEDIUM_PROBA',
              'CLOUD_HIGH_PROBA',
              'THIN_CIRRUS',
              'SNOW_ICE'].

    Returns
    -------
    Nothing:
        Computes data attribute for the given AOI instances.

    """
    datestart = req_params.datestart
    dateend = req_params.dateend
    bands = req_params.bands
    # if single AOI instance, make a list
    if isinstance(AOIs, AOI):
        AOIs = list([AOIs])

    features = []
    for a in AOIs:
        filtered_qi = filter_s2_qi_dataframe(a.qi, qi_threshold, qi_filter)
        if len(filtered_qi) == 0:
            print("No observations to retrieve for area %s" % a.name)
            continue

        if a.tile is None:
            min_tile = min(filtered_qi["tileid"].values)
            filtered_qi = filtered_qi[filtered_qi["tileid"] == min_tile]
            a.tile = min_tile
        else:
            filtered_qi = filtered_qi[filtered_qi["tileid"] == a.tile]

        full_assetids = "COPERNICUS/S2_SR/" + filtered_qi["assetid"]
        image_list = [ee.Image(asset_id) for asset_id in full_assetids]
        crs = filtered_qi["projection"].values[0]["crs"]
        feature = ee.Feature(
            ee.Geometry.Polygon(a.coordinate_list),
            {"name": a.name, "image_list": image_list},
        )

        features.append(feature)

    if len(features) == 0:
        print("No data to be retrieved!")
        return None

    feature_collection = ee.FeatureCollection(features)

    def ee_get_s2_data_feature(feature):
        geom = feature.geometry(0.01, crs)
        image_collection = (
            ee.ImageCollection.fromImages(feature.get("image_list"))
            .filterBounds(geom)
            .filterDate(datestart, dateend)
            #      .select(bands + ["SCL"])
        )
        image_collection = image_collection.map(calc_ndvi(nir="B8A", red="B4")).select(
            bands + ["SCL", "NDVI"]
        )

        def ee_get_s2_data_image(img):
            # img = img.clip(geom)
            productid = img.get("PRODUCT_ID")
            assetid = img.id()
            tileid = img.get("MGRS_TILE")
            system_index = img.get("system:index")
            proj = img.select(bands[0]).projection()
            sun_azimuth = img.get("MEAN_SOLAR_AZIMUTH_ANGLE")
            sun_zenith = img.get("MEAN_SOLAR_ZENITH_ANGLE")
            view_azimuth = (
                ee.Array(
                    [img.get("MEAN_INCIDENCE_AZIMUTH_ANGLE_%s" % b) for b in bands]
                )
                .reduce(ee.Reducer.mean(), [0])
                .get([0])
            )
            view_zenith = (
                ee.Array([img.get("MEAN_INCIDENCE_ZENITH_ANGLE_%s" % b) for b in bands])
                .reduce(ee.Reducer.mean(), [0])
                .get([0])
            )

            img = img.resample("bilinear").reproject(crs=crs, scale=req_params.scale)

            # get the lat lon and add the ndvi
            image_grid = ee.Image.pixelCoordinates(ee.Projection(crs)).addBands(
                [img.select(b) for b in bands + ["SCL", "NDVI"]]
            )

            # apply reducer to list
            image_grid = image_grid.reduceRegion(
                reducer=ee.Reducer.toList(),
                geometry=geom,
                maxPixels=1e8,
                scale=req_params.scale,
            )

            # get data into arrays
            x_coords = ee.Array(image_grid.get("x"))
            y_coords = ee.Array(image_grid.get("y"))
            band_data = {b: ee.Array(image_grid.get("%s" % b)) for b in bands}

            scl_data = ee.Array(image_grid.get("SCL"))
            ndvi_data = ee.Array(image_grid.get("NDVI"))

            # perform LAI et al. computation possibly here!

            tmpfeature = (
                ee.Feature(ee.Geometry.Point([0, 0]))
                .set("productid", productid)
                .set("system_index", system_index)
                .set("assetid", assetid)
                .set("tileid", tileid)
                .set("projection", proj)
                .set("sun_zenith", sun_zenith)
                .set("sun_azimuth", sun_azimuth)
                .set("view_zenith", view_zenith)
                .set("view_azimuth", view_azimuth)
                .set("x_coords", x_coords)
                .set("y_coords", y_coords)
                .set("SCL", scl_data)
                .set("NDVI", ndvi_data)
                .set(band_data)
            )
            return tmpfeature

        s2_data_feature = image_collection.map(ee_get_s2_data_image)

        return (
            feature.set("productid", s2_data_feature.aggregate_array("productid"))
            .set("system_index", s2_data_feature.aggregate_array("system_index"))
            .set("assetid", s2_data_feature.aggregate_array("assetid"))
            .set("tileid", s2_data_feature.aggregate_array("tileid"))
            .set("projection", s2_data_feature.aggregate_array("projection"))
            .set("sun_zenith", s2_data_feature.aggregate_array("sun_zenith"))
            .set("sun_azimuth", s2_data_feature.aggregate_array("sun_azimuth"))
            .set("view_zenith", s2_data_feature.aggregate_array("view_zenith"))
            .set("view_azimuth", s2_data_feature.aggregate_array("view_azimuth"))
            .set("x_coords", s2_data_feature.aggregate_array("x_coords"))
            .set("y_coords", s2_data_feature.aggregate_array("y_coords"))
            .set("SCL", s2_data_feature.aggregate_array("SCL"))
            .set("NDVI", s2_data_feature.aggregate_array("NDVI"))
            .set({b: s2_data_feature.aggregate_array(b) for b in bands})
        )

    s2_data_feature_collection = feature_collection.map(
        ee_get_s2_data_feature
    ).getInfo()

    s2_data = s2_feature_collection_to_dataframes(s2_data_feature_collection)

    for a in AOIs:
        name = a.name
        a.data = s2_data[name]


def filter_s2_qi_dataframe(s2_qi_dataframe, qi_thresh, s2_filter=s2_filter1):
    """Filter qi dataframe.

    Parameters
    ----------
    s2_qi_dataframe : pandas dataframe
        S2 quality information dataframe (AOI instance qi attribute).
    qi_thresh : float
        Threshold value to filter images based on used qi filter.
        qi filter holds labels of classes whose percentages within the AOI
        is summed. If the sum is larger then the qi_threhold, data will not be
        retrieved for that date/image. The default is 1, meaning all data is
        retrieved.
    s2_filter : list
        List of strings with class labels (of unwanted classes) used to compute qi value,
        see qi_threhold. The default is s2_filter1 = ['NODATA',
              'SATURATED_DEFECTIVE',
              'CLOUD_SHADOW',
              'UNCLASSIFIED',
              'CLOUD_MEDIUM_PROBA',
              'CLOUD_HIGH_PROBA',
              'THIN_CIRRUS',
              'SNOW_ICE'].

    Returns
    -------
    filtered_s2_qi_df : pandas dataframe
        Filtered dataframe.

    """
    filtered_s2_qi_df = s2_qi_dataframe.loc[
        s2_qi_dataframe[s2_filter1].sum(axis=1) < qi_thresh
    ]

    return filtered_s2_qi_df


def s2_feature_collection_to_dataframes(s2_feature_collection):
    """Convert feature collection dict from GEE to pandas dataframe.

    Parameters
    ----------
    s2_feature_collection : dict
        Dictionary returned by GEE.

    Returns
    -------
    dataframes : pandas dataframe
        GEE dictinary converted to pandas dataframe.

    """
    dataframes = {}

    for featnum in range(len(s2_feature_collection["features"])):
        tmp_dict = {}
        key = s2_feature_collection["features"][featnum]["properties"]["name"]
        productid = s2_feature_collection["features"][featnum]["properties"][
            "productid"
        ]

        dates = [
            datetime.datetime.strptime(d.split("_")[2], "%Y%m%dT%H%M%S")
            for d in productid
        ]

        tmp_dict.update({"Date": dates})  #  , 'crs': crs}
        properties = s2_feature_collection["features"][featnum]["properties"]
        for prop, data in properties.items():
            if prop not in ["Date"]:  # 'crs' ,, 'projection'
                tmp_dict.update({prop: data})
        dataframes[key] = pd.DataFrame(tmp_dict)
    return dataframes


def s2_data_to_xarray(aoi, request_params, convert_to_reflectance=True):
    """Convert AOI.data dataframe to xarray dataset.

    Parameters
    ----------
    aoi : AOI instance
        AOI instance.
    request_params : S2RequestParams
        S2RequestParams.
    convert_to_reflectance : boolean, optional
        Convert S2 data from GEE (integers) to reflectances (floats),
        i,e, divide by 10000.
        The default is True.

    Returns
    -------
    Nothing.
        Converts the data atrribute dataframe to xarray Dataset.
        xarray is better for handling multiband data. It also has
        implementation for saving the data in NetCDF format.

    """
    # check that all bands have full data!
    datalengths = [
        aoi.data[b].apply(lambda d: len(d)) == len(aoi.data.iloc[0]["x_coords"])
        for b in request_params.bands
    ]
    consistent_data = reduce(lambda a, b: a & b, datalengths)
    aoi.data = aoi.data[consistent_data]

    #  2D data
    bands = request_params.bands

    #  1D data
    list_vars = [
        "assetid",
        "productid",
        "sun_azimuth",
        "sun_zenith",
        "system_index",
        "view_azimuth",
        "view_zenith",
    ]

    # crs from projection
    crs = aoi.data["projection"].values[0]["crs"]
    tileid = aoi.data["tileid"].values[0]
    # original number of pixels requested (pixels inside AOI)
    aoi_pixels = len(aoi.data.iloc[0]["x_coords"])

    # transform 2D data to arrays
    for b in bands:

        aoi.data[b] = aoi.data.apply(
            lambda row: s2_lists_to_array(
                row["x_coords"],
                row["y_coords"],
                row[b],
                convert_to_reflectance=convert_to_reflectance,
            ),
            axis=1,
        )

    aoi.data["SCL"] = aoi.data.apply(
        lambda row: s2_lists_to_array(
            row["x_coords"], row["y_coords"], row["SCL"], convert_to_reflectance=False
        ),
        axis=1,
    )

    aoi.data["NDVI"] = aoi.data.apply(
        lambda row: s2_lists_to_array(
            row["x_coords"], row["y_coords"], row["NDVI"], convert_to_reflectance=False
        ),
        axis=1,
    )

    array = aoi.data[bands].values

    # this will stack the array to ndarray with
    # dimension order = (time, band, x,y)
    narray = np.stack(
        [np.stack(array[:, b], axis=2) for b in range(len(bands))], axis=2
    ).transpose()  # .swapaxes(2, 3)

    scl_array = np.stack(aoi.data["SCL"].values, axis=2).transpose()
    ndvi_array = np.stack(aoi.data["NDVI"].values, axis=2).transpose()

    coords = {
        "time": aoi.data["Date"].values,
        "band": bands,
        "x": np.unique(aoi.data.iloc[0]["x_coords"]),
        "y": np.unique(aoi.data.iloc[0]["y_coords"]),
    }

    dataset_dict = {
        "band_data": (["time", "band", "x", "y"], narray),
        "SCL": (["time", "x", "y"], scl_array),
        "NDVI": (["time", "x", "y"], ndvi_array),
    }
    var_dict = {var: (["time"], aoi.data[var]) for var in list_vars}
    dataset_dict.update(var_dict)

    ds = xr.Dataset(
        dataset_dict,
        coords=coords,
        attrs={
            "name": aoi.name,
            "crs": crs,
            "tile_id": tileid,
            "aoi_geometry": aoi.geometry.to_wkt(),
            "aoi_pixels": aoi_pixels,
        },
    )
    aoi.data = ds


def s2_lists_to_array(x_coords, y_coords, data, convert_to_reflectance=True):
    """Convert 1D lists of coordinates and corresponding values to 2D array.

    Parameters
    ----------
    x_coords : list
        List of x-coordinates.
    y_coords : list
        List of y-coordinates.
    data : list
        List of data values corresponding to the coordinates.
    convert_to_reflectance : boolean, optional
        Convert S2 data from GEE (integers) to reflectances (floats),
        i,e, divide by 10000.
        The default is True.

    Returns
    -------
    arr : 2D numpy array
        Return 2D numpy array.

    """
    # get the unique coordinates
    uniqueYs = np.unique(y_coords)
    uniqueXs = np.unique(x_coords)

    # get number of columns and rows from coordinates
    ncols = len(uniqueXs)
    nrows = len(uniqueYs)

    # determine pixelsizes
    # ys = uniqueYs[1] - uniqueYs[0]
    # xs = uniqueXs[1] - uniqueXs[0]

    y_vals, y_idx = np.unique(y_coords, return_inverse=True)
    x_vals, x_idx = np.unique(x_coords, return_inverse=True)
    if convert_to_reflectance:
        arr = np.empty(y_vals.shape + x_vals.shape, dtype=np.float64)
        arr.fill(np.nan)
        arr[y_idx, x_idx] = np.array(data, dtype=np.float64) / S2_REFL_TRANS
    else:
        arr = np.empty(y_vals.shape + x_vals.shape, dtype=np.float64)
        arr.fill(NO_DATA)  # or whatever yor desired missing data flag is
        arr[y_idx, x_idx] = data
    arr = np.flipud(arr)
    return arr


def xr_dataset_to_timeseries(xr_dataset, variables):
    """Compute timeseries dataframe from xr dataset.

    Parameters
    ----------
    xr_dataset : xarray dataset

    variables : list
        list of variable names as string.

    Returns
    -------
    df : pandas dataframe
        Pandas dataframe with mean, std, se and percentage of NaNs inside AOI.

    """
    df = pd.DataFrame({"Date": pd.to_datetime(xr_dataset.time.values)})

    for var in variables:
        df[var] = xr_dataset[var].mean(dim=["x", "y"])
        df[var + "_std"] = xr_dataset[var].std(dim=["x", "y"])

        # nans occure due to missging data from 1D to 2D array
        # (pixels outside the polygon),
        # from snap algorihtm nans occure due to input/output ouf of bounds
        # checking.
        # TODO: flaggging with snap biophys algorith or some other solution to
        # check which nan are from snap    algorithm and which from 1d to 2d transformation
        nans = np.isnan(xr_dataset[var]).sum(dim=["x", "y"])
        sample_n = len(xr_dataset[var].x) * len(xr_dataset[var].y) - nans

        # compute how many of the nans are inside aoi (due to snap algorithm)
        out_of_aoi_pixels = (
            len(xr_dataset[var].x) * len(xr_dataset[var].y) - xr_dataset.aoi_pixels
        )
        nans_inside_aoi = nans - out_of_aoi_pixels
        df["aoi_nan_percentage"] = nans_inside_aoi / xr_dataset.aoi_pixels

        df[var + "_se"] = df[var + "_std"] / np.sqrt(sample_n)

    return df


def gee2pecan_s2(geofile, outdir, start, end, scale, qi_threshold):
    """ 
    Downloads Sentinel 2 data from gee and saves it in a netCDF file at the specified location.
    
    Parameters
    ----------
    geofile (str) -- path to the file containing the name and coordinates of ROI, currently tested with geojson. 
    
    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.
  
    start (str) -- starting date of the data request in the form YYYY-MM-DD
    
    end (str) -- ending date of the data request in the form YYYY-MM-DD

    scale (int) - pixel resolution, recommended value 10
    
    qi_threshold (float) -- From satellitetools: Threshold value to filter images based on used qi filter. qi filter holds labels of classes whose percentages within the AOI is summed. If the sum is larger then the qi_threshold, data will not be retrieved for that date/image. The default is 1, meaning all data is retrieved
        
    Returns
    -------
    Nothing:
            output netCDF is saved in the specified directory.
            
    Python dependencies required: earthengine-api, geopandas, pandas, netCDF4, xarray      
    """

    # read in the input file containing coordinates
    df = gpd.read_file(geofile)

    request = S2RequestParams(start, end, scale)

    # filter area of interest from the coordinates in the input file
    area = AOI(df[df.columns[0]].iloc[0], df[df.columns[1]].iloc[0])

    # calcuate qi attribute for the AOI
    ee_get_s2_quality_info(area, request)

    # get the final data
    ee_get_s2_data(area, request, qi_threshold=qi_threshold)

    # convert dataframe to an xarray dataset, used later for converting to netCDF
    s2_data_to_xarray(area, request)

    # if specified output directory does not exist, create it
    if not os.path.exists(outdir):
        os.makedirs(outdir, exist_ok=True)

    area.data.to_netcdf(os.path.join(outdir, area.name + "_bands.nc"))
