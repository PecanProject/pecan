"""
Downloads SMAP Global Soil Moisture Data from Google Earth Engine and saves it in a netCDF file.

Data retrieved: ssm, susm, smp, ssma, susma

Requires Python3

Author: Ayush Prasad
"""
from gee_utils import create_geo, get_sitecoord, get_sitename
import ee
import pandas as pd
import os
import xarray as xr
import datetime

ee.Initialize()


def gee2pecan_smap(geofile, outdir, start, end):
    """
    Downloads and saves SMAP data from GEE

    Parameters
    ----------
    geofile (str) -- path to the geosjon file containing the name and coordinates of ROI
    
    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.

    start (str) -- starting date of the data request in the form YYYY-MM-dd

    end (str) -- ending date areaof the data request in the form YYYY-MM-dd

    Returns
    -------
    Nothing:
            output netCDF is saved in the specified directory
    """

    geo = create_geo(geofile)

    def smap_ts(geo, start, end):
        # extract a feature from the geometry
        features = [ee.Feature(geo)]
        # create a feature collection from the features
        featureCollection = ee.FeatureCollection(features)

        def smap_ts_feature(feature):
            area = feature.geometry()
            # create the image collection
            collection = (
                ee.ImageCollection("NASA_USDA/HSL/SMAP_soil_moisture")
                .filterBounds(area)
                .filterDate(start, end)
                .select(["ssm", "susm", "smp", "ssma", "susma"])
            )

            def smap_ts_image(img):
                # scale (int) Default: 30
                scale = 30
                # extract date from the image
                dateinfo = ee.Date(img.get("system:time_start")).format("YYYY-MM-dd")
                # reduce the region to a list, can be configured as per requirements
                img = img.reduceRegion(
                    reducer=ee.Reducer.toList(),
                    geometry=area,
                    maxPixels=1e8,
                    scale=scale,
                )
                # store data in an ee.Array
                ssm = ee.Array(img.get("ssm"))
                susm = ee.Array(img.get("susm"))
                smp = ee.Array(img.get("smp"))
                ssma = ee.Array(img.get("ssma"))
                susma = ee.Array(img.get("susma"))
                tmpfeature = (
                    ee.Feature(ee.Geometry.Point([0, 0]))
                    .set("ssm", ssm)
                    .set("susm", susm)
                    .set("smp", smp)
                    .set("ssma", ssma)
                    .set("susma", susma)
                    .set("dateinfo", dateinfo)
                )
                return tmpfeature

            # map tmpfeature over the image collection
            smap_timeseries = collection.map(smap_ts_image)
            return (
                feature.set("ssm", smap_timeseries.aggregate_array("ssm"))
                .set("susm", smap_timeseries.aggregate_array("susm"))
                .set("smp", smap_timeseries.aggregate_array("smp"))
                .set("ssma", smap_timeseries.aggregate_array("ssma"))
                .set("susma", smap_timeseries.aggregate_array("susma"))
                .set("dateinfo", smap_timeseries.aggregate_array("dateinfo"))
            )

        # map feature over featurecollection
        featureCollection = featureCollection.map(smap_ts_feature).getInfo()
        return featureCollection

    fc = smap_ts(geo=geo, start=start, end=end)

    def fc2dataframe(fc):
        ssm_datalist = []
        susm_datalist = []
        smp_datalist = []
        ssma_datalist = []
        susma_datalist = []
        date_list = []
        # extract var and date data from fc dictionary and store in it in smapdatalist and datelist
        for i in range(len(fc["features"][0]["properties"]["ssm"])):
            ssm_datalist.append(fc["features"][0]["properties"]["ssm"][i][0])
            susm_datalist.append(fc["features"][0]["properties"]["susm"][i][0])
            smp_datalist.append(fc["features"][0]["properties"]["smp"][i][0])
            ssma_datalist.append(fc["features"][0]["properties"]["ssma"][i][0])
            susma_datalist.append(fc["features"][0]["properties"]["susma"][i][0])
            date_list.append(
                str(datetime.datetime.strptime(
                    (fc["features"][0]["properties"]["dateinfo"][i]).split(".")[0],
                    "%Y-%m-%d",
                ))
            )
        fc_dict = {
            "date": date_list,
            "ssm": ssm_datalist,
            "susm": susm_datalist,
            "smp": smp_datalist,
            "ssma": ssma_datalist,
            "susma": susma_datalist,
        }
        # create a pandas dataframe and store the data
        fcdf = pd.DataFrame(
            fc_dict, columns=["date", "ssm", "susm", "smp", "ssma", "susma"]
        )
        return fcdf

    datadf = fc2dataframe(fc)

    site_name = get_sitename(geofile)
    AOI = get_sitecoord(geofile)

    # convert the dataframe to an xarray dataset, used for converting it to a netCDF file
    tosave = xr.Dataset(
        datadf,
        attrs={
            "site_name": site_name,
            "start_date": start,
            "end_date": end,
            "AOI": AOI,
        },
    )

    # # if specified output path does not exist create it
    if not os.path.exists(outdir):
        os.makedirs(outdir, exist_ok=True)

    # convert to netCDF and save the file
    tosave.to_netcdf(os.path.join(outdir, site_name + "_smap.nc"))

