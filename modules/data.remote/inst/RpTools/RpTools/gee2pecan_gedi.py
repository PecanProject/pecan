"""
Downloads Global Forest Canopy Height, 2019 from Google Earth Engine and saves it in a netCDF file.
 
Dataset: https://glad.umd.edu/dataset/gedi

Data retrieved: canopy_height

Requires Python3

"""

from RpTools.gee_utils import create_geo, get_sitecoord, get_sitename
import ee
import pandas as pd
import os
import xarray as xr
import time


ee.Initialize()


def gee2pecan_gedi(geofile, outdir, filename, start, end):
    """
    Downloads and saves SMAP data from GEE

    Parameters
    ----------
    geofile (str) -- path to the geosjon file containing the name and coordinates of ROI

    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.

    filename (str) -- filename of the output file

    start (str) -- starting date of the data request in the form YYYY-MM-dd

    end (str) -- ending date areaof the data request in the form YYYY-MM-dd

    Returns
    -------
    Absolute path to the output file.
    output netCDF is saved in the specified directory.

    """

    geo = create_geo(geofile)
    SatImage = ee.ImageCollection("users/potapovpeter/GEDI_V27")
    info = SatImage.filterBounds(geo).getRegion(geo, 500).getInfo()

    data = {"lon": [info[1][1]], "lat": [info[1][2]], "canopy_height": [info[1][4]]}
    df = pd.DataFrame(data)
    coords = {
        "time": [2019],
    }

    site_name = get_sitename(geofile)
    AOI = get_sitecoord(geofile)

    tosave = xr.Dataset(
        df,
        coords=coords,
        attrs={
            "site_name": site_name,
            "AOI": AOI,
        },
    )

    # if specified output path does not exist create it
    if not os.path.exists(outdir):
        os.makedirs(outdir, exist_ok=True)

    timestamp = time.strftime("%y%m%d%H%M%S")

    filepath = os.path.join(outdir, filename + "_" + timestamp + ".nc")

    # convert to netCDF and save the file
    tosave.to_netcdf(filepath)

    return os.path.abspath(filepath)
