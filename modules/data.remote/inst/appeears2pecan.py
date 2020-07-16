#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
appears2pecan downloads remote sensing data using the AppEEARS API

AppEEARS API documentation: https://lpdaacsvc.cr.usgs.gov/appeears/api/?language=Python%203#introduction

Requires Python3

Author(s): Ayush Prasad
"""

import requests as r
import geopandas as gpd
import getpass
import time
import os
import cgi
import json
from gee_utils import get_sitename
from datetime import datetime
from warnings import warn


def appeears2pecan(geofile, outdir, start, end, product, projection=None, credfile=None):
    """
    Downloads remote sensing data from AppEEARS

    Parameters
    ----------
    geofile (str) -- path to the GeoJSON file containing the name and coordinates of AOI
    
    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.
  
    start (str) -- starting date of the data request in the form YYYY-MM-DD
    
    end (str) -- ending date area of the data request in the form YYYY-MM-DD

    product (str) -- product name followed by " . " and the product version, e.g. "SPL3SMP_E.003", as listed on AppEEARS website.

    projection (str) -- type of projection, only required for polygon AOI type. None by default

    credfile (str) -- path to JSON file containing Earthdata username and password. None by default

    Returns
    -------
    Nothing:
            Output files are saved in the specified directory.
            Output file is of netCDF type when AOI is a Polygon and csv type when AOI is a Point.
    """

    # API url
    api = "https://lpdaacsvc.cr.usgs.gov/appeears/api/"

    def authenticate():
        """
        uses the user provided NASA Earthdata credentials to request a token.
        
        Returns
        -------
        head (dict) : header contatining the authentication token 
        """
        if credfile:
            try:
                # if the user does not want to enter their credentials everytime they use this function, they need to store their username and password in a JSON file, preferabbly not in a git initialized directory
                with open(credfile, "r") as f:
                    cred = json.load(f)
                    user = cred["username"]
                    password = cred["password"]
            except IOError:
                print("specified file does not exist, please make sure that you have specified the path correctly")
        else:
            # if user does not want to store the credentials
            user = getpass.getpass(prompt="Enter NASA Earthdata Login Username: ")
            password = getpass.getpass(prompt="Enter NASA Earthdata Login Password: ")
        # use the credentials to call log in service. Use request's HTTP Basic Auth to do the authentication
        response = r.post("{}login".format(api), auth=(user, password))
        # delete the user and password variables as they are no longer needed
        del user, password
        # raise an exception if the POST request returned an unsuccessful status code
        response.raise_for_status()
        token_response = response.json()
        # extract the token
        token = token_response["token"]
        head = {"Authorization": "Bearer {}".format(token)}
        return head

    head = authenticate()

    # query the available layers for the product and store it in a layer + product tuple
    lst_response = r.get("{}product/{}".format(api, product)).json()
    l = list(lst_response.keys())
    layers = [(product, each_l) for each_l in l]
    prodLayer = [({"layer": l[1], "product": l[0]}) for l in layers]

    # special case to handle SMAP products
    # SMAP products individually have more than 40 layers all of which are not allowed by the API to be downloaded in a single request
    # if the requested product is one of the SMAP products then select the first 25 layers
    if product in [
        "SPL3FTP.002",
        "SPL3SMP.006",
        "SPL3SMP_E.003",
        "SPL4CMDL.004",
        "SPL4SMGP.004",
    ]:
        warn("Since you have requested a SMAP product, all layers cannot be downloaded, selecting first 25 layers..")
        # change this part to select your own SMAP layers
        prodLayer = prodLayer[0:25]

    site_name = get_sitename(geofile)

    task_name = site_name + product

    # convert start date to MM-DD-YY format as needed by the API
    start = datetime.strptime(start, "%Y-%m-%d")
    start = datetime.strftime(start, "%m-%d-%Y")
    # convert end date to MM-DD-YY format
    end = datetime.strptime(end, "%Y-%m-%d")
    end = datetime.strftime(end, "%m-%d-%Y")

    # read in the GeoJSON file containing name and coordinates of the AOI
    df = gpd.read_file(geofile)

    if (df.geometry.type == "Point").bool():
        # extract coordinates
        lon = float(df.geometry.x)
        lat = float(df.geometry.y)
        coordinates = [{"longitude": lon, "latitude": lat,}]
        # compile the JSON task request
        task = {
            "task_type": "point",
            "task_name": task_name,
            "params": {
                "dates": [{"startDate": start, "endDate": end}],
                "layers": prodLayer,
                "coordinates": coordinates,
            },
        }

    elif (df.geometry.type == "Polygon").bool():
        # query the projections
        projections = r.get("{}spatial/proj".format(api)).json()
        projs = {}
        for p in projections:
            projs[p["Name"]] = p
        # select the projection which user requested
        proj = projs[projection]["Name"]
        # extract the coordinates from the dataframe and convert it to JSON
        geo = df[df["name"] == site_name].to_json()
        geo = json.loads(geo)
        # compile the JSON task request
        task = {
            "task_type": "area",
            "task_name": task_name,
            "params": {
                "dates": [{"startDate": start, "endDate": end}],
                "layers": prodLayer,
                "output": {"format": {"type": "netcdf4"}, "projection": proj},
                "geo": geo,
            },
        }

    else:
        # if the input geometry is not of Polygon or Point Type
        raise ValueError("geometry type not supported")

    # submit the task request
    task_response = r.post("{}task".format(api), json=task, headers=head).json()

    # limit response to 2
    params = {
        "limit": 2,
        "pretty": True,
    }

    # retrieve task response and extract task id
    tasks_response = r.get("{}task".format(api), params=params, headers=head).json()
    task_id = task_response["task_id"]

    # wait_time (float) : time (sceconds) after which task status is checked
    wait_time = 60.0

    # check the task status as per the wait_time specified
    starttime = time.time()
    while (
        r.get("{}task/{}".format(api, task_id), headers=head).json()["status"] != "done"
    ):
        print(r.get("{}task/{}".format(api, task_id), headers=head).json()["status"])
        time.sleep(wait_time - ((time.time() - starttime) % wait_time))
    print(r.get("{}task/{}".format(api, task_id), headers=head).json()["status"])

    # if specified output directory does not exist create it
    if not os.path.exists(outdir):
        os.makedirs(outdir, exist_ok=True)

    # query the created files using the bundle
    bundle = r.get("{}bundle/{}".format(api, task_id)).json()

    # use the contents of the bundle to store file name and id in a dictionary
    files = {}
    for f in bundle["files"]:
        files[f["file_id"]] = f["file_name"]
    # download and save the files
    for f in files:
        dl = r.get("{}bundle/{}/{}".format(api, task_id, f), stream=True)
        filename = os.path.basename(
            cgi.parse_header(dl.headers["Content-Disposition"])[1]["filename"]
        )
        filepath = os.path.join(outdir, filename)
        with open(filepath, "wb") as f:
            for data in dl.iter_content(chunk_size=8192):
                f.write(data)
