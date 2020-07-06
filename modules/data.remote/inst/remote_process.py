#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
remote_process controls the individual functions to create an automatic workflow for downloading and performing computation on remote sensing data.

Requires Python3

Author: Ayush Prasad
"""

from get_remote_data import get_remote_data
from process_remote_data import process_remote_data
from gee_utils import get_sitename


def remote_process(
    geofile,
    outdir,
    start,
    end,
    source,
    collection,
    qc=None,
    algorithm="snap",
    output={"get_data": "bands", "process_data": "lai"},
    stage={"get_data": True, "process_data": True},
):

    """
    Controls get_remote_data() and process_remote_data() to download and process remote sensing data.

    Parameters
    ----------
    geofile (str) -- path to the file containing the name and coordinates of ROI, currently tested with geojson. 
    
    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.
  
    start (str) -- starting date of the data request in the form YYYY-MM-DD
    
    end (str) -- ending date area of the data request in the form YYYY-MM-DD

    source (str) -- source from where data is to be downloaded

    collection (str) -- dataset ID

    qc (float) -- quality control parameter

    algorithm (str) -- algorithm used for processing data in process_data()

    output (dict) -- "get_data" - the type of raw data, "process_data" - final proccesed data

    stage (dict) -- temporary argument to imitate database checks
  
    Returns
    -------
    Nothing:
            output files from the functions are saved in the specified directory.

    """

    # when db connections are made, this will be removed
    aoi_name = get_sitename(geofile)

    if stage["get_data"]:
        get_remote_data(geofile, outdir, start, end, source, collection, qc)

    if stage["process_data"]:
        process_remote_data(aoi_name, output, outdir, algorithm)


if __name__ == "__main__":
    remote_process(
        geofile="./satellitetools/test.geojson",
        outdir="./out",
        start="2018-01-01",
        end="2018-12-31",
        source="gee",
        collection="COPERNICUS/S2_SR",
        qc=1,
        algorithm="snap",
        output={"get_data": "bands", "process_data": "lai"},
        stage={"get_data": True, "process_data": True},
    )
