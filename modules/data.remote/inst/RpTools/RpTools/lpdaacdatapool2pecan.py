"""
lpdaacdatapool2pecan downloads files from Earth Data Pool. Currently GEDI Level1B, Level2A and Level2B options available.

Based on DAAC Downloader script https://git.earthdata.nasa.gov/projects/LPDUR/repos/daac_data_download_python/browse/DAACDataDownload.py 
and GEDI subsetter https://git.earthdata.nasa.gov/projects/LPDUR/repos/gedi-subsetter/browse/GEDI_Subsetter.py

"""


import geopandas as gpd
import requests as r
from datetime import datetime
import os
import h5py
import pandas as pd
import numpy as np
import time
import json
import getpass
from subprocess import Popen
from netrc import netrc


def lpdaacdatapool2pecan(geofile, outdir, out_filename, start, end, product, credfile=None):
    """
    Downloads files from LP DAAC Data Pool

    Parameters
    ----------
    geofile (str) -- path to the geosjon file containing the name and coordinates of ROI

    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.

    out_filename (str) -- filename of the output file

    start (str) -- starting date of the data request in the form YYYY-MM-dd

    end (str) -- ending date areaof the data request in the form YYYY-MM-dd

    product (str) -- product id, example GEDI02_A.002

    credfile (str) -- path to JSON file containing Earthdata username and password. None by default

    Returns
    -------
    Absolute path to the output file.
    output H5 file is saved in the specified directory.

    """

    urs = "urs.earthdata.nasa.gov"

    if credfile:
        try:
            # if the user does not want to enter their credentials everytime they use this function, they need to store their username and password in a JSON file, preferabbly not in a git initialized directory
            with open(credfile, "r") as f:
                cred = json.load(f)
                user = cred["username"]
                password = cred["password"]
        except IOError:
            print(
                "specified file does not exist, please make sure that you have specified the path correctly"
            )
    else:
        # if user does not want to store the credentials
        user = getpass.getpass(prompt="Enter NASA Earthdata Login Username: ")
        password = getpass.getpass(prompt="Enter NASA Earthdata Login Password: ")

    try:
        netrcDir = os.path.expanduser("~/.netrc")
        netrc(netrcDir).authenticators(urs)[0]

    except FileNotFoundError:
        homeDir = os.path.expanduser("~")
        Popen(
            "touch {0}.netrc | chmod og-rw {0}.netrc | echo machine {1} >> {0}.netrc".format(
                homeDir + os.sep, urs
            ),
            shell=True,
        )
        Popen("echo login {} >> {}.netrc".format(user, homeDir + os.sep), shell=True)
        Popen(
            "echo password {} >> {}.netrc".format(password, homeDir + os.sep),
            shell=True,
        )

    except TypeError:
        homeDir = os.path.expanduser("~")
        Popen("echo machine {1} >> {0}.netrc".format(homeDir + os.sep, urs), shell=True)
        Popen("echo login {} >> {}.netrc".format(user, homeDir + os.sep), shell=True)
        Popen(
            "echo password {} >> {}.netrc".format(password, homeDir + os.sep),
            shell=True,
        )

    df = gpd.read_file(geofile)

    lon = float(df.geometry.x)
    lat = float(df.geometry.y)

    # Define the base CMR granule search url, including LPDAAC provider name and max page size (2000 is the max allowed)
    cmr = f"https://cmr.earthdata.nasa.gov/search/granules.json?pretty=true&provider=LPDAAC_ECS&temporal={start},{end}&page_size=2000&concept_id="

    # Set up dictionary where key is GEDI shortname + version
    concept_ids = {
        "GEDI01_B.002": "C1908344278-LPDAAC_ECS",
        "GEDI02_A.002": "C1908348134-LPDAAC_ECS",
        "GEDI02_B.002": "C1908350066-LPDAAC_ECS",
    }

    # CMR uses pagination for queries with more features returned than the page size
    page = 1
    bbox = f"{lon},{lat},{lon},{lat}"
    bbox = bbox.replace(" ", "")  # remove any white spaces
    try:
        # Send GET request to CMR granule search endpoint w/ product concept ID, bbox & page number, format return as json
        cmr_response = r.get(
            f"{cmr}{concept_ids[product]}&bounding_box={bbox}&pageNum={page}"
        ).json()["feed"]["entry"]

        results = cmr_response

        # If 2000 features are returned, move to the next page and submit another request, and append to the response
        while len(cmr_response) % 2000 == 0:
            page += 1
            cmr_response += r.get(
                f"{cmr}{concept_ids[product]}&bounding_box={bbox}&pageNum={page}"
            ).json()["feed"]["entry"]

        # CMR returns more info than just the Data Pool links, below use list comprehension to return a list of DP links
        results = [c["links"][0]["href"] for c in cmr_response]
        print(results)

        for result in results:
            print(result)
            print("downloading files")
            print("output location")
            print(os.path.join(outdir, out_filename + ".h5"))
            res = r.get(result, auth=(user, password), allow_redirects=True)
            open(os.path.join(outdir, out_filename + ".h5"), "wb").write(res.content)
        
        print("All files downloaded")
            

        
        if product in concept_ids:
            output_file = gedi_subsetter(outdir, out_filename)
            return output_file

        

    except:
        # If the request did not complete successfully, print out the response from CMR
        print(
            r.get(
                f"{cmr}{concept_ids[product]}&bounding_box={bbox.replace(' ', '')}&pageNum={page}"
            ).json()
        )
    
    




def gedi_subsetter(outdir, out_filename):
    """
    Converts GEDI H5 files to CSV format

    Parameters
    ----------
    outdir (str) -- path to the directory where the GEDI H5 file is stored. This is also where the otutput csv file would be saved

    out_filename (str) -- filename of the output file

    Returns
    -------
    Absolute path to the output file.
    output CSV file is saved in the specified directory.

    """



    os.chdir(outdir)
    beamSubset = [
        "BEAM0000",
        "BEAM0001",
        "BEAM0010",
        "BEAM0011",
        "BEAM0101",
        "BEAM0110",
        "BEAM1000",
        "BEAM1011",
    ]
    layerSubset = None

    if not os.path.exists(outdir):
        os.makedirs(outdir)

    # Create list of GEDI HDF-EOS5 files in the directory
    gediFiles = [o for o in os.listdir() if o.endswith(".h5")]

    # --------------------DEFINE PRESET BAND/LAYER SUBSETS ------------------------------------------ #
    # Default layers to be subset and exported, see README for information on how to add additional layers
    l1bSubset = [
        "/geolocation/latitude_bin0",
        "/geolocation/longitude_bin0",
        "/channel",
        "/shot_number",
        "/rxwaveform",
        "/rx_sample_count",
        "/stale_return_flag",
        "/tx_sample_count",
        "/txwaveform",
        "/geolocation/degrade",
        "/geolocation/delta_time",
        "/geolocation/digital_elevation_model",
        "/geolocation/solar_elevation",
        "/geolocation/local_beam_elevation",
        "/noise_mean_corrected",
        "/geolocation/elevation_bin0",
        "/geolocation/elevation_lastbin",
        "/geolocation/surface_type",
        "/geolocation/digital_elevation_model_srtm",
    ]
    l2aSubset = [
        "/lat_lowestmode",
        "/lon_lowestmode",
        "/channel",
        "/shot_number",
        "/degrade_flag",
        "/delta_time",
        "/digital_elevation_model",
        "/elev_lowestmode",
        "/quality_flag",
        "/rh",
        "/sensitivity",
        "/digital_elevation_model_srtm",
        "/elevation_bias_flag",
        "/surface_flag",
        "/num_detectedmodes",
        "/selected_algorithm",
        "/solar_elevation",
    ]
    l2bSubset = [
        "/geolocation/lat_lowestmode",
        "/geolocation/lon_lowestmode",
        "/channel",
        "/geolocation/shot_number",
        "/cover",
        "/cover_z",
        "/fhd_normal",
        "/pai",
        "/pai_z",
        "/rhov",
        "/rhog",
        "/pavd_z",
        "/l2a_quality_flag",
        "/l2b_quality_flag",
        "/rh100",
        "/sensitivity",
        "/stale_return_flag",
        "/surface_flag",
        "/geolocation/degrade_flag",
        "/geolocation/solar_elevation",
        "/geolocation/delta_time",
        "/geolocation/digital_elevation_model",
        "/geolocation/elev_lowestmode",
    ]

    # -------------------IMPORT GEDI FILES AS GEODATAFRAMES AND CLIP TO ROI-------------------------- #
    # Loop through each GEDI file and export as a point geojson
    l = 0
    for g in gediFiles:
        l += 1
        print(f"Processing file: {g} ({l}/{len(gediFiles)})")
        gedi = h5py.File(g, "r")  # Open file
        gediName = g.split(".h5")[0]  # Keep original filename
        gedi_objs = []
        gedi.visit(gedi_objs.append)  # Retrieve list of datasets

        # Search for relevant SDS inside data file
        gediSDS = [str(o) for o in gedi_objs if isinstance(gedi[o], h5py.Dataset)]

        # Define subset of layers based on product
        if "GEDI01_B" in g:
            sdsSubset = l1bSubset
        elif "GEDI02_A" in g:
            sdsSubset = l2aSubset
        else:
            sdsSubset = l2bSubset

        # Append additional datasets if provided
        if layerSubset is not None:
            [sdsSubset.append(y) for y in layerSubset]

        # Subset to the selected datasets
        gediSDS = [c for c in gediSDS if any(c.endswith(d) for d in sdsSubset)]

        # Get unique list of beams and subset to user-defined subset or default (all beams)
        beams = []
        for h in gediSDS:
            beam = h.split("/", 1)[0]
            if beam not in beams and beam in beamSubset:
                beams.append(beam)

        gediDF = pd.DataFrame()  # Create empty dataframe to store GEDI datasets
        del beam, gedi_objs, h

        # Loop through each beam and create a geodataframe with lat/lon for each shot, then clip to ROI
        for b in beams:
            beamSDS = [s for s in gediSDS if b in s]

            # Search for latitude, longitude, and shot number SDS
            lat = [l for l in beamSDS if sdsSubset[0] in l][0]
            lon = [l for l in beamSDS if sdsSubset[1] in l][0]
            shot = f"{b}/shot_number"

            # Open latitude, longitude, and shot number SDS
            shots = gedi[shot][()]
            lats = gedi[lat][()]
            lons = gedi[lon][()]

            # Append BEAM, shot number, latitude, longitude and an index to the GEDI dataframe
            geoDF = pd.DataFrame(
                {
                    "BEAM": len(shots) * [b],
                    shot.split("/", 1)[-1].replace("/", "_"): shots,
                    "Latitude": lats,
                    "Longitude": lons,
                    "index": np.arange(0, len(shots), 1),
                }
            )

            # Convert lat/lon coordinates to shapely points and append to geodataframe
            geoDF = gpd.GeoDataFrame(
                geoDF, geometry=gpd.points_from_xy(geoDF.Longitude, geoDF.Latitude)
            )

            # Clip to only include points within the user-defined bounding box
            # geoDF = geoDF[geoDF['geometry'].within(ROI.envelope)]
            gediDF = geoDF
            del geoDF

        # Convert to geodataframe and add crs
        # gediDF = gpd.GeoDataFrame(gediDF)
        gediDF.crs = "EPSG:4326"

        if gediDF.shape[0] == 0:
            print(
                f"No intersecting shots were found between {g} and the region of interest submitted."
            )
            continue
        del lats, lons, shots

    # --------------------------------OPEN SDS AND APPEND TO GEODATAFRAME---------------------------- #
    beamsDF = pd.DataFrame()  # Create dataframe to store SDS
    j = 0

    # Loop through each beam and extract subset of defined SDS
    for b in beams:
        beamDF = pd.DataFrame()
        beamSDS = [
            s
            for s in gediSDS
            if b in s and not any(s.endswith(d) for d in sdsSubset[0:3])
        ]
        shot = f"{b}/shot_number"

        try:
            # set up indexes in order to retrieve SDS data only within the clipped subset from above
            mindex = min(gediDF[gediDF["BEAM"] == b]["index"])
            maxdex = max(gediDF[gediDF["BEAM"] == b]["index"]) + 1
            shots = gedi[shot][mindex:maxdex]
        except ValueError:
            print(f"No intersecting shots found for {b}")
            continue
        # Loop through and extract each SDS subset and add to DF
        for s in beamSDS:
            j += 1
            sName = s.split("/", 1)[-1].replace("/", "_")

            # Datasets with consistent structure as shots
            if gedi[s].shape == gedi[shot].shape:
                beamDF[sName] = gedi[s][mindex:maxdex]  # Subset by index

            # Datasets with a length of one
            elif len(gedi[s][()]) == 1:
                beamDF[sName] = [gedi[s][()][0]] * len(
                    shots
                )  # create array of same single value

            # Multidimensional datasets
            elif len(gedi[s].shape) == 2 and "surface_type" not in s:
                allData = gedi[s][()][mindex:maxdex]

                # For each additional dimension, create a new output column to store those data
                for i in range(gedi[s].shape[1]):
                    step = []
                    for a in allData:
                        step.append(a[i])
                    beamDF[f"{sName}_{i}"] = step

            # Waveforms
            elif s.endswith("waveform") or s.endswith("pgap_theta_z"):
                waveform = []

                if s.endswith("waveform"):
                    # Use sample_count and sample_start_index to identify the location of each waveform
                    start = gedi[f'{b}/{s.split("/")[-1][:2]}_sample_start_index'][
                        mindex:maxdex
                    ]
                    count = gedi[f'{b}/{s.split("/")[-1][:2]}_sample_count'][
                        mindex:maxdex
                    ]

                # for pgap_theta_z, use rx sample start index and count to subset
                else:
                    # Use sample_count and sample_start_index to identify the location of each waveform
                    start = gedi[f"{b}/rx_sample_start_index"][mindex:maxdex]
                    count = gedi[f"{b}/rx_sample_count"][mindex:maxdex]
                wave = gedi[s][()]

                # in the dataframe, each waveform will be stored as a list of values
                for k in range(len(start)):
                    singleWF = wave[
                        int(start[k] - 1) : int(start[k] - 1 + count[k])
                    ]
                    waveform.append(",".join([str(q) for q in singleWF]))
                beamDF[sName] = waveform

            # Surface type
            elif s.endswith("surface_type"):
                surfaces = ["land", "ocean", "sea_ice", "land_ice", "inland_water"]
                allData = gedi[s][()]
                for i in range(gedi[s].shape[0]):
                    beamDF[f"{surfaces[i]}"] = allData[i][mindex:maxdex]
                del allData
            else:
                print(f"SDS: {s} not found")
            print(f"Processing {j} of {len(beamSDS) * len(beams)}: {s}")

        beamsDF = beamsDF.append(beamDF)
    del beamDF, beamSDS, beams, gedi, gediSDS, shots, sdsSubset

    # Combine geolocation dataframe with SDS layer dataframe
    outDF = pd.merge(
        gediDF,
        beamsDF,
        left_on="shot_number",
        right_on=[sn for sn in beamsDF.columns if sn.endswith("shot_number")][0],
    )
    outDF.index = outDF["index"]
    del gediDF, beamsDF

    df = pd.DataFrame(outDF)

    timestamp = time.strftime("%y%m%d%H%M%S")
    save_path = os.path.join(outdir, out_filename + "_" + timestamp + ".csv")

    df.to_csv(os.path.join(save_path))

    return os.path.abspath(save_path)

