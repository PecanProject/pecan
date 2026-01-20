This document walks through the code written by Katherine Rein during the 
Spring 2025 semester for the CCMMF project. This code works on downloading and
manipulating evapotranspiration data and precipitation data for different sites
in California.

Data Sources:
- Evapotranspiration: OpenET
    - https://openet.gitbook.io/docs
    - https://developers.google.com/earth-engine/datasets/catalog/OpenET_ENSEMBLE_CONUS_GRIDMET_MONTHLY_v2_0
- Precipitation: CHIRPS
    - https://data.chc.ucsb.edu/products/CHIRPS-2.0/
    
Main Storage Folder: /projectnb/dietzelab/ccmmf/management/irrigation
Github Code Storage Folder: /projectnb/dietzelab/ccmmf/management/irrigation/
                            pecan/modules/data.remote/inst/Python

How to use SCC:
- When creating desktop ensure -> Extra qsub options: -l buyin
- Once desktop loads:
    - Open Terminal
    - Type: module load miniconda
    - Create or Load environment
        - Load: conda activate ccmmf_env
        - Create (all on one line): conda create -n ccmmf_env python jupyter 
        spyder xarray requests numpy netcdf4 matplotlib pandas pyarrow earthengine-api 
        scikit-learn seaborn
    - To open spyder: spyder &
        - This may take a second to run. Be patient it will open eventually.

Google Earth Engine Account:
- Contact Brian Anderson (andyson@bu.edu) for a new Google Earth Enginge project
- Go to https://code.earthengine.google.com/
- Click on your profile picture in the top right corner
- Select Project Info
- Under Cloud Project you will find the Cloud Project ID (ex. ee-krein21-s25)
    - Save this value for later
- Find the manage cloud project link and click on it
- Under Project Info on the left hand side select Go to Project Settings
- Select IAM on the left hand side
- Select Grant Access
- Add openet@googlegroups.com as a viewer (under basic)
- Now open up a terminal window and navigate to the irrigation folder
- Run the following command in terminal: earthengine authenticate --auth_mode=notebook
- Paste the link it gives you into a browser and log into your Google account
that is linked to the Google Earth Engine project
- Paste the token back into the terminal window

OpenET Account:
- Click log in/sign up for an account at https://etdata.org/
- Use the same account as you used for your Google Earth Engine project
- Once account has been created, add in the saved Cloud Project ID into the
Cloud Project ID field at the bottom of profile settings

Organization:
- Python Files
    - CCMMF_Irrigation_API: This file is the main file that runs the data downloading
    and other data manipulation for using the OpenET API. It loads in the previously
    downloaded data and decides which data it needs to download.
    - CCMMF_Irrigation_DataDownload: This file contains the different download
    fucntions for each data type. It also contains the fuction that downloads
    compiles all of the functions to download new data for a new location/years.
    - CCMMF_Irrigation_CalcVis: This file contains the functions used to clean
    and visualize the raw data.
    - CCMMF_Irrigation_Events: This file contains the function that turns a
    dictionary of dataframes into txt files for each location in the dictionary.
    It both selects columns and sets constants for other columns. It also aggregates
    the data by week.
    - CCMMF_Irrigation_GEE: This is the same as CCMMF_Irrigation_API except it
    grabs the OpenET data from Google Earth Engine. It also does not create any
    irrigation event files.
    - CCMMF_Irrigation_GEEvAPI: This script is completely independent of all other
    workflows. This reads in all saved data from both the Google Earth Enginge
    downloads and the API downloads. It then creates graphs and summary statistics
    to help us identify if we can use Google Earth Engine monthly data instead of
    the daily data from the API.
- Folders
    - WaterBalanceCSV: This is where all of the csv files for each location get
    saved. This is a back up way to save all of the data and also makes it easier
    to quickly view data per location. Each file is labeled with the corresponding
    lat and long coordinate. The folder name is defined in the "Define multi use 
    variables" section of CCMMF_Irrigation_API.
    - WaterBalanceCSV_GEE: This is the same as the regular WaterBalanceCSV but
    simply for the et data downloaded from Google Earth Engine.
    - TimeseriesPNG: This is where the timeseries graphs for each location and
    each year are saved. There is no variable name for this folder it is simply
    included in this string f'TimeseriesPNG/CCMMR_et_precip_irr_cumsum_{YEAR}_{LAT}_{LON}.png'
    in the timeseries_graphs function in CCMMF_Irrigation_CalcVis.
    - TimeseriesPNG_GEE: This is the same as the regular TimeseriesPNG but simply
    for the et data downloaded from Google Earth Engine. The format for the files
    is 
    - CCMMF_Irrigation_Parquet: This folder is a directory for all of the parquet
    files. It is written in a way that Python and R can then tile the data by
    both location and year. This folder name is also defined in the "Define multi 
    use variables" section of CCMMF_Irrigation_API.
    - CCMMF_Irrigation_Parquet_GEE: This is the same as the regular CCMMF_Irrigation_Parquet
    but simply for the et data downloaded from Google Earth Engine.
    - CCMMF_Irrigation_EventFiles: This holds all of the event txt files for each
    location. The column names are in the header of CCMMF_Irrigation_Events. The
    naming format for the files is irrigation_eventfile_{location_id}.txt.
    - pecan: This folder contains the entire pecan repo from Github. The only portion
    of this that is needed is the Python code files which can be found in Python_Code.
    - Python_Code: This is a symlink to the folder within pecan that holds all
    of the .py files. This is so that you can add the files to the pecan Github
    repo.
- Other
    - chirps-v2.0.{year}.days_p05.nc: These are the files that contain the downloaded
    CHIRPS data on a daily scale for the whole world. They are downloaded from the
    web and then read in for each location and year. They are quite large and
    take a while to download so if your code isn't running quickly that may be
    why (if given new years).
    - design_points.csv: This is the inital locations dataframe that we started
    with. To scale this program up, simply change the csv that is being read in.
    Currently the column headers are id, lat, and lon. Keeping these the same
    will be easiest.
    
Workflow:
This workflow is the same for both the OpenET API scripts and the Google Earth
Engine Scripts.

- Data is read in from parquet file
- Calculate how old the data is (and how much new data needs to be read in)
    - If data is old, then delete the most recent CHIRPS file because we want
    to read in new data
- Read in location data (lat, lon, location_id)
- Iterate through the location data and download new data
- Check if the location id is in the parquet file we downloaded
    - If yes: check that our data is currently up to date (download/organize
     new dates if needed)
         - Also check that the years sequence is the same from what has been
         downloaded to what we defined as the years we want to look at (This
         really only catches any years that are new at the front)
    - If no: download/organize for predefined year span
- Write irrigation txt files for each location
- Write the data that has been downloaded and organzied to the parquet file

Functions (by files):
- CCMMF_Irrigation_DataDownload
    - GEEOpenET: This function downloads data from Google Earth Engine and turns
    it into a dataframe with evapotranspiration data and the date.
    - OpenETData: This function downloads data using the OpenET API and turns
    it into a dataframe with evapotranspiration data and the date.
    - CHIRPSData: This function downloads the .nc file from the CHIRPS website
    and then reads in the values for the closest latitude longitude values. It 
    then returns the data as a dataframe.
    - new_data_entry_API: This function calls on other functions to download and
    organize the years and location that was passed to it.
- CCMMF_Irrigation_CalcVis
    - water_balance: This function takes the raw data for each location and calculates
    the water balance equation for each time step. It also calculates the different
    time columns (week, year, day of year). 
    - timeseries_graphs: This takes a dataframe and saves/prints a cumulative
    sum graph for evapotranspiration, irrigation, and precipitation. There is
    also a runoff curve that is not a cumulative sum.
- CCMMF_Irrigation_Events
    - file_creation: This function takes in a dictionary of dataframes. It then
    itterates over each location in the dictionary and selects/calculates the 
    expected columns for the txt file. It also aggregates this data by week.

Next Steps:
- Figure out what is wrong with time series and predicted observed irrigation plots
    - What do cumulative and monthly evapotranspiration not match?
- Missing/mislabeled weekly data in irrigation files
- Site specific water holding capacity and crop specific rooting depth

    
    
    
    
    
    