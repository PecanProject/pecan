
# Guide to initial condition prep for new SDA sites
*Author: Cami Webb (cwebb16@bu.edu)*

To add new sites to the PEcAn SDA system, there are a few things you need to prepare as initial conditions. After site_info and samples are ready, these can be done in any order.

### 1. Site prep
#### site_info and samples
The first thing you need to do is compile a list of your new site IDs and their lat/lon coordinates, called **site_info.RData**. See here for an example on how to format it:

`/projectnb/dietzelab/cwebb16/SDA/site_info.RData`

The other thing doesn't require anything new for new sites, but you'll need the **samples** object later on in the SDA process. This is a list with different attributes (like SLA) as 100 ensemble members for each PFT. Find it here:

`/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/samples.Rdata`

Or here (they're the same thing):

`/projectnb/dietzelab/cwebb16/SDA/samples.Rdata`

-----------------

### 2. Phenology
To include leaf phenology in your runs, first make sure you have PEcAn installed *(https://github.com/PecanProject/pecan/)*. Then use this code:

**`/projectnb/dietzelab/cwebb16/SDA/Inputs/Phenology/phenology.R`** (Author: Cami Webb)

*Which calls:*

*`modules/data.remote/R/extract_phenology_MODIS.R` (PEcAn)*

And exports your phenology data as a .csv to your desired directory.

-----------------

### 3. Site PFTs
Each of your sites will now be assigned a PFT. This can be done a number of ways, and really depends on what areas your sites are in. The MODIS land cover maps (https://doi.org/10.5067/MODIS/MCD12Q2.061) are a good place to start. Once you assign PFT, you'll want a csv called **site_pft.csv** that has columns **site** and **pft**.

-----------------

### 4. Soil inputs
This part is a little more involved. The first step extracts raw SoilGrids texture data (.vrt) across depths and quantiles and converts to .tif. This is my version, edited slightly from Dongchen's original version:

**`/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/gdal_extraction.R`** (Author: Dongchen Zhang)

Once you have the data downloaded, you can extract soil texture ensembles using this script:

**`projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soil_texture_extraction.R`** (Author: Cami Webb)

*Which calls this function:*

*`/projectnb/dietzelab/dongchen/anchorSites/NA_runs/soilgrids_texture_extract.R` (Author: Dongchen Zhang)*

*And uses this function to generate ensembles:*

*`/projectnb/dietzelab/cwebb16/pecan_base/modules/data.land/R/soil_params_ensemble.R` (Edited from PEcAn repo)*

In the end, you should have an .rds file for clay, silt, and sand, and soil texture ensembles for each site.

-----------------

### 5. ERA5 meteorological drivers
ERA5 is a global climate dataset that we can use as meteorological drivers in the SDA. The global annual dataset has already been downloaded (by Dongchen Zhang), so all we need to do is process that data for our new sites. The downloaded annual data has an .nc file for each year and can be found here:

`/projectnb/dietzelab/dongchen/anchorSites/ERA5`

To process this data for our sites, use this file in the pecan repo:

**`/modules/data.atmosphere/R/ERA5_met_process.R`** (PEcAn)

-----------------

### 6. Pool initial conditions
The last initial conditions are the starting values for the model's state variables (SIPNET) like carbon pools and soil moisture. These get passed into the model as the initial conditions for the first forecast timestep.

This script is pretty straightforward:

**`/projectnb/dietzelab/cwebb16/pecan_base/modules/assim.sequential/inst/anchor/IC_prep_anchorSites.Rmd`**

*I adapted it from this original script in the PEcAn repo (with some debugging): *

*`modules/assim.sequential/inst/anchor/IC_prep_anchorSites.Rmd`*

-----------------

#### That's it for the initial conditions. Once your observational data is prepped, your sites are ready to be run through the SDA!

Updated 3/16/26
