# IC preparation workflow for new SDA sites. Run each step independently as needed.

# 1. site_info and samples
# site_info.RData: /projectnb/dietzelab/cwebb16/SDA/Inputs/site_info.RData
# samples.Rdata:   /projectnb/dietzelab/cwebb16/SDA/samples.Rdata

# 2. phenology
source("/projectnb/dietzelab/cwebb16/SDA/Inputs/Phenology/phenology.R")

# 3. site PFTs
# assign using MODIS land cover (MCD12Q2); output site_pft.csv with columns 'site' and 'pft'
# no automated script - see IC_prep_guide.md for details

# 4. soil inputs
source("/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/gdal_extraction.R")
source("/projectnb/dietzelab/cwebb16/SDA/Inputs/Soil/soil_texture_extraction.R")

# 5. ERA5 met drivers (raw data: /projectnb/dietzelab/dongchen/anchorSites/ERA5)
source("/projectnb/dietzelab/cwebb16/pecan_base/modules/data.atmosphere/R/ERA5_met_process.R")

# 6. pool initial conditions
rmarkdown::render("/projectnb/dietzelab/cwebb16/pecan_base/modules/assim.sequential/inst/anchor/IC_prep_anchorSites.Rmd")
