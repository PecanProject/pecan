# Irrigation Projections

## Overview
This section is split into two scripts. `get_climate_data.R` turns downloaded Cal-Adapt NetCDFs into a daily climate series for each county,  and `irrigation_projection.R` runs the irrigation 
water balance on that series using the projected crop and date products.

The water balance is the same as the inventory, but everything going into it changes. Weather comes from downscaled climate projections rather than observations, planting and harvest dates come 
from the date projections, and peak greenness, which the inventory observes, has to be reconstructed.

For simplicity, the predictions currently use a single gcm/ssp combination (CESM2/SSP370). The inventory also resolves precipitation and ET to the parcel level, while the projection averages 
45 km Cal-Adapt grid cells into one daily series per county. Parcel-level variation survives in crop type, soil water holding capacity, rooting depth, and crop dates, but not in the weather.

## Set up
Setup remains the same as it has been throughout the workflow: each script starts with a set up section that loads the packages, reads  `config.yml`, and configures new paths relative to your
`work_root`. You can refer to `config.yml` for what each setting is and how it points at the necessary paths. The GCM and SSP are set by `climate_gcm` and `climate_ssp`. Changing either 
produces a different climate file and therefore a different irrigation product.

## get_climate_data.R

This script builds the climate input for the irrigation projections. It uses 2 types of files:
1. Cal-Adapt WRF NetCDFs stored under `wrf_base_dir`. The current download covers California at 45 km, which comes to 198 cells.
2. California county boundaries, pulled from the US Census through the `tigris` package.

## Running the script + important simplifications

The script starts by assigning each grid cell to a county. The latitude and longitude of each cell are read from its first NetCDF, turned into points, and intersected with the county boundaries.
One caveat is that a small county can contain no cell center at a 45km level. Any county without a center is assigned to the nearest cell. Every county therefore ends up with at least one cell, 
and the script stops if any county is still unmatched. This lookup is written out as `caladapt_county_site_lookup.csv` to your configured `work_root`.

*Note that* The climate has to extend one year past the final projection year. A crop planted late in 2045 is harvested in 2046, and the water balance runs all the way to the harvest date. 
Therefore the script has a checks that every expected NetCDF exists for the chosen GCM and SSP, from the first projection year through one year past `end_year` (currently 2024 to 2046), and 
will stop if any are missing.

Each file is then processed to calculate daily reference evapotranspiration (ET0). The hourly variables (air temperature, wind speed, humidity, radiation, and precipitation) are aggregated to 
daily values, and daily ET is computed with the FAO-56 Penman-Monteith method. Wind is adjusted to a 2m height if the data is recorded at a different height (`wind_height_m` in the configuration). 

Lastly, daily ET0, precipitation, and temperatures are averaged across the cells assigned to each county, creating one series per county per day. Note that due to the 45km grid, it is possible for
large counties average several cells, while smaller ones are represented by a single cell, sometimes one whose center falls outside the actual county. 

**Important simplifications**
1. Daily weather is uniform within each county for reasonable run and computational times.
2. Net radiation assumes a fixed albedo (0.23) and surface emissivity (0.98), and soil heat flux is set to zero at the daily step, following 
   standard FAO-56 practice.

## Outputs
This script writes 2 files directly to your `work_root`:
1. `caladapt_county_daily_climate.csv` - County, date, climate + ET info, and the number of cells averaged for each county.
2. `caladapt_county_site_lookup.csv` - which grid cell(s) supply each county.

## irrigation.R

This script creates the irrigation predictions for each scenario. It uses 7 types of files:

1. The projected crop identity files created in `predict_and_store.R`

2. The projected planting and harvest files created in `planting_harvest_dates.R`

3. The LandIQ crop identity parquet, for each parcel's historical county

4. `assigned_year=<year>_gapfilled.parquet`, the matched phenology directory for historical peak greenness timing.

5. The county daily climate file created in `get_climate_data.R`

6. The SSURGO parcel weights and the SSURGO soil database for soil water holding capacity

7. Crop water parameters from PEcAn's `data.land` package (`bism_kc_by_crop` and `crop_whc`)

### Running the script + important simplifications
The script starts by reconstructing peak greenness, since the water balance needs a peak date and nothing can observe one in the future. Instead, the historical phenology 
record gives where the peak fell as a fraction of the season (the time from greenness onset to peak ÷ the time from onset to the end of the season). Only dominant crops are used, and each 
parcel is given its most recent historical county. Matching the previous scripts, the peak fraction uses five fallback levels:
1. county + crop code
2. county + crop class
3. crop code
4. crop class
5. statewide 

The script then loops through each scenario. The projected crop cycles are assembled by joining each crop to its planting date on crop code after dropping unclassified fallow and idle rows. 
Harvest files carry no crop code, so cycles are matched chronologically instead. Both tables are sorted by their own date and paired by position within each parcel-year. The script stops if 
the counts do not match, if a crop has no planting or harvest date, or if a harvest does not fall after its planting. The historical peak fraction is then applied to each projected 
season length (planting to harvest) to give a projected peak date.

*One important note* is that a cover crop projected with the same crop code as its dominant crop cannot be told apart from it downstream, so that cover cycle is dropped and the count is logged, 
the same approach in the fertilization script. Unlike fertilization, the remaining cover crop cycles are kept, and each receives its own water balance.

Each crop is then matched to a BISM crop name, which supplies the crop coefficient curve, allowable depletion fraction, and rooting depth. When a LandIQ code has no exact BISM match, a small 
proxy table assigns the nearest reasonable crop. For example, several stone fruit subclasses use "Stone fruits" and D14 uses "Almonds". Crops with no mapping, rooting depth, or depletion fraction 
are dropped, and the counts for each reason are printed in the console.

Next, soil water holding capacity is calculated for each parcel and rooting depth. For every SSURGO soil horizon within the rooting depth, available water capacity is multiplied by the horizon's 
thickness, summed, and then weighted across soil components and map units to give one value per parcel. This step is slow, so results are cached per county under `irrigation_awc_cache_dir`. 
The first run pays the cost and later runs, including the next scenario(s), can reuse it. Rice is exempt, since the flooded rice model does not use a soil bucket. Non-rice parcels without a 
usable SSURGO value are dropped.

The climate file is then filtered to the configured GCM and SSP, and the script stops if it does not cover every projection year and `end_year` + 1. Before running the balance, a 'pre-flight' 
check prints a summary for each scenario that includes a. counties with gaps in their climate, b. crop cycles that start before or end after the climate record, and c. crop names with no crop 
coefficient entry that are dropped. 

This runs the largest counties to be run first and gets them out of the way. When a county is finished, that empty worker will immediately grab a new county and process it. This slightly improves 
the overall run time, which should take approximately 1 hour. 

Parcels that share a crop, planting, peak, and harvest date are grouped together within each county, so their daily crop ET only has to be calculated once. Crop ET starts from the county's 
daily ET0 and is scaled by the crop coefficient and by canopy cover. Canopy cover rises in a straight line from 0.15 at planting to 1 at peak, then falls back to 0.15 at harvest.

Each parcel then runs its own soil water balance. Irrigation is triggered when soil water drops below the allowable depletion, and each application is capped at `irrigation_max`. Rice is an 
exception, and uses a separate flood model instead. It keeps the water depth near `rice_target`, between `rice_min` and `rice_max`, and accounts for water lost to `rice_seepage` (all defined 
in the configuration file). Only days with irrigation above zero are saved as events.

Each county writes its own file to `<scenario>/_county_tmp/` when it finishes, so an interrupted run resumes by skipping counties that already have an output. Once every county is complete, 
the county files are combined into one statewide file per projection year.

**Important simplifications**
1. The canopy curve is piecewise linear, from 0.15 at planting to 1 at peak and back to 0.15 at harvest.

2. Each crop cycle runs its own water balance. Where a cover crop and a main crop overlap on the same parcel, both can generate irrigation independently.

3. Parcels with no usable crop water parameters or no SSURGO capacity are dropped rather than given a fallback, so this product covers fewer parcels than the others.

4. A crop cycle that runs past the edge of the climate record is clipped to the days the record covers, rather than stopping the run. The canopy curve still uses the original dates, so the 
   season is shortened, not reshaped.
   
5. Although the county files under `_county_tmp` and the AWC cache are reused on a rerun, any change to crop cycles, dates, climate, or soil inputs requires deleting them first, or the run will 
   mix old and new results.

## Outputs
Outputs go to scenario subdirectories under `irrigation_output_dir` - 
`<scenario>/irrigation_statewide_<2024-2045>.parquet`

Each file holds the irrigation events for crops projected in that year. A crop planted late in the year can have irrigation dates in the following calendar year, and those events stay in the 
file for the crop's year.


