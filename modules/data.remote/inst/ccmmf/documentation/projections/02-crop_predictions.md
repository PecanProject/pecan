# Crop Class Projections

## Overview

This script now applies the matrix development produced in the earlier stage.
It takes each parcels last observed crop state and projects it one year at a time until the desired end year 
(currently 2045), as well as other crop identity records derived from historical patterns. Cover crops are projected 
separately, because while BAU and NBS targets share the same acres goals, their cover crop acres slightly differ.

The final outputs are state-wide annual crop projection parquets, with the same formatting as inventory. 

## Setup
The `config.yml` remains the same as the previous scripts, and will continue to be the same for the rest of the workflow.
pacman::p_load loads the packages required for the script, and the file paths needed will be configured with 
`config = config::get(config = "default", file = "config.yml")`. `work_root` will continue to be your directory to 
save intermediate and final outputs too.

The scripts uses 5 files:
1. `crop_year_states_cleaned.csv` — organizes the historical data as one crop state per parcel-year, created in transition_matrix.R 
2. `crops_full_counties.csv` — the full record with SUBCLASS, also created in transition_matrix.R 
3. LandIQ crop identity parquet — the inventory product, filtered to configured years 
d. Crop code lookup — has CLASS/SUBCLASS, descriptions, and PFT 
e. county_optimized_matrices — each optimized matrix per county created in scenarios.R

## Crop class projection
Each county's optimzied matrix is loaded and cleaned into a readable format before use. Every parcel starts from 
the crop it was last observed growing at `start_year`. For each year from `start_year + 1` to `end_year`, the parcels 
currently in a given class draw their next class from that class's row of the county matrix, and the draw becomes 
the starting state for the following year. Counties are projected independently.

**Important**
This script makes a few assumptions/simplifications to for a smoother prediction workflow:
1. The matrix predictions are for crop class only. The rest of the crop identity records, like subclass, 
multiuse, adoy, are not projected as well, they are looked up from what those attributes historically were 
for that crop.

2. Subclass is assigned by run rather than by year. While a parcel stays in the same class it keeps one subclass. 
For example, a parcel projected to grow T for six years does not switch between berries and lettuce each year. 
A new subclass is drawn only when the class changes, using the historical county-and-class distribution, falling 
back to the statewide class distribution, then to a uniform draw over the lookup table.

## Crop crop projection
Cover crops are projected on top of the crop identity once for each scenario. Instead of predicting in general what 
each parcel  will plant in each year, this part adds another layer to determine which of these will be cover crops. 

The scenario sheet gives a cover crop acreage and a total acreage for each county and year. Dividing one by the 
other gives a cover share. That share is applied to the acreage the pipeline is actually projecting for the county, 
so the cover target stays in step with the parcel base even if the sheet's total acreage differs.

Parcels that were cover crops in the previous year are more likely to be so again, so each county has a small 2x2 
matrix built from the inventory COVER flag giving that probability. Counties with no history use a statewide matrix as a fall back.
Parcels are then shuffled with a weighted random key, -ln(rand) / p_cover, which puts likely cover croppers near the 
front without making the order fixed. Working down that list, a parcel is assigned cover if more than half of its 
acreage fits under the target acreage, mimicking the same selection approach used for compost in the amendments workflow.

A parcel assigned cover gets a second crop row for that year. The dominant season 2 crop stays as it was, with 
COVER = 0, and the cover crop is added as its own row in a non-dominant season. Its class, subclass, special 
condition, MULTIUSE, and ADOY are copied from the most common cover crop historically grown in that county after 
that dominant crop, falling back to the same dominant crop statewide, then the county, then statewide. All of those 
values come from one fallback level together, so the result always describes a cover crop that was really observed 
rather than a combination assembled from different places.

Outputs go to the prediction directory, with one subdirectory per scenario:
`BAU_Targets/crop_identity_statewide_<year>.parquet` and the same under `NBS_Targets/` — one file per year from 2024 
through 2045, sorted by parcel and season

`BAU_Targets/crops_all_years.parq and NBS_Targets/crops_all_years.parq` — the historical inventory record and the 
projection concatenated into a single file

`crop_projection_qc.parquet` — expected versus realized acreage by county, year, and class
