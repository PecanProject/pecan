# Crop Class Projections

## Overview
This script now applies the matrix development from the earlier stage. It takes each parcel's last observed crop state and projects it one year at a time until the desired end year 
(currently 2045), as well as other crop identity records derived from historical patterns. Cover crops are projected  separately, because while BAU and NBS targets share the same acres goals, their cover crop acres slightly differ.

The final outputs are state-wide annual crop projection parquets, with the same formatting as inventory. 

## Setup
The set up remains the same as the previous scripts, and will continue to be the same for the rest of the workflow. `work_root` will continue to be your directory to save intermediate and 
final outputs too. You can refer to `config.yml `for what each  setting and how it points at the necessary paths. 

The scripts uses 5 files, which are outputs from prior workflows and whose locations are specified in the configuration file:
1. `crop_year_states_cleaned.csv` — organizes the historical data as one crop state per parcel-year, created in transition_matrix.R 
2. `crops_full_counties.csv` — the full record with SUBCLASS, also created in transition_matrix.R 
3. LandIQ crop identity parquet — the crop inventory product, filtered to the desired historical years 
4. Crop code lookup — has extra crop descriptions including subclass and PFT 
5. county_optimized_matrices — each optimized matrix per county created in the previous script `scenarios.R`

## Running the script + prediction simplifications

To start, each county's optimized matrix is loaded and cleaned into a readable format before use. This beginning cleaning step is just to make sure the matrices, which were saved as csvs, 
maintain their matrix format. It also double checks no numbers were impaired when re loading. For example, it checks for missing values, numbers not between 0 and 1, and makes sure every row 
sums to 0.

Every parcel starts from the crop it was last observed growing at start_year. For each year from start_year + 1 to end_year, the parcels currently in a given class draw their next class from 
that class's row of the county matrix, and the draw becomes the starting state for the following year. To prevent prolonged computational times, the draw is made once per class rather than 
once per parcel, so all the parcels sitting in the same class are sampled together off the same row. Counties are projected independently. Along with each draw the script records 
prob_crop_class, the probability the matrix gave that particular transition, so you can see how likely each projected crop actually was.

This script makes a few assumptions/simplifications for a smoother prediction workflow:
1. County, county_geoid, and acreage are frozen at each parcel's last observed year and unchanged through to end_year. Parcels do not change size, get subdivided, or move counties over 
the course of the projection.

2. The matrix predictions are for crop class only. The rest of the crop identity records, like subclass, multiuse, adoy, are not projected as well, they are looked up and assigned from what 
those attributes historically were for that crop.

3. Subclass is assigned by run rather than by year. While a parcel stays in the same class it keeps one subclass. For example, a parcel projected to grow T for six years does not switch between 
berries and lettuce. A new subclass is drawn only when the class changes, using the historical county-and-class distribution.

4. The non-class attributes are filled by using fallbacks rather than one lookup to ensure as many are assigned them as possible. SPECOND, MULTIUSE, and ADOY are taken from the most specific 
historical grouping that has a value for them: 
a. county + class + subclass
b. county + class,
c. class + subclass
d. just class
e. just PFT
f. single statewide value 

Each attribute is assigned on its own, so a parcel can take its ADOY from a county-level group and its MULTIUSE from a broader one. 

5. ADOY is averaged on a wrapped calendar. If a crop's historical planting days group around New Year, with some in December and some in January, a plain average would land somewhere in the middle 
of summer. The script checks for that pattern, shifts the early days forward by 365 before averaging, then wraps the result back into 1 to 365.

6. Runs are reproducible. The `seed` value in the config file is set once at the top for the class and subclass draws. The cover crop selection (described below) reseeds per scenario, year, 
and county so BAU and NBS rank parcels differently instead of identically. Rerunning with the same config gives the same projection, and changing the seed gives a different but equally valid one.

## Cover crop projection
The second layer of crop projection includes the cover crops, which are projected on top of the main identity, once per scenario. Instead of generally predicting what each parcel will plant 
in each year, this part adds another layer to determine which parcel also include cover crops. 

The scenario sheet is currently set up with a total acreage and cover crop acreage target for each crop, county, and year. Dividing one by the other gives a cover share. That share is applied 
to the acreage the pipeline is actually projecting for the county, so the cover target stays in step with the parcel base even if the sheet's total acreage differs.

Parcels that were cover crops in the previous year are more likely to be so again, so each county has a small 2x2 matrix built from the inventory cover flag giving that probability. Counties 
with no history use the statewide matrix as a fall back. Parcels are then shuffled with a weighted random key, `-ln(rand) / p_cover`, which puts likely cover crops near the front without making 
the order fixed. Working down that list, a parcel is assigned as cover if more than half of its acreage fits under the target acreage. 

Then, the parcels assigned as cover gets a second crop row for that year. The dominant/season 2 crop stays as it was with COVER = 0, and the cover crop is added as its own row in a non-dominant 
season. Its class, subclass, and other identity attributes are copied from the most common cover crop historically grown in that county after that dominant crop, using a similar fallback structure 
if a more specific grouping is not available. The last case fallback is once again statewide. As opposed to the main crop projections, the values come from one fallback level together, so the 
result always describes a cover crop that was really observed rather than a combination assembled from different places. This is done because the information on cover crops is much smaller compared to 
main crops, and mixing between fallback levels is more likely to create unobserved attributes. 

After both main and cover crops are predicted, a few outputs are written as the final parquets and extra QC files, described below. 

## Outputs
The final parquets are added to a folder called `prediction_dir` that is created when the script runs to your defined `work_root`. Within the main folder, subfolders named after each scenario 
are created as well. The final parquets are formatted the same as the inventory, divded by each each and aggregated back up to statewide file. For example, the outputs will currently be:

`BAU_Targets/crop_identity_statewide_<year>.parquet`and `NBS_Targets/crop_identity_statewide_<year>.parquet` - one file per year from start_year +1 through end_year, sorted by parcel and season

`BAU_Targets/crops_all_years.parq and NBS_Targets/crops_all_years.parq` — the historical inventory record and the projection concatenated into a single file

`crop_projection_qc.parquet` — expected versus realized acreage by county, year, and class