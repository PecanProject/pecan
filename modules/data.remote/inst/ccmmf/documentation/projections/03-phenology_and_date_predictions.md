# Phenology, Planting, and Harvest Date Projections

## Overview
This section turns projected crop identities into dated events to combine the questions 'what will be grown?' with 'when will it happen?' `phenology.R` produces leaf-on and leaf-off dates for 
every projected crop cycle, and `planting_harvest_dates.R` produces the planting and harvest predictions. 

Both are downstream of the crop predictions and this workflow does not include any sampling. All predictions are historical means applied to a projected crop, so if crop_class X has a 
historical mean date of March 5th, that date is the same if crop_class X appears in a future year. Similarly, predictions will loop through each scenario. 

## Setup
The setup remains the same as the previous scripts, and will continue to be the same for the rest of the workflow. `work_root` will continue to be your directory to save intermediate and 
final outputs to. You can refer to `config.yml` for what each setting is and how it points at the necessary paths. 

## Phenology.R

This script reads four types of files:
1. `phenology_statewide_<year>.parquet`, the phenology inventory 
2. `assigned_year=<year>_gapfilled.parquet`, the matched inventory that connects a phenology cycle to its LandIQ crop class 
3. The LandIQ crop identity parquet for county and cover status 
4. The projected crop identity parquets that were produced in `predict_and_store.R`, one per scenario and year

## Running the script + important simplifications

The script starts off by building the historical record. The historical phenology files are read year by year, and those files are joined to the matched product to attach a LandIQ crop class
+ cover crop status. The assigned county comes from the most recent year each parcel has one on record. This follows the same rule the crop prediction script uses so the two products always agree
about where in the state a parcel is. 

The next step is turning the dates into usable numbers. The inventory files and projection outputs will require a standard date format, but those cannot be used to calculate means. To solve this, 
dates are stored as the number of days they are from January 1st of that year, which creates continuous values we can average. For example, March 5th becomes 63. The values are averaged across 
the historical rows, and then converted back to an actual date for the final outputs.
*Note:*
The converted date values can be negative if a growing period started the previous year. For example, a growing date logged in December of the previous year can be a number like -20 instead of 345. 
Keeping the numbers in this order prevents skewing the averages. 

As the historical averages are calculated, they use a similar fallback method as the crop predictions in case some values are missing for certain parcels. The phenology predictions use four 
fallback 'levels' if the previous is not available. 

1. county x crop class x cover status
2. crop class x cover status
3. county x cover status
4. statewide, split just by cover status 

After historical averages are computed, the script will read through each projected crop parquet and drop unclassified fallow (X) and idle (I) rows. Each cycle takes its historical means from 
the most specific fallback level available. The values are added to January 1st of the target year, and then converted back to date format to produce the projected values. This process is done
for both the main crop and cover crop if a parcel is assigned one. 

**Important simplifications**
1. Cover status is boolean, so every cover crop in the same county and crop class gets identical dates. Historical cover crop observations are also sparse, so more means will fall back to a 
broader level — the QC file is where to check how many.

2. Every cycle receives dates. Where no county or class match exists, values will fall back to the statewide average as a last resort and the level used is recorded in the QC file.

3. Leaf-on and leaf-off always come from the same fallback level, so one predicted date can never be a county mean while the other is global. This prevents unobserved dates from being considered
realistic future behavior.

## planting_harvest_dates.R

This script reads four types of files:
a. `planting_statewide_<year>.parquet`, the planting inventory 
b. `harvest_statewide_<year>.parquet`, the harvest inventory 
c. `crops_full_counties.csv`, the full record with SUBCLASS created in transition_matrix.R 
d. The projected crop identity parquets that were produced in `predict_and_store.R`, one per scenario and year

## Running the script + simplifications
This script follows a similar structure to the phenology section by creating means/fallback levels from historical behavior and converting dates to continuous numbers with some caveats.

Building the historical record: Historical planting and harvest events are read by year the same way as phenology. The event files hold the 8 N/C pools while harvest carries the litter 
fractions. 

**Unlike phenology,** planting dates must have day of year wrapping because planting records carry no growing-season label, only the year of the file they came from. For example, a December 
planting of day 363 and a January planting of day 2 are only a few days apart, but averaging them without wrapping puts their average in July, completely skewing the predictions. 

The planting dates use 6 fallback levels:
1. county + crop code
2. county + crop_class
3. crop code
4. crop class
5. PFT
6. Statewide

The mean calculations are done specifically on the planting dates, and harvest dates are not predicted directly. For each parcel, year, and crop, the script finds its historical planting and 
harvest event, and the time between them is how long that crop took to grow. If the time comes out negative or zero, we infer the crop must have been harvested in the following calendar year, so
a year is added.

A future harvest date is then the projected planting date plus that historical growing season length. This method was chosen so harvest dates are never projected before planting, and crops 
that run across New Year are handled without any special cases.

**Important simplifications:** 
1. Two cycles of the same crop in one parcel-year are averaged into a single pair to find the growing season length. County, crop class, and PFT are taken from the first record in each 
parcel-year-crop group rather than the most common one, since they do not vary inside a group.

2. Cover crops are planted and terminated, never harvested.

3. Phenology takes cover crop status into account while planting and harvest do not, because the historical planting records do not carry a cover crop flag. 

4. Both scripts stop rather than carry on if dates are missing, if a harvest does not follow its planting, if a crop has no county, or if the crop input has duplicate parcel-year-season rows. 
The planting script also re-checks that every projected harvest fraction is between 0 and 1.

## Outputs
The final parquets are added to folders called `phenology_projections`, `planting_projections`, and `harvest_projections` that are created in your defined `work_root` when the scripts run. You
can refer to the configuration file to also see how these output folders are created. Within the main folders, subfolders named after each scenario are created as well. The final parquets are 
formatted the same as the inventory, divided by each year and aggregated back up to a statewide file. For example, the outputs will currently be:

`<scenario>/phenology_statewide_<2024-2045>.parquet`
`<scenario>/planting_statewide_<2024-2045>.parquet`
`<scenario>/harvest_statewide_<2024-2045>.parquet` 

QC outputs are written as well in each script to record how many parcels were predicted with each fallback level.
`<scenario>/phenology_qc_<2024-2045>.parquet` 
`<scenario>/planting_harvest_qc.parquet`