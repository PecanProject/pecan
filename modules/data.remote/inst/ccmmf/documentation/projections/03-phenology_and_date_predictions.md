# Phenology, Planting, and Harvest Date Projections

## Overview

This section turns projected crop identities into dated events to combine what is going to be grown with when its going
to happen. `phenology_projection.R` produces leaf-on and leaf-off dates for every projected perennial crop cycle, and `planting_harvest_projection.R` produces the planting and harvest 
predictions. 

Both are downstream of the crop predictions and at the moment this workflow does not include any sampling. All predictions 
are a historical mean applied to a projected crop, so if crop_class X has a historical mean planting date of March 5th, 
that date is the same if crop class X appears in a future year. Similarly, predictions will loop through both scenarios. 

## Phenology.R
Overall, this scripts reads four types of files:
a. `historical phenology_statewide_<year>.parquet` from the inventory phenology event directory 
b. `assigned_year=<year>_gapfilled.parquet` from the matched directory, which is what connects a phenology cycle to 
its LandIQ crop class 
c. LandIQ crop identity parquet for county and cover status 
d. Projected crop identity files, one per scenario and year

To build the historical record: Historical phenology files are read year by year, and those files are joined to the matched 
product to attach a LandIQ crop class and cover crop status. The assigned county comes from the most recent year each 
parcel has one on record, which is the same rule the crop prediction script uses, so the two products always agree about 
where in the state a parcel is. 

To convert dates to offsets: Dates are stored as days they are from January 1st of that year, which creates continuous 
values we can average. For example, March 5th becomes 63. The values are averaged across the historical rows, and then 
converted back to an actual date for the final outputs. 
**Important:** The offset dates can be negative if a crops growing period started the previous year. For example, a growing date 
logged in December of the previous year can be a number like -20 instead of 345. Keeping the numbers in this order prevents
skewing the averages. 

Historical averages are ideally calculated as specifically as possible, but there must be fallbacks incase some do not exist
for certain parcels. Four levels of averages are created to use as fallbacks if the prior is not available:
1. county x crop class x cover status
2. crop class x cover status
3. county x cover status
4. statewide, split just by cover status

Projections: For each scenario and year, the projected crop file is read, unclassified fallow and idle are dropped, 
and each cycle takes its offsets from the most specific level available. Adding the offset to January 1st of the 
target year gives the predicted date.

**Important: simplifications**
1. Cover crop status is part of the lookup key, so a winter cover crop gets winter canopy dates rather than the summer 
dates of the crop it follows. The simplification is that cover status is boolean: every cover crop in the same county and crop class gets identical 
dates. Historical cover crop observations are also sparse, so more means will fallback to a broader level — the QC file is where to check how many.

2.Every cycle receives dates. Where no county or class match exists, values will fallback to the statewide average and 
the level used is recorded in the QC file.

3.Leaf-on and leaf-off always come from the same level, so predicted date can never be a county mean while the other 
is a global.

## Planting_harvest_projections.R
Building the historical record:
Historical planting and harvest events are read by year the same way as phenology, with older column names renamed on the 
as they're loaded for simplicity. The event files hold the 8 N/C pools while harvest carries the litter fractions. Dates are 
converted the same way to continuous numbers and parcels are assigned a county geoid. 

Similarly, multiple levels of means are calculated so every parcel get date, but the fall back goes back 6 steps:
1. county + crop code
2. county + crop_class
3. crop code
4. crop class
5. PFT
6. Statewide

Means are computed at six levels, matching the cascade documented for the inventory: county + crop code, 
county + crop class, crop code, crop class, PFT, global. Planting day uses circular wrapping so December and 
January plantings do not average to mid-year; the pools and fractions do not.

**Unlike phenology,** planting dates must have day of year wrapping because planting records carry no growing-season 
label, only the year of the file they came from. A December planting is day 363 and a January planting day 2, 
a few days apart, but averaging them without wrapping makes their average in July.

For harvest dates, these are not predicted directly. For each parcel, year, and crop, we find its planting event and 
its harvest event, and the gap between them is how long that crop took to grow. If the gap comes out negative or zero, 
the crop must have been harvested in the following calendar year, so we add a year.

A future harvest date is then just the projected planting date plus that historical growing season length. Doing it this 
way means harvest can never accidentally land before planting, and crops that run across New Year are handled without any 
special cases.

**Important simplifications:** 
a. Two cycles of the same crop in one parcel-year are averaged into a single pair to find the growing season length. 
County, crop class, and PFT are taken from the first record in each parcel-year-crop group rather than the most common one, since they do not vary inside a group.

b. Cover crops are planted and terminated, never harvested..

c. Phenology takes cover crop status into account; planting and harvest do not, because the historical planting records 
do not carry a cover crop flag. 

d. A wrapped planting average can come out larger than the length of the year, which puts a projected planting date in 
the following January. 

e. Both scripts stop rather than carry on if dates are missing, if a harvest does not follow its planting, if a crop has 
no county, or if the crop input has duplicate parcel-year-season rows. The planting script also re-checks that every projected 
harvest fraction is between 0 and 1.

## Outputs

Outputs go to scenario subdirectories under the phenology, planting, and harvest output roots and match the inventory 
structure/column names:

`<scenario>/phenology_statewide_<2024-2045>.parquet`
`<scenario>/planting_statewide_<2024-2045>.parquet`
`<scenario>/harvest_statewide_<2024-2045>.parquet` 

Diagnostics are written to separate files to preserve the output structure:

`<scenario>/phenology_qc_<2024-2045>.parquet` — how many crops fell to each fallback level, by county, class, and cover status
`<scenario>/planting_harvest_qc.parquet` — the same counts but for the C/N pools and harvest fractions

