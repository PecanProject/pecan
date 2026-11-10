# Fertilization and Amendment Projections

## Overview
The second management script produces two kinds of events: synthetic nitrogen and compost. Both are downstream of the crop, planting, and phenology predictions, and both loop through 
each scenario.

Like the previous predictions, fertilization rates are not re-derived. They are averaged from the inventory event files, so a projected rate is the mean of what the inventory applied for 
that crop in that county. The inventory's ensemble members are averaged into one rate, and no new ensemble is generated.

The two event types are handled differently. Synthetic N follows the inventory's logic, where a crop-specific rate is applied at planting for annuals and at leaf-on for perennials. Compost 
replaces the inventory's 10% base rate with county x crop acreage targets read from the target scenario sheets, which differ between BAU and NBS. Which parcels receive compost is chosen 
by a seeded, weighted random ranking, so a rerun reproduces the same assignments. The seed can be changed in the configuration file to produce different, but equally valid, assignments. 
Check `config.yml` to do so. 

## Setup
The setup remains the same as the previous scripts, and will continue to be the same for the rest of the workflow. `work_root` will continue to be your directory to save intermediate and 
final outputs to. You can refer to `config.yml` for what each setting is and how it points at the necessary paths. The fertilization-specific settings are `fertilization_event_dir`, 
`ncc_event_dir`, `fertilization_output_root`, `ncc_output_root`, and `seed`.

## Fertilization_projection.R

This script reads seven types of files:
1. The fertilization inventory  (`fertilization_event_dir`).

2. The NCC inventory (`ncc_event_dir`). 

3. `crops_full_counties.csv` created in `transition_matrix.R`, for parcel acreage. If it is not found, `crop_year_states_cleaned.csv` is used instead.

4. The LandIQ crop identity parquet, for each parcel's county.

5. The LandIQ crop lookup, for each crop's plant functional type (PFT) and its matching scenario crop name.

6. The projected crop identity, planting, and phenology files created in `predict_and_store.R`, `planting_harvest_dates.R`, and `phenology.R`.

7. The scenario target sheets (`BAU_Targets.csv`, `NBS_Targets.csv`), for their compost acreage and compost N and C columns.

## Running the script + important simplifications

The script starts off by attaching acreage and county to each parcel. Then each parcel's acreage is the median of its recorded acreage, and its county comes from the most recent LandIQ year up 
to `start_year`, the same rule used in the crop predictions.
Next, the historical events are read and split by type. The fertilization and NCC files share a column set, so they are concatenated into one historical record and separated by their 
contents rather than handled through two code paths. Rows that carry organic N or C are considered compost, and rows with neither are synthetic fertilizer. Only events dated between 
`historical_start_year` and `start_year` (currently 2016-2023) are kept. 

On the synthetic event side, total inorganic N (NH4 + NO3) is averaged across every event and ensemble member. The rates follow a similar fallback process as the previous sections, with 
four levels:
1. county + crop code
2. crop code
3. crop class
4. statewide

All inorganic N is written as NH4 and NO3 is set to zero, matching the inventory. The historical NH4:NO3 ratio is carried through the lookups but not applied, so a later version can eventually
split it without having to rebuild.

The compost events are used to calculate two values, both using the same four fallback levels. The first is compost propensity (`p_org`), the share of fertilized parcel-years that also 
received an amendment. This is the historical likelihood that a crop in that county gets compost. The second is the plant-available N fraction (`pan_frac`), which is NH4 / (NH4 + organic N) 
in historical amendment events and sets how compost N is later split into mineral and organic forms.

To actually begin the predictions, the projected crop files are read for every year, and unclassified fallow and idle rows are dropped. Each crop cycle is matched to its acreage, projected 
planting date (called the "anchor"), and its projected leaf-on date. Phenology files do not carry a crop code, so cycles are matched by order instead where the earliest planting within a 
parcel-year is paired with the earliest leaf-on, and so on. Each crop is also labeled perennial (woody PFT) or annual (everything else) to set event timing.

*One important note* is that a cover crop projected with the same crop code as its dominant crop cannot be told apart from it, so that cover cycle is dropped and the count 
is logged. Its labelled as an upstream issue in the crop projection, but it does not affect the final outputs since they only use the dominant crop.

Synthetic N events are then created for each dominant crop parcel-year. Each takes its N rate from the most specific fallback level available, and the event is dated at the 
projected planting date for annuals and the projected leaf-on date for perennials. Crops whose historical rate is zero receive no event.

Compost is handled differently because it follows the scenario sheet year by year, rather than only reading the 2045 target like tillage. Similar to the tillage and optimization scripts, 
the crop codes must first be mapped to the sheet's crop names. For each county, crop, and year, the adoption share is the sheet's compost acres divided by its total acres. That share is applied 
to the acreage of the projected parcels mapped to that county-crop rather than to the sheet's own total, so the target stays consistent with the parcel base being projected.

Parcels are then ordered by a weighted random key, `-ln(rand) / p_org`, so crops and counties that historically used compost tend to come first without the order being fixed. They are 
taken in rank order using the same cumulative acreage midpoint as the tillage assignment, where a parcel is selected if the midpoint of its acreage falls under the target. 

**Important:** 
The random ranking is seeded by year only, so BAU and NBS rank the same parcels in the same order. Where the parcel base is the same, the scenarios differ only in how far down the 
ranking each target reaches.

Lastly, the compost composition and timing are set. The sheet's compost N and C (lbs per acre) are converted to kg/m². Total N is split using `pan_frac`, where the plant available share 
is written as NH4 and the rest as organic N. All compost C is written as organic C. Each compost event is placed a random number of days before the projected planting date, 14 to 180 days 
for annuals and 30 to 210 days for perennials.

**Simplifications**
1. Both synthetic N and compost follow the dominant crop only. Cover crops sit in non-dominant seasons under grain and hay codes, so including them would assign them historical N which is not realistic. 
   
2. There is no ensemble and therefore no uncertainty spread in this product, unlike the inventory it is derived from.

3. Each fertilized parcel-year gets one synthetic event at the historical mean rate per event. If the inventory splits a crop's N across several applications per year, the projection 
   represents that as a single event of average size.
   
4. A parcel-year can receive both a synthetic event and a compost event. They are written to separate products and are not netted against each other.

5. Compost propensity is only measured among parcel-years that received synthetic fertilizer historically.

6. The provided scenario sheets carry compost acreage above the stated total acreage for about half of the county-crop rows. Shares are clamped to 0-1 so the parcel selection stays well 
   defined, and the clamped count is reported in the run log. A clamped row means every mapped parcel in that county-crop receives compost.
   
7. Pre-planting offsets can put a compost event in the calendar year before the crop it belongs to. This is allowed, matching the inventory amendment workflow, and the count is logged. 
   The event is still written to the file for its crop's year.

## Outputs
Outputs go to scenario subdirectories under `fertilization_output_root` and `ncc_output_root`, and match the inventory's file structure and column names:
`<scenario>/fertilization_statewide_<2024-2045>.parquet` - synthetic N events
`<scenario>/ncc_statewide_<2024-2045>.parquet` - compost events

Diagnostics are written to a separate file so the output structure is preserved:
`<scenario>/ncc_projection_qc.parquet`: target versus realized compost acreage and parcel counts by county, scenario crop, and year.

