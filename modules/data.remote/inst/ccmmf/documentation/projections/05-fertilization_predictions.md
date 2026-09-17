# Fertilization and Amendment Projections

## Overview

This section produces two event predictions: synthetic nitrogen and compost. Both are downstream of the
crop, planting, and phenology predictions, and both loop through the scenarios.

Rates are not re-derived, they are averaged from the inventory event files, so a projected rate is the 
mean of what the inventory actually applied for that crop in that county. There is no sampling here — 
the inventory's ensemble members are averaged into one rate and no new ensemble is generated.

The two halves differ in how much they change. Synthetic N keeps the inventory's logic: a crop-specific
rate placed at planting for annuals and leaf-on for perennials. Compost does not — the inventory's flat
10% of parcels becomes a county x crop acreage target read from the CARB sheets, and that is what makes
BAU and NBS differ.

## Setup

Setup remains the same that it has been throughout the workflow: running `pacman::p_load` and making
sure `config::get(config = "default", file = "config.yml")` and `work_root` are set.

## Fertilization_projection.R

Overall, this script reads five types of files:

a. The inventory fertilization event directory
b. The inventory NCC event directory, read together with the fertilization files (since the two
products are written separately but both are needed)
c. `crops_full_counties.csv` for parcel acreage, and the LandIQ parquet for county
d. Projected crop identity, planting, and phenology files, one per scenario and year
e. The CARB scenario sheets, read for their compost columns

To split the two event kinds: the combined historical record is sorted on whether a row carries
organic N or C. Rows with neither are synthetic fertilizer, rows with either are compost. The two
inventory products share a column set, so they can be concatenated and separated this way rather than
handled through two code paths.

Synthetic rates: total inorganic N is averaged and follows a similar fallback process with four levels:
1. county + crop code
2. crop code
3. crop class
4. statewide

**Important:** all inorganic N is written as NH4 and NO3 is structurally zero, matching the inventory. 
The historical NH4:NO3 ratio is carried through the lookups but not applied, so a later release can split
it without rebuilding them.

Compost propensity: the inventory's 10% rule is replaced by a historical propensity — for each county
and crop, the share of parcel-years that received an amendment. Parcels are ordered by a weighted
random key, `-ln(rand) / p_org`, so parcels that historically composted tend to come first without the
order being fixed. This is the same mechanism the cover crop and tillage projections use.

Compost acreage: the sheet gives compost acres and total acres per county, crop, and year, and their
ratio is the adoption share. That share is applied to the acreage of the mapped parcels rather than to
the sheet's own total, so the target stays consistent with the parcel base being projected. Parcels are
taken in rank order using a cumulative acreage midpoint until the target is met.

Compost composition and timing: the sheet also gives compost N and C per acre, converted to kg/m2.
Total N is split into mineral and organic fractions using the historical plant available nitrogen
fraction for that county and crop. Events are placed before planting — 14 to 180 days for annuals, 30
to 210 days for perennials.

**Important: simplifications**
1. Synthetic N follows the dominant season 2 crop only. Cover crops sit in non-dominant seasons under
grain and hay codes, so including them would assign them the historical N rates for barley, oats, and
hay, which is not realistic.

2. There is no ensemble and therefore no uncertainty spread in this product, unlike the inventory it is
derived from.

3. A parcel-year can receive both a synthetic event and a compost event. They are written to separate
products and are not netted against each other.

4. Unclassified fallow and idle land receive no fertilization events.

5. The provided scenario sheets carry compost acreage above the stated total acreage for about half of
the county-crop rows. Shares are clamped to 0-1 so the parcel selection stays well defined, and the
clamped count is reported in the run log. A clamped row means every mapped parcel in that county-crop
receives compost.

6. Pre-planting offsets can put a compost event in the calendar year before the crop it belongs to.
This is allowed, matching the inventory amendment workflow, and the count is logged.

7. The script stops rather than carrying on if a crop cycle has no planting anchor, if crop-cycle and
phenology counts do not match, if an event row is incomplete, or if a scenario writes fewer files than
projection years.

## Outputs

Outputs go to scenario subdirectories under the fertilization and NCC output roots and match the
inventory structure/column names:

`<scenario>/fertilization_statewide_<2024-2045>.parquet` — synthetic N events
`<scenario>/ncc_statewide_<2024-2045>.parquet` — compost events

Both carry event_type, parcel_id, date, nh4_n_kg_m2, no3_n_kg_m2, org_n_kg_m2, org_c_kg_m2. Synthetic
events have zero organic N and C; compost events have zero NO3 and a split between mineral and organic N.

Diagnostics are written to separate files to preserve the output structure:

`<scenario>/ncc_projection_qc.parquet` — target versus realized compost acreage and parcel counts by
county, scenario crop, and year