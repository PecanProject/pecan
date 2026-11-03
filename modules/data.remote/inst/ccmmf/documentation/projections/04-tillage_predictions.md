# Tillage Projections

## Overview
This section shifts from crop identity predictions to management predictions, starting with tillage. It assigns a tillage state to every projected parcel-year and turns the tilled parcels into
dated events.

The tillage projection workflow can be thought of as the reverse of the inventory. Before, the tillage state is the result of reading NDTI. Now, the state is an input and the NDTI value is 
attached afterward. Since there is no imagery past 2023, nothing can be detected directly. The scenario sheet sets how many acres belong in each tillage class, the script chooses which 
parcels those acres are, and the timing and NDTI magnitude of each event are filled in from historical means.

Parcel selection is random but seeded, so a rerun reproduces the same assignments. As in the previous sections, the script loops through both scenarios (BAU and NBS). The seed can be changed in the configuration file 
to produce different, but equally valid, assignments. Check `config.yml` to do so. 

## Setup
The setup remains the same as the previous scripts, and will continue to be the same for the rest of the workflow. `work_root` will
continue to be your directory to save intermediate and final outputs to. You can refer to `config.yml` for what each setting is and how
it points at the necessary paths. The tillage-specific settings are `tillage_seed`, `tillage_output_root`, `tillage_event_dir`,
`historical_start_year`, and the two NDTI thresholds (`no_till_threshold`, `low_till_threshold`).

## Tillage_projection.R

### Inputs
This script reads seven types of files:
1. `all_data.csv`, the historical parcel-level tillage record used to build the county x crop tillage baseline. This table is not created in any previous section. It is an input, so make sure 
it is saved within your `work_root`.

2. `assigned_year=<year>_tillage.parquet`, the gapfilled inventory tillage events (one file per historical year, `historical_start_year` through `start_year`), used for event timing and NDTI values.

3. `crop_year_states_cleaned.csv` created in `transition_matrix.R`, for each parcel's county, acreage, and historical crop class.

4. The LandIQ crop lookup, for assigning a plant functional type (PFT) to each crop.

5. The projected crop identity files created in `predict_and_store.R`.

6. The projected phenology files created in `phenology.R`.

7. The scenario target sheets (`BAU_Targets.csv`, `NBS_Targets.csv`), for their tillage acreage columns.

## Running the script + important simplifications

The script starts off with a few set up sections to prepare the prediction process. The first one involves resolving the same crop mapping issue in the optimization script (`scenarios.R`). The 
target sheets name crops differently than the LandIQ data, so they must be aligned before they can be used. Most crops map one-to-one, but two do not. "All Other Field Crops" splits across 
F and P, and "Annual Cropland" splits across F, G, T, and R. Both are split in proportion to how much acreage the county currently holds in each class.

After resolving the crop mapping, it then creates the historical tillage baseline. For each parcel in `all_data.csv`, the script takes its most recent tillage record up to `start_year`. 
Acreage is then totaled for each county, crop class, and tillage state combination, giving the share of each county-crop currently in no-till, low (reduced) till, and high (conventional)
till.

The last set up step establishes annual target shares. The scenario sheets give a target for every projection year, but only the `end_year` (currently 2045) row is read for simplicity. 
For each county-crop, the three tillage acreage columns (`No till acres (CPS 329)`, `Reduced till acres (CPS 345)`, `Tilled acres`) are converted into shares of that group's total. Every 
county-crop group that appears in the projected crops gets a tillage share for every projection year, whether or not the sheet covers it. Groups with a 2045 target move from their starting 
shares to that target in equal yearly steps. Groups without one hold their starting shares flat. The `target_source` column in the QC file records which case applied:

| `target_source` | Does sheet have a target | Does county have history for the crop | Result |
|---|---|---|---|
| `scenario_ramp` | yes | yes | Ramps from the county's baseline shares to the 2045 target |
| `scenario_ramp_statewide_start` | yes | no | Ramps from the statewide shares for that crop to the 2045 target |
| `baseline_hold` | no | yes | County's baseline shares held flat to 2045 |
| `statewide_hold` | no | no | Statewide shares for that crop held flat to 2045 |

A group whose sheet target sums to zero acres is treated as having no target.

**Important:** 
in practice, only a minority of county-crops are on a scenario ramp. Most projected acreage carries its current tillage practice forward unchanged, so BAU and NBS tillage shares differ only 
in the groups the sheet covers. The parcels themselves can still differ between scenarios because the projected crops differ.


To actually begin the parcel and prediction assignments, the projected crop files are filtered to the dominant crop so each parcel-year has one row. Each parcel takes its
county and acreage from its most recent historical record, and a PFT from the crop lookup (by class and subclass, falling back to the
most common PFT for the class). Idle, young perennial, and unclassified fallow rows are dropped, since none contains a crop being prepared for planting.

Within each county, year, and crop class, the parcels are shuffled randomly and then assigned states in order using a cumulative acreage
midpoint. Each state's target acres equals its share times the group's total acres, and a parcel takes a state if the midpoint of its
acreage falls within that state's block. Assigning by acreage rather than parcel count matters because parcel sizes vary enormously.
Counting parcels could create a false result saying it hit the target number of fields while missing the acres.

If any projected parcel ends up without a state, the script writes the affected groups to `tillage_missing_targets.parquet` and stops, so
the parcel can be inspected and cleaned before predicting more. 

The historical tillage events are classified into states using the predefined NDTI thresholds in `config.yml`:
- **no-till:** NDTI change between 0 and `no_till_threshold` (currently between 0 and 30)
- **low till:** between `no_till_threshold` and `low_till_threshold` (currently between 30 and 70) 
- **high till:** at or above `low_till_threshold` (currently 70+)
These thresholds value can easily be changed in the configuration file if new values are desired. 

Each event is given the county and crop class its parcel had that year, and a PFT from the crop class. Only low and high-till events
are kept, because no-till is treated as a condition rather than an event. Mean day of year and mean NDTI change are then computed at
three fallback levels:
1. county + PFT + tillage state
2. PFT + tillage state
3. tillage state alone

Day of year is computed on a fixed 365-day calendar. It uses circular wrapping similar to the
planting date predictions, so winter tillage (late December and early January) does not average to mid-year.
*One important note* is that on leap years, February 29th is treated as February 28th and assumes the same behavior. 

No-till parcels are dropped since they produce no event, and every low and high-till parcel-year gets one tillage event. Its day of year
and NDTI change come from the most specific fallback level available, and the day of year is converted to a date in the projection
year. 

**Note **
A projected date can land inside a growing period. The inventory never has to handle this, because it only looks in fallow windows to
begin with. Any event that falls between a projected leaf-on and leaf-off date (for either the main crop or a cover crop) is moved to
the nearest day just before or after that window. This check repeats up to four times, since cover crops mean a parcel-year can have
overlapping active periods and moving an event out of one can put it inside another. If an event still falls inside a growing period
after that, the script stops.

**Important simplifications**
1. Tillage acreage applies to the dominant crop only. A cover crop affects when tillage can happen by occupying part of the year, but
   it does not count as more land.
2. As stated above, only the 2045 target is read from the sheet, and the path from the starting acreage to that target is a straight line. 
   The sheet also gives intermediate years, so a future version could follow its trajectory instead of interpolating.
3. Tillage states are assigned independently each year. A parcel's state in one year does not carry into the next, so an individual
   parcel can switch between no-till and tilled across years even though the county-crop shares change smoothly.
4. Historical tillage events missing a county are left out of the county-level means but still count toward the PFT and statewide
   means, so some counties are represented by a broader mean than others.
5. Historical events get their PFT from the crop class only, while projected parcels use class and subclass when available.

## Outputs
Outputs go to scenario subdirectories under `tillage_output_root` and match the inventory's file structure and column names:
`<scenario>/tillage_statewide_<2024-2045>.parquet`

Diagnostics are written to separate files so the output structure is preserved:
`<scenario>/tillage_projection_qc.parquet`: target versus realized acreage and share by county, year, crop class, and tillage
  state, plus the `target_source` for each group.
`<scenario>/tillage_missing_targets.parquet`: written only if some projected parcels could not be assigned a state. It lists those
  groups, and the script stops.