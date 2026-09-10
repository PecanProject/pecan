# Tillage Projections

## Overview

This workflow projects future tillage states and tillage events from 2024
through 2045.

Unlike crop, phenology, planting, and harvest, tillage branches by scenario.

Separate projections are produced for:

```text
BAU_Targets
NBS_Targets
```

because the two scenarios contain different future tillage acreage targets.

The script covered here is:

```text
tillage_projection.R
```

## Workflow

```text
all_data.csv
        |
        v
2023 county × crop tillage baseline
        |
        +-----------------------------+
                                      |
BAU/NBS 2045 tillage targets          |
        |                             |
        +-------------+---------------+
                      |
                      v
          interpolate annual shares
               2024-2045
                      |
                      v
          assign tillage to parcels
                      |
                      +--------------------------+
                                                 |
historical v4.1 tillage events                  |
        |                                        |
        v                                        |
timing + NDTI lookups                           |
        |                                        |
        +----------------+-----------------------+
                         |
                         v
               future tillage events
                 /               \
                v                 v
         BAU_Targets         NBS_Targets
```

## 1. Historical tillage baseline

The baseline is read from:

```text
<work_root>/all_data.csv
```

Required fields include:

```text
parcel_id
year
county
crop_class
ACRES
till_state
```

For each parcel, the latest tillage state at or before 2023 is selected.

Historical acreage is then summarized by:

```text
county
crop class
tillage state
```

to calculate the baseline acreage shares.

## 2. Tillage states

Tillage states are standardized to:

```text
no_till
low_till
high_till
```

Historical NDTI values are classified using:

```text
0-30%              -> no_till
>30% and <70%      -> low_till
>=70%              -> high_till
```

The thresholds are configurable.

## 3. Future crop metadata

Future parcels come from:

```text
crop_predictions/
```

and use the corrected v4.1.2 parcel metadata from:

```text
crop_year_states_cleaned.csv
```

including:

```text
county
county_geoid
ACRES
CLASS
SUBCLASS
```

## 4. Important old-v4.1 restriction

Historical tillage event files come from the older v4.1 inventory.

Those parcel IDs are used only to estimate historical:

```text
tillage timing
NDTI magnitude
```

Old v4.1 parcel IDs are **never joined directly to future v4.1.2 parcels**.

Historical old tillage events are joined only to historical metadata belonging
to the same old parcel inventory.

## 5. Scenario tillage targets

Future targets are read from:

```text
MAGiC_scenarios_FINAL/BAU_Targets.csv
MAGiC_scenarios_FINAL/NBS_Targets.csv
```

The relevant columns are:

```text
No till acres (CPS 329)
Reduced till acres (CPS 345)
Tilled acres
```

These are converted to:

```text
no_till
low_till
high_till
```

target acreage.

## 6. MAGiC crop mapping

MAGiC crop categories are mapped to LandIQ crop states.

Examples include:

```text
Almonds -> D
Citrus -> C
Grapes -> V
Fallow -> X
```

Some categories are split across multiple LandIQ states.

```text
All Other Field Crops
    -> F / P

Annual Cropland
    -> F / G / T / R
```

Historical county crop acreage determines the split weights.

## 7. 2045 tillage target shares

For each county and crop state:

```text
target_share =
    target tillage-state acreage /
    total tillage target acreage
```

Target shares are calculated separately for:

```text
no_till
low_till
high_till
```

County/crop groups with zero total target acreage are reported.

## 8. Annual interpolation

Future tillage shares gradually move from the historical 2023 baseline to the
2045 scenario target.

For each year:

```text
ramp =
    (year - 2023) /
    (2045 - 2023)
```

The annual share is:

```text
annual share =
    (1 - ramp) * baseline_share
    + ramp * target_share
```

Therefore:

```text
2023 -> historical baseline
2024 -> mostly historical
...
2045 -> scenario target
```

## 9. PFT assignment

Future PFT is assigned from the LandIQ lookup table.

Preferred matching is:

```text
CLASS + SUBCLASS
```

with fallback to:

```text
CLASS
```

PFT is later used to assign tillage event timing and NDTI magnitude.

## 10. Acreage-aware parcel assignment

Future tillage state is assigned separately for each:

```text
county
year
crop CLASS
```

When valid acreage exists, parcels are randomly ordered.

Target tillage shares are converted to target acreage, and parcel acreage
midpoints are assigned across the cumulative target acreage intervals.

This produces realized acreage distributions that more closely follow scenario
targets than simple parcel-count sampling.

## 11. Reproducibility

The random seed is reset separately for BAU and NBS.

This causes both scenarios to begin from the same random ordering so that their
differences are driven primarily by scenario target differences.

## 12. Missing-target validation

Every future crop parcel must receive:

```text
no_till
low_till
or
high_till
```

The script reports the number of:

```text
rows without tillage target
```

and stops if any exist.

A missing target is not treated as equivalent to no-till.

## 13. Realized-versus-target QC

For every:

```text
scenario
county
year
crop class
tillage state
```

the workflow calculates:

```text
target_share
target_acres
realized_share
realized_acres
difference_share
difference_acres
```

QC outputs are:

```text
tillage_projections/BAU_Targets/tillage_projection_qc.parquet
tillage_projections/NBS_Targets/tillage_projection_qc.parquet
```

Small discrepancies are expected because whole parcels rather than fractional
acreage are assigned.

## 14. No-till versus event generation

All parcels receive a tillage state for scenario acreage accounting.

However:

```text
no_till
```

represents a management condition rather than a physical disturbance event.

Therefore only:

```text
low_till
high_till
```

produce tillage event rows.

## 15. Historical tillage event timing

Historical v4.1 tillage event files are used to learn:

```text
event timing
NDTI percent change
```

Only low- and high-tillage historical events are used for future event
generation.

## 16. Leap-year-safe timing

Historical tillage dates are converted onto a fixed non-leap 365-day reference
calendar.

February 29 is mapped to February 28 for timing summaries.

This prevents leap years from introducing day-366 inconsistencies.

## 17. Year-boundary-safe timing

Historical event dates near both December and January are averaged using
wrapped day-of-year logic.

For example:

```text
December 28
January 5
```

is treated as a winter timing distribution rather than averaging to summer.

## 18. Historical event lookup hierarchy

Future low/high tillage events receive timing and NDTI magnitude using:

```text
1. county + PFT + tillage state
2. statewide PFT + tillage state
3. statewide tillage state
```

This retains local and PFT-specific information when available while still
providing broader fallbacks.

## 19. Event output

The final tillage event schema is:

```text
event_type
parcel_id
date
ndti_pct_drop
```

where:

```text
event_type = tillage
```

## 20. Outputs

BAU:

```text
tillage_projections/BAU_Targets/
├── tillage_projection_qc.parquet
├── tillage_statewide_2024.parquet
├── ...
└── tillage_statewide_2045.parquet
```

NBS:

```text
tillage_projections/NBS_Targets/
├── tillage_projection_qc.parquet
├── tillage_statewide_2024.parquet
├── ...
└── tillage_statewide_2045.parquet
```

## 21. Validation

The workflow checks:

- valid historical tillage states
- scenario target mapping
- zero-total target groups
- missing future tillage assignments
- realized versus target acreage shares
- historical low/high tillage event availability
- valid future event timing
- complete NDTI values
- complete annual output rows

## 22. Running the workflow

Required upstream inputs include:

```text
all_data.csv
crop_year_states_cleaned.csv
crop_predictions/
MAGiC_scenarios_FINAL/
```

Run:

```bash
Rscript tillage_projection.R
```

## 23. Relationship to crop prediction

Tillage is downstream of the shared crop projection:

```text
crop_prediction.R
        |
        v
shared future crop identity
        |
        +---------------------+
        |                     |
        v                     v
BAU tillage              NBS tillage
```

Future crop identity determines which crop-specific tillage target applies,
while the BAU/NBS scenario determines the desired future tillage distribution.