# Phenology, Planting, and Harvest Date Projections

## Overview

This workflow projects future crop-season timing from 2024 through 2045 using
historical CCMMF phenology, planting, and harvest products together with the
shared future crop predictions.

It consists of two related scripts:

```text
phenology_projection_shared_compact.R
planting_harvest_projection.R
```

Phenology, planting, and harvest are projected once because the current BAU and
NBS scenarios share the same future crop identity.

## Workflow

```text
Future crop predictions
        |
        +-------------------------+
        |                         |
        v                         v
 historical phenology      historical planting
      products             + harvest products
        |                         |
        v                         v
county × crop CLASS       planting timing +
leaf-on/off lookup        planting->harvest lag
        |                         |
        v                         v
phenology projections     planting/harvest projections
```

# Phenology

## 1. Historical phenology inputs

Historical phenology events are read from:

```text
<ccmmf_root>/management/event_files_v4.1.2/
```

for:

```text
2018-2023
```

Matched LandIQ/MSLSP information is read from:

```text
<ccmmf_root>/management/phenology/
matched_landiq_mslsp_v4.1.2/gapfill_dates/
```

The matched files provide:

```text
parcel_id
landiq_CLASS
mslsp_50PCGI
mslsp_50PCGD
```

## 2. Historical phenology matching

Historical phenology records are standardized to:

```text
parcel_id
year
leafonday
leafoffday
```

These dates are matched to the LandIQ/MSLSP data using:

```text
parcel_id
year
leafonday
leafoffday
```

This attaches the corresponding LandIQ crop class.

## 3. County assignment

County information is obtained from:

```text
crops_full_counties.csv
```

Each parcel must map to only one county.

## 4. Phenology lookup

Historical leaf-on and leaf-off timing is converted to offsets from January 1.

The lookup is calculated by:

```text
county + crop CLASS
```

For each group:

```text
mean leaf-on offset
mean leaf-off offset
```

is calculated across the historical 2018-2023 observations.

## 5. Future phenology

Each future parcel-year is matched using:

```text
county
predicted CLASS
```

Projected dates are:

```text
leaf-on date =
    January 1 of prediction year
    + historical mean leaf-on offset

leaf-off date =
    January 1 of prediction year
    + historical mean leaf-off offset
```

If no county/class historical estimate exists, the projected event remains
missing and the number of missing rows is reported.

## 6. Phenology outputs

The final schema is:

```text
event_type
parcel_id
date
```

where:

```text
event_type = leafon
```

or:

```text
event_type = leafoff
```

Files are written to:

```text
phenology_projections/
├── phenology_statewide_2024.parquet
├── ...
└── phenology_statewide_2045.parquet
```

# Planting and Harvest

## 7. Historical planting inputs

Historical planting events are read from:

```text
<ccmmf_root>/management/event_files_v4.1.2/
```

for:

```text
2018-2023
```

Planting records provide:

```text
crop code
PFT
planting date
leaf C
wood C
fine-root C
coarse-root C
leaf N
wood N
fine-root N
coarse-root N
```

The standardized C/N pool columns are:

```text
leaf_c_kg_m2
wood_c_kg_m2
fine_root_c_kg_m2
coarse_root_c_kg_m2
leaf_n_kg_m2
wood_n_kg_m2
fine_root_n_kg_m2
coarse_root_n_kg_m2
```

## 8. Historical harvest inputs

Historical harvest events provide:

```text
crop code
harvest date
frac_above_removed_0to1
frac_below_removed_0to1
frac_above_to_litter_0to1
frac_below_to_litter_0to1
```

All historical fraction values are checked to ensure:

```text
0 <= fraction <= 1
```

## 9. Planting date timing

Historical planting dates are converted to:

```text
planting_relative_day
```

relative to January 1.

Dates occurring near December and January require circular-date handling.

For example:

```text
December 25
January 10
```

should represent one winter planting period rather than averaging to July.

The workflow therefore shifts early-year observations forward by 365 days when
the distribution crosses the year boundary before calculating mean planting
timing.

## 10. Planting lookup hierarchy

Historical planting timing and C/N pool values use:

```text
1. county GEOID + crop code
2. county GEOID + crop class
3. crop code
4. crop class
5. PFT
6. global historical mean
```

Planting timing uses wrapped averaging.

C/N pool values use normal arithmetic means.

## 11. PFT fallback

If future crop identity does not provide a PFT, the workflow attempts to assign
one from historical information using:

```text
crop code
        |
        v
crop class
```

## 12. Historical planting-to-harvest duration

Historical planting and harvest events are paired by:

```text
parcel_id
source_year
crop_code
```

For each matched pair:

```text
harvest_lag_days =
    harvest_relative_day
    - planting_relative_day
```

If the result is non-positive, harvest is interpreted as occurring in the next
calendar year.

Only positive finite durations are retained.

## 13. Harvest lookup hierarchy

Historical harvest duration and harvest fractions use:

```text
1. county GEOID + crop code
2. county GEOID + crop class
3. crop code
4. crop class
5. PFT
6. global historical mean
```

The harvest date itself is not independently averaged.

## 14. Future crop filtering

Future crops represent dominant:

```text
season = 2
```

The following states do not receive planting or harvest events:

```text
X = unclassified fallow
I = idle
```

## 15. Project planting dates

Projected planting is calculated as:

```text
planting_date =
    January 1 of future year
    + historical planting relative day
    - 1
```

Because wrapped timing is retained on a continuous timeline, a valid planting
date may occur in the following calendar year.

## 16. Project harvest dates

Future harvest is always calculated from planting:

```text
harvest_date =
    planting_date
    + historical mean harvest_lag_days
```

This guarantees:

```text
harvest_date > planting_date
```

and correctly preserves crop cycles that span calendar years.

## 17. Planting outputs

The planting schema is:

```text
event_type
parcel_id
date
crop_code
leaf_c_kg_m2
wood_c_kg_m2
fine_root_c_kg_m2
coarse_root_c_kg_m2
leaf_n_kg_m2
wood_n_kg_m2
fine_root_n_kg_m2
coarse_root_n_kg_m2
```

Files are written to:

```text
planting_projections/
├── planting_statewide_2024.parquet
├── ...
└── planting_statewide_2045.parquet
```

## 18. Harvest outputs

The harvest schema is:

```text
event_type
parcel_id
date
frac_above_removed_0to1
frac_below_removed_0to1
frac_above_to_litter_0to1
frac_below_to_litter_0to1
```

Files are written to:

```text
harvest_projections/
├── harvest_statewide_2024.parquet
├── ...
└── harvest_statewide_2045.parquet
```

## 19. Validation

The timing workflow checks:

- historical phenology-to-crop matching
- parcel-to-county consistency
- missing county/class phenology estimates
- duplicate future parcel-year rows
- missing planting dates
- missing harvest dates
- harvest always occurs after planting
- harvest fractions remain between 0 and 1
- annual output schemas are complete

## 20. Running order

Run after crop prediction:

```text
crop_prediction.R
        |
        +--------------------------+
        |                          |
        v                          v
phenology_projection_      planting_harvest_
shared_compact.R           projection.R
```