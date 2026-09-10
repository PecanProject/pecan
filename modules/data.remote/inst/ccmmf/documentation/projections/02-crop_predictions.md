# Crop Class Projections

## Overview

This workflow uses optimized county crop-transition matrices to generate annual
parcel-level crop-class projections from 2024 through 2045.

Crop projections are shared across BAU and NBS because both scenarios currently
use the same crop-acreage targets. Scenario-specific branching occurs later in
the management-event workflows.

The script covered here is:

```text
crop_prediction.R
```

This workflow depends on outputs from the transition-matrix and optimization
workflow, so those scripts must be run first.

## Workflow

```text
crop_year_states_cleaned.csv
        +
crops_full_counties.csv
        +
county_optimized_matrices/
        |
        v
Select each parcel's latest observed crop state
at or before 2023
        |
        v
Sequential annual crop-class prediction
2024 -> 2025 -> ... -> 2045
        |
        v
Assign crop subclass
        |
        v
Attach LandIQ-compatible identity attributes
        |
        v
crop_predictions/
├── crop_identity_statewide_2024.parquet
├── ...
├── crop_identity_statewide_2045.parquet
├── crops_all_years.parq
├── crop_projection_qc.parquet
└── crop_prediction_manifest.parquet
```

The key feature of this workflow is that crop classes are projected
sequentially. A parcel's predicted crop class in one year becomes the current
state used to determine its transition probabilities in the following year.

## 1. Inputs

### Historical annual crop states

```text
<work_root>/crop_year_states_cleaned.csv
```

Required fields are:

- `parcel_id`
- `year`
- `county`
- `county_geoid`
- `state`
- `ACRES`

The `state` field is renamed internally to `crop_class`.

### Historical crop and subclass records

```text
<work_root>/crops_full_counties.csv
```

This file preserves historical LandIQ `CLASS` and `SUBCLASS` observations used
to assign subclasses to future crop-class predictions.

### Optimized county transition matrices

```text
<work_root>/county_optimized_matrices/
```

The directory contains files of the form:

```text
<county>_crop_matrix.csv
```

### Shared LandIQ crop identity data

```text
/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1.2/crops_all_years.parq
```

Historical LandIQ identity fields are used to construct lookup tables for
projected crop attributes.

### LandIQ crop lookup

```text
/projectnb/dietzelab/ccmmf/management/LandIQ_cropCode_lookup_table.csv
```

The lookup provides valid crop class/subclass combinations and associated
descriptive fields.

## 2. Configuration

Set the workspace:

```r
work_root = "/path/to/your/folder"
```

Shared CCMMF data are stored under:

```r
ccmmf_root = "/projectnb/dietzelab/ccmmf"
```

The default projection period is:

```text
2024-2045
```

A fixed random seed is used so repeated runs with the same inputs are
reproducible.

## 3. Load and validate optimized crop matrices

Each optimized county matrix is read from:

```text
county_optimized_matrices/
```

The script checks that row and column crop-state names match.

Before prediction:

- missing probabilities are replaced with 0
- negative probabilities are clipped to 0
- probabilities greater than 1 are clipped to 1
- zero-sum rows are converted to self-loops
- rows are renormalized to sum to 1

The transition convention is:

```text
A[i,j] = probability that crop class i transitions to crop class j next year
```

## 4. Select each parcel's starting state

Future projections begin from each parcel's most recent observed crop state at
or before 2023.

A parcel does not need a record specifically in 2023 as long as a valid earlier
state is available.

Only parcels with:

- a starting state represented in the transition matrix
- positive finite acreage

are included.

## 5. Sequential annual crop prediction

For a parcel currently in crop class `i`, the next crop is sampled from row `i`
of the county transition matrix:

```text
P(next crop = j | current crop = i) = A[i,j]
```

After the next crop is sampled, it becomes the current state for the following
year.

```text
Observed 2023 class
        |
        v
sample 2024
        |
        v
2024 predicted class
        |
        v
sample 2025 using 2024 class
        |
       ...
        |
        v
2045 predicted class
```

This preserves the conditional structure of the Markov model and allows
persistent crops such as orchards and vineyards to remain persistent when
their transition matrices contain high self-transition probabilities.

## 6. Transition probability

Each parcel-year stores:

```text
prob_crop_class
```

This is the one-year transition probability associated with the crop transition
that was actually sampled.

## 7. Expected versus realized acreage

The optimized transition matrix defines an expected county crop distribution:

```text
X(t+1) = X(t) A
```

The script also sums the acreage of the actual sampled parcel states.

For each county, year, and crop class it records:

- `expected_acres`
- `realized_acres`
- `difference_acres`
- `abs_difference_acres`

These diagnostics are written to:

```text
<work_root>/crop_predictions/crop_projection_qc.parquet
```

Small differences are expected because whole parcels are assigned rather than
fractional acreage.

## 8. Subclass assignment

Transition matrices operate at the LandIQ `CLASS` level.

If a parcel remains in its last observed crop class, the last observed
`SUBCLASS` is retained when possible.

When a new subclass is required, the hierarchy is:

```text
county + crop class historical distribution
        |
        v
statewide crop class historical distribution
        |
        v
valid LandIQ lookup-table subclasses
```

A sampled subclass is retained for a continuous run of the same crop class.

## 9. LandIQ-compatible identity attributes

Projected rows are converted to:

```text
parcel_id
COUNTY
year
season
CLASS
SUBCLASS
SPECOND
MULTIUSE
ADOY
COVER
```

Projected crop identity represents dominant season 2:

```text
season = 2
```

Historical LandIQ observations are used to populate `SPECOND`, `MULTIUSE`, and
`ADOY`.

`COVER` is currently retained in the schema but written as `NA` for projected
records.

## 10. Outputs

Annual crop identity:

```text
<work_root>/crop_predictions/
├── crop_identity_statewide_2024.parquet
├── ...
└── crop_identity_statewide_2045.parquet
```

Combined historical and future crop identity:

```text
<work_root>/crop_predictions/crops_all_years.parq
```

QC:

```text
<work_root>/crop_predictions/crop_projection_qc.parquet
```

Manifest:

```text
<work_root>/crop_predictions/crop_prediction_manifest.parquet
```

## 11. Validation

Check:

- missing county matrices
- duplicate parcel-year rows
- expected versus realized acreage
- perennial self-transition probabilities
- annual output schema

Large or systematic expected-versus-realized acreage differences should be
investigated before using projections downstream.

## 12. Running the workflow

Required upstream outputs:

```text
crop_year_states_cleaned.csv
crops_full_counties.csv
county_optimized_matrices/
```

Run:

```bash
Rscript crop_prediction.R
```

## 13. Relationship to previous workflow

```text
transition_matrix.R
        |
        v
optimize_crop_matrices_only.R
        |
        v
crop_prediction.R
```

The first script estimates historical crop transitions.

The second adjusts those transitions toward the 2045 crop distribution.

The crop-prediction script applies the optimized matrices sequentially at the
parcel level through 2045.