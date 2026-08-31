# Crop Type and Tillage Projections

## Overview

This workflow uses historical LandIQ crop and tillage observations to generate
county-level transition matrices and project parcel-level crop and tillage
states through 2045.

The workflow consists of four main scripts:

1. `transition_matrix.R`
2. `tillage.R`
3. `scenarios.R`
4. `predict_and_store.R`

Reusable transition-matrix functions are stored in
`modules/data.remote/R/transition_functions.R`.

## Workflow
```text
Historical LandIQ crop records
             |
             v
    transition_matrix.R
        /             \
       v               v
crop_year_states     tillage.R
_cleaned.csv             |
       |                 v
       |             all_data.csv
       |                 |
       v                 |
   scenarios.R           |
       |                 |
       v                 |
optimized crop matrices  |
+ BAU/NBS tillage targets
        \               /
         \             /
          v           v
        predict_and_store.R
                |
                v
      2024-2045 parcel predictions
```

## 1. Crop transition matrices

`transition_matrix.R` reads harmonized LandIQ crop records from 2018-2023.

Agricultural records are identified using the LandIQ crop lookup table. Parcel
centroids are spatially joined to California counties so that transition
matrices can later be created separately for each county.

A parcel can have multiple crop observations within the same year because of
multiple seasons. These observations are reduced to one annual crop state.

### a. Handling unknown crop states

LandIQ crop class `X` represents an unknown or unresolved crop state. Short
runs of `X` are corrected only when neighboring observations provide enough
information to make a reasonable replacement. Longer or unresolved runs remain
as `X`.

### b. Annual crop state and uncertainty

For each parcel-year, the dominant crop class is selected. If more than one
crop class occurs within the year, `non_dom_prob` records the fraction of
observations that do not match the dominant class.

This uncertainty is later used to down-weight less certain transitions.

### c. Outputs

The script writes:
- `crops_full_counties.csv`: preserves historical crop subclass information for
later subclass assignment in predict_and_store.R.

- `crop_year_states_cleaned.csv`: contains the full crop population and is the
main historical crop input used by `scenarios.R` and crop prediction.

- one county crop transition matrix per county


## 2. Tillage transition matrices

`tillage.R` combines historical tillage observations with the annual crop
states.

Tillage intensity is classified as:
- `no_till`: NDTI percent change less than or equal to 30
- `low_till`: NDTI percent changes in between 30 and 70
- `high_till`: NDTI percent change greater than or equal to 70

When multiple tillage observations occur within a parcel-year, the dominant
tillage class is used.

The script writes:
- `all_data.csv`: represents the crop/tillage matched subset. It is used to
calculate the historical tillage baseline for future tillage projections.

It is not used as the full crop population for crop-matrix optimization or
crop prediction because only parcels with usable tillage observations are
included.

- county tillage transition matrices


## 3. Scenario optimization

`scenarios.R` reads scenario inputs from:
- `BAU_Targets.csv`
- `NBS_Targets.csv`

The historical county crop transition matrices (created in `transition_matrix.R`) are optimized toward the
2045 crop distribution specified by the configured matrix target scenario.

The default matrix target scenario is:

```r
matrix_target_scenario_name = "BAU_Targets"
```

Scenario crop acreage is rescaled to the total acreage represented by the
observed starting crop population. Therefore, the optimization targets the
scenario crop distribution and direction of change rather than requiring the
optimized projection to reproduce the scenario's absolute acreage exactly.

The crop starting distribution is constructed from
`crop_year_states_cleaned.csv` using each parcel's latest observed crop state
up to the starting year.

The script optimizes the crop matrix once per county and also produces
scenario-specific tillage targets for both `BAU_Targets` and `NBS_Targets`

## 4. Parcel projections

`predict_and_store.R` uses the optimized crop matrices and scenario-specific
tillage targets to generate annual parcel-level projections.

Crop and tillage projections use different historical inputs:

- `crop_year_states_cleaned.csv` provides the full crop population used for
  crop prediction and parcel metadata.
- `all_data.csv` provides the historical crop/tillage matched subset used to
  calculate baseline tillage distributions.

Crop states are projected from each parcel's historical crop state using the
optimized county transition matrix.

Projected crop classes are then translated to parcel-level assignments while
preserving the acreage distribution implied by the optimized matrix as closely
as possible. The optimized crop matrix is shared between prediction scenarios.
BAU and NBS differ through their scenario-specific tillage targets rather than through
separately optimized crop matrices.

Historical tillage shares are gradually shifted toward the scenario-specific
2045 tillage targets. This produces separate BAU and NBS tillage trajectories.


Predictions are produced annually for 2024-2045.

## Transition functions
Reusable transition functions are defined in:

```text
modules/data.remote/R/transition_functions.R
```

The main functions are:
- `make_transitions()`
- `make_transition_matrix()`
- `make_grouped_transition_matrices()`

These functions are exported from `PEcAn.data.remote` and are called from the
workflow scripts using the package namespace.

## Configuration

The scripts use the environment variable:
```r
CCMMF_WORK_ROOT
```

Set this variable to the workspace where intermediate files and workflow
outputs should be stored. For example:

```r
Sys.setenv(CCMMF_WORK_ROOT = "/projectnb/dietzelab/ananyak")
```
Or, more generally,

```r
Sys.setenv(CCMMF_WORK_ROOT = "/path/to/your/folder")
```

Shared CCMMF inputs default to:

```text
/projectnb/dietzelab/ccmmf
```

where applicable.

## Running the workflow

Run the scripts in this order:

```text
transition_matrix.R
tillage.R
scenarios.R
predict_and_store.R
```

Each script depends on outputs generated by earlier stages of the workflow.
If a required upstream file is missing, the script stops with a message
indicating which earlier step should be run first.

## Validation

The workflow includes checks for:

- required input columns
- valid transition probabilities
- transition-matrix row sums
- negative or greater-than-one probabilities
- scenario mapping totals
- optimizer status
- optimized scenario fit
- parcel acreage assignment differences

Optimized crop matrices are expected to move projected crop distributions
toward the configured scenario targets. Exact equality with scenario acreage
is not expected.
