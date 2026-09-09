# Crop Transition Matrices and Scenario Optimization

## Overview

This workflow converts historical LandIQ crop observations into county-level
crop transition matrices and then optimizes those matrices toward the configured
2045 crop acreage distribution.

The workflow consists of two main stages:

1. historical crop transition-matrix construction
2. county crop-matrix optimization

The resulting optimized matrices are used by the crop-prediction workflow to
generate parcel-level future crop identity through 2045.

## Workflow

```text
Historical LandIQ crop observations
        |
        v
assign parcels to counties
        |
        v
clean short unknown crop runs
        |
        v
reduce seasonal observations
to one annual crop state
        |
        v
crop_year_states_cleaned.csv
        |
        v
county transition matrices
        |
        +-----------------------------+
                                      |
2045 scenario crop targets            |
        |                             |
        +-------------+---------------+
                      |
                      v
          optimize county matrices
                      |
                      v
county_optimized_matrices/
```

## 1. Historical crop inputs

Historical crop observations come from harmonized LandIQ records.

The workflow uses agricultural LandIQ classes identified through the LandIQ
crop lookup table.

Relevant historical fields include:

```text
parcel_id
year
season
CLASS
SUBCLASS
centx
centy
ACRES
```

Parcel centroids are spatially joined to California counties so that crop
transition matrices can be estimated separately for each county.

## 2. Agricultural crop states

The crop-transition state space is:

```text
YP
D
X
T
G
F
P
C
I
V
R
```

These represent the LandIQ crop classes retained by the framework.

The transition convention is:

```text
A[i,j] =
    probability that crop class i
    transitions to crop class j next year
```

Each row of the matrix therefore sums to 1.

## 3. Cleaning unknown crop states

LandIQ class:

```text
X
```

represents an unknown or unresolved crop state.

Short runs of `X` are corrected only when surrounding observations provide
enough evidence for a reasonable replacement.

The cleaning procedure includes:

1. `T-X-T` is replaced with `T-T-T`
2. an `X` run of length two or less bounded by the same crop class is filled
3. an edge `X` can be replaced using the adjacent observed crop
4. a remaining short `X` run can be filled when one valid non-X neighbor is
   available

Long or unresolved `X` runs remain `X`.

The goal is to remove obvious short-term classification gaps without
artificially assigning crop identity to longer uncertain periods.

## 4. Annual crop state

A parcel can contain multiple LandIQ observations within the same year because
multiple growing seasons may be recorded.

These observations are reduced to one annual crop state.

If two or three crop observations are all different and season 2 exists, the
season-2 crop is treated as the annual dominant state.

Otherwise, the modal crop class is used.

The workflow also calculates:

```text
non_dom_prob
```

which represents the fraction of within-year observations that do not match
the dominant crop state.

This provides a measure of uncertainty in the annual classification.

## 5. Historical crop outputs

The transition-matrix workflow writes:

```text
crops_full_counties.csv
```

This preserves historical crop class/subclass information for later crop
subclass assignment and management-event projections.

It also writes:

```text
crop_year_states_cleaned.csv
```

with fields including:

```text
county
county_geoid
parcel_id
year
state
ACRES
non_dom_prob
```

This is the primary historical crop dataset used by both matrix optimization
and future crop prediction.

## 6. County crop transition matrices

Transitions are calculated between consecutive annual crop states for each
parcel.

Reusable transition functions from `PEcAn.data.remote` are used to construct
county-specific matrices.

The primary functions are:

```text
make_transitions()
make_transition_matrix()
make_grouped_transition_matrices()
```

One historical matrix is written per county:

```text
county_crop_matrices/
├── Alameda_crop_matrix.csv
├── ...
└── Yuba_crop_matrix.csv
```

## 7. Optimization inputs

The optimization stage uses:

```text
crop_year_states_cleaned.csv
county_crop_matrices/
MAGiC_scenarios_FINAL/BAU_Targets.csv
```

The default crop matrix target source is:

```text
BAU_Targets
```

Crop optimization is performed once because the current BAU and NBS scenarios
use the same crop acreage targets.

The resulting optimized crop matrices are therefore shared by both scenarios.

## 8. Starting crop acreage distribution

For each county, the starting crop distribution is constructed from each
parcel's latest observed crop state at or before:

```text
2023
```

Parcel acreage is summed by crop state to create:

```text
X0
```

where `X0` is the county crop acreage vector used by the optimizer.

## 9. Mapping MAGiC crop targets to LandIQ states

Scenario crop categories are mapped to LandIQ crop classes.

Examples include:

```text
All Other Berries              -> T
Strawberries Fresh Market      -> T
All Other Fruit Crops          -> D
All Other Nut Crops            -> D
Almonds                        -> D
Pome Fruit                     -> D
Stone Fruit                    -> D
Citrus                         -> C
Grapes Dried/Raisins           -> V
Grapes Table                   -> V
Grapes Wine                    -> V
Fallow                         -> X
```

Some scenario categories are split across multiple LandIQ states.

```text
All Other Field Crops
    -> F / P

Annual Cropland
    -> F / G / T / R
```

Split weights are based on the county's historical starting acreage
distribution.

If no historical acreage is available, the target is divided equally among
the relevant states.

## 10. Scenario acreage scaling

Scenario target acreage is rescaled to the total acreage represented by `X0`.

Therefore, optimization primarily targets the scenario:

```text
crop composition
and
direction of change
```

rather than requiring the projected county acreage to equal the scenario's
absolute reported acreage exactly.

This avoids mismatches caused by differences between the scenario acreage
inventory and the parcel acreage represented in LandIQ.

## 11. Matrix optimization

For each county, the optimizer searches for an adjusted transition matrix:

```text
A'
```

that remains close to the historical transition matrix:

```text
A
```

while producing a 2045 crop distribution close to the scenario target.

The projection period is:

```text
2023 -> 2045
```

or:

```text
22 annual transitions
```

The projected county acreage is:

```text
X2045 = X0 A'^22
```

The optimization objective combines:

```text
historical matrix perturbation
+
scenario target mismatch
```

Conceptually:

```text
minimize
    ||A' - A||²
    +
    lambda × target mismatch
```

subject to:

```text
0 <= A'[i,j] <= 1

sum_j A'[i,j] = 1
```

The default optimizer is COBYLA through `nloptr`.

## 12. Optimization output

One optimized matrix is written per county:

```text
county_optimized_matrices/
├── Alameda_crop_matrix.csv
├── ...
└── Yuba_crop_matrix.csv
```

These matrices are the direct inputs to the crop-prediction workflow.

## 13. Optimization status

County optimization results are classified using statuses such as:

```text
success
poor_fit
optimizer_failed
```

A `poor_fit` status indicates that the final projected crop-share error exceeds
the configured tolerance.

Optimizer failures should be investigated before the corresponding county
matrix is used for parcel projection.

## 14. Validation

The workflow checks:

- required historical input fields
- county assignment
- valid annual crop states
- transition-matrix row sums
- transition probabilities between 0 and 1
- scenario crop mappings
- optimizer convergence
- final 2045 crop-share fit
- missing county outputs

Particular attention should be given to persistent crop classes such as:

```text
D
C
V
YP
```

If optimization substantially lowers historically high self-transition
probabilities for perennial crops, the optimized matrix should be reviewed
before parcel prediction.

## 15. Outputs used downstream

The main downstream inputs are:

```text
crop_year_states_cleaned.csv
crops_full_counties.csv
county_optimized_matrices/
```

These feed directly into the future crop-prediction workflow.

## 16. Running order

Run:

```text
transition_matrix.R
        |
        v
optimize_crop_matrices_only.R
        |
        v
crop_prediction.R
```

The transition-matrix stage must be completed before optimization, and
optimization must be completed before parcel-level crop prediction.