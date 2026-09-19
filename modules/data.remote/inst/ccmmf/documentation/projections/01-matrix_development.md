# Crop Transition Matrices and Scenario Optimization

## Overview
This workflow is split up into two scripts/stages. `transition_matrix.R` 
converts historical LandIQ crop observations into county-level
crop transition matrices, and then `scenarios.R `optimizes those matrices toward the 2045 crop 
acreage distribution targets.


The final optimized matrices are the ones used for the crop prediction workflow, and therefore serve as the 
basis of the prediction pipeline. 

## transition_matrix.R
This script filters the LandIQ records to agricultural classes and to the years
between `historical_start_year` and `start_year`, then assigns each parcel to a
county.

Matrices are built over the eleven LandIQ crop classes: `YP`,`D`,`T`,`G`,`F` `P`,`C`, 
`V`, `R`, `I`, and `X`. 

### simplifications
1. Sequences cleaning is applied for entries with X, that can be consideres a data error.
For example. a single `X` between two identical classes, or an `X` at the start/end of an identical 
sequence were filled in with the same class. Longer or unidentical runs stay `X`.

2.A parcel can have several observations per year due to season, but the transition matrices predict 
yearly probabilities, so each parcel-year is reduced to a single dominant crop: the season 2 crop 
when all observations differ, otherwise the most frequent class. The script also records 
`non_dom_prob`, the fraction of that year's observations that disagree with the assigned state.

Outputs are `crops_full_counties.csv` (full record with SUBCLASS, needed later
by crop prediction), `crop_year_states_cleaned.csv` (one crop state per
parcel-year), and one matrix per county in `county_crop_matrices/`.

## scenarios.R

If you take a county's historical matrix and run it forward to 2045, you won't automatically get 
the scenario's target acreage. Our goal is to take this historical data and reach target goals, but these scenarios 
reflects policy and market changes that haven't happened yet. Therefore the matrix has to be adjusted.

There are several matrices that could hit the 2045 target, so which should we use for this prediction 
workflow? This optimizer script gives us a rule for picking one. 
Overall, this scripts rule is: change the historical matrix as little as possible while still reaching the target. 
`lambda_target` sets how strictly the matrix has to hit the target — a higher value means the 
optimizer will accept bigger changes to get closer.

`X0` is the starting acreage. For each parcel, the workflow takes the crop it was last observed 
growing at `start_year` and sums acreage by class to get one fixed acreage total per class for each county.

**imporant**
a. The parcel list and their acreages are fixed at the start meaning total land stays constant through 2045. 
The model projects parcels changing crops within a fixed land base, it does not represent farmland 
being converted to other uses or new farmland being added.

b. The Magic scenario sheets and LandIQ classes do not perfectly map to each other. For example,
one code in LandIQ may not mean the same thing in the scenario sheet, or it may not be included at all. 
This script accounts for these mismatches and applies a crop mapping before optimization. 
  - Most mapping is one-to-one, but "All Other Field Crops" splits across `F`/`P` and "Annual 
    Cropland" across `F`/`G`/`T`/`R`, all weighted by the county's current acreage in each. 
    
c. BAU and NBS currently use the same crop acre targets, so this runs once and both
share the matrices. This script will take some time during the optimization step, but run time should 
not be longer than 90 minutes. 

Outputs go to `county_optimized_matrices/`, consisting of one matrix per county, a per-state
summary, and a full run manifest, `all_county_run_manifest.csv`. 
