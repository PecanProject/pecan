# Crop Transition Matrices and Scenario Optimization

## Overview
This workflow is split up into two scripts. `transition_matrix.R` converts historical LandIQ crop observations into county-level
crop transition matrices, and `scenarios.R `optimizes those matrices toward the 2045 targets.

The final optimized matrices produced in `scenarios.R` are the ones used for the crop prediction workflow, and therefore serve as the  basis of the prediction pipeline. 

## Set up
This script is the first in the workflow, and builds off the LandIQ crop data from the inventory section. It also begins to create  the outputs that will be used downstream for crop class, 
date, and management projections. Each script starts off the same with a set up section that configures the paths and loads the files needed. You can refer to `config.yml `for what each 
setting and how it points at the necessary paths. 

This script uses 2 files: 
1. The LandIQ crop data `crops_all_years.parq` read as `landiq_path` in the configuration 
2. Landiq lookup `LandIQ_cropCode_lookup_table.csv` read as `lookup_path` in the configuration
You can refer to `config.yml` for a breif explanation of what each table contains, and where this data is stored  

## transition_matrix.R
This script starts off by filtering the LandIQ records to agricultural classes and to the years between `historical_start_year` and `start_year`, 
and then assigns each parcel to a county. It is important to note that while downstream prediction files will be organized statewide 
per year, calculations are done on a county level to factor in California's diverse climate. 

### Running the script + important simplifications
As the script creates county transition matrices, a few simplifications are made regarding crop sequences. 

1. Sequence 'cleaning' is applied for crop sequences with `X` (unclassified fallow) where it can be considered a data error. For example, 
a single `X` between two identical classes, or an `X` at the start/end of an identical sequence were filled in with the same class:
  - `T-T-X-T` --> `T-T-T-T`
  - `T-T-T-X` --> `T-T-T-T`
Longer runs of X or unidentical sequences are not changed.

2. A parcel can have up to four observations per year due to seasons, but the transition matrices are yearly probabilities. Each 
parcel-year is reduced to a single dominant crop, either the most frequent class or the season 2 crop when all observations differ.
The script also records `non_dom_prob` (the fraction of that year's observations that disagree with the assigned state) in the 
output file `crop_year_states_cleaned`, which is described below. 

Once the crop sequences are cleaned and reduced to one county, crop class, and year per parcel id, the data is converted and grouped 
per county, and a transition matrix is built for each over the eleven LandIQ crop classes: `YP`,`D`,`T`,`G`,`F` `P`,`C`, `V`, `R`, `I`, `X`. 

An example transition matrix for Fresno County is shown below: 
(rows = crop class at time *t*, columns = crop class at *t*+1):

| From \ To | YP    | D     | X | T     | G     | F     | P     | C     | I | V     | R     |
|-----------|------:|------:|--:|------:|------:|------:|------:|------:|--:|------:|------:|
| **YP**    | 0.289 | 0.501 | 0 | 0.007 | 0.004 | 0.001 | 0.001 | 0.165 | 0 | 0.032 | 0.000 |
| **D**     | 0.012 | 0.956 | 0 | 0.013 | 0.003 | 0.006 | 0.002 | 0.007 | 0 | 0.001 | 0.000 |
| **X**     | 0     | 0     | 0 | 0     | 0     | 0     | 0     | 0     | 0 | 0     | 0     |
| **T**     | 0.016 | 0.038 | 0 | 0.789 | 0.048 | 0.101 | 0.005 | 0.001 | 0 | 0.002 | 0.000 |
| **G**     | 0.037 | 0.036 | 0 | 0.205 | 0.566 | 0.107 | 0.039 | 0.004 | 0 | 0.003 | 0.003 |
| **F**     | 0.011 | 0.026 | 0 | 0.251 | 0.086 | 0.579 | 0.043 | 0.000 | 0 | 0.000 | 0.003 |
| **P**     | 0.025 | 0.071 | 0 | 0.056 | 0.033 | 0.050 | 0.741 | 0.022 | 0 | 0.001 | 0.000 |
| **C**     | 0.011 | 0.063 | 0 | 0.021 | 0.005 | 0.007 | 0.003 | 0.889 | 0 | 0.001 | 0     |
| **I**     | 0     | 0     | 0 | 0     | 0     | 0     | 0     | 0     | 0 | 0     | 0     |
| **V**     | 0.020 | 0.049 | 0 | 0.021 | 0.004 | 0.007 | 0.003 | 0.013 | 0 | 0.883 | 0     |
| **R**     | 0.015 | 0.113 | 0 | 0.288 | 0.076 | 0.077 | 0.006 | 0     | 0 | 0     | 0.425 |

Notice that the largest values fall along the diagonals, as crops tend to stay the same throughout the years. 

### Outputs: 
This script will produce 3 outputs that will all be used in future downstream predictions. 
1. `crops_full_counties.csv`: full record with SUBCLASS and other crop data

2. `crop_year_states_cleaned.csv`: the one crop state per parcel-year file

3. `county_crop_matrices/`: the folder containing one transition matrix per county 


## scenarios.R

scenarios.R holds the second part of the matrix development section. If you take a county's historical matrix and run it forward to 2045, 
you won't automatically achieve the scenario's target acreage. Our goal is to take historical data and reach target goals, but these 
scenarios reflect changes that haven't happened yet. Therefore the matrix has to be adjusted.

However, there are endless combinations of transition probabilities that can be in the transition matrix, and therefore several matrices 
could hit the 2045 target. So which should we actually use for this prediction workflow? This optimizer script gives us a rule for picking one. 

Overall, this script's rule is 'change the historical matrix as little as possible while still reaching the target.' The parameter  
`lambda_target`, defined in `config.yml`, sets how strictly the matrix has to hit the target. A higher value means the optimizer will 
accept bigger changes to get closer, acting as a trade off between changing the historical matrix and reaching a target. 

### Running the script + important simplifications

The scenario target sheets currently define their yearly goals in terms of acres, which means the optimization also must be in terms of 
acres. This is done by first creating `X0`, the county's starting vector. The script records each parcel's most recent observed crop class up to and 
including `start_year` (currently 2023), and then sums parcel acreage by class. This gives one starting acreage per crop class for each county.
For example, the optimization for Fresno county assigns the following X0 vector:
(values are rounded to the nearest whole number)

| Class | YP     | D       | X | T       | G       | F      | P      | C      | I | V       | R     |
|-------|-------:|--------:|--:|--------:|--------:|-------:|-------:|-------:|--:|--------:|------:|
| Acres | 12,212 | 535,417 | 0 | 152,918 | 130,621 | 96,818 | 52,583 | 63,333 | 0 | 141,266 | 3,744 |

Note that the starting acreage is documented in output files post optimization, which will be explained in the outputs section. 

Next, it is also important to note that The Magic scenario sheets and LandIQ classes do not perfectly map to each other. For example
one code in LandIQ may not mean the same thing in the scenario sheet, or it may not be included at all. This script accounts for these 
mismatches and applies a crop mapping before optimization. 
  - Most mapping is one-to-one, but "All Other Field Crops" splits across `F`/`P` and "Annual Cropland" across `F`/`G`/`T`/`R`, all 
    weighted by the county's current acreage in each. 
    
This script only needs to run once because BAU and NBS targets currently share the same target acreage values. This script has one of the 
longer run times due to the optimization step, but is expected to take ~90 minutes to 1 hour. 
    
*disclaimer* 
The parcel list and their acreages are fixed at the start, meaning total land stays constant through 2045. The model projects parcels 
changing crops within a fixed land base. It does not represent land being converted to other uses or new land being added.

As the optimizer runs per county, information including the starting acres, target acres, and some matrix checks will print inside 
the console, so you can observe the optimizer's behavior in real time. These printed outputs will also be saved as final outputs for future reference.

## Outputs
Outputs go a new folder `county_optimized_matrices/` that will be created relative to your `work_root`. The folder will contain 2 
files for each county, and an overall manifest:
1. Optimized matrix, `<county>_crop_matrix.csv`

2. Optimization summary, `optimization_summary_<county>.csv	`, that records the starting acres (X0), 2045 targets acres, projected 
2045 acres under the original and optimized matrices, and the error between projected and target acres.

3. A full run manifest, `all_county_run_manifest.csv`, that records its run status (success, poor_fit, optimizer_failed, or error), 
acreage totals, overall fit error, and paths to that county's output files.
