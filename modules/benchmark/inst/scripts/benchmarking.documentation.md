# Benchmarking Documentation

There are two ways to use `calc_benchmark`

1. With `workflow.R` to create new outputs
2. With `benchmark.workflow.R` to use exiting outputs

Both cases start with the same settings XML file, the only difference is that
`settings$new_run` will be `TRUE` or `FALSE`

This XML file should ultimately be generated through an online interface but for now can be
constructed by hand.

## The bare minimum that the XML needs to contain

- `<database>` information
- `<host>` information (though in this case host doesn't matter because everything is happening locally)
- `<outdir>`, `<rundir>`, `<modeloutdir>`
- `<benchmarking>` settings:
  - `ensemble_id`: the id of the ensemble that you will be using - either to determine the settings for a new ensemble or to provide the settings for the existing ensemble
  - `input_id`: the id(s) of the benchmarking data
  - `variable_id`: the id(s) of the variable(s) of interest within the data
  - `metric_id`: the id(s) of the metric(s) to be calculated
  - `new_run`: TRUE = create new run, FALSE = use existing run
  
Note: you don't need the benchmark reference run id. That will be retrieved/created later.

Example:

```xml
 <benchmark>
     <ensemble_id>1000004796</ensemble_id> 
     <input_id>1000008121</input_id>
     <variables>
       <variable_id>411</variable_id> 
     </variables>
     <metrics>
       <metric_id>1000000003</metric_id> 
     </metrics>
     <new_run>TRUE</new_run>
 </benchmark>

```

## Example XML files

(I'll check them off when they are working)

## 1 variable, 1 metric, 1 site, 1 model

- settings.file: `modules/benchmark/inst/scripts/bm.1var.1metric.1site.1model.xml`
- [x] New Run
- [x] Existing Run

## 2 variables, 2 metric, 1 site, 1 model

- settings.file: `modules/benchmark/inst/scripts/bm.2var.2metric.1site.1model.xml`
- [x] New Run
- [x] Existing Run

## 2 variables, 2 metric, 1 site, 1 model

- settings.file: `modules/benchmark/inst/scripts/bm.2var.2metric.2site.1model.xml`
- settings will now be a multisite object
- [ ] New Run
- [ ] Existing Run

-------------------------------------------------------------------

# Cheatsheet for looking up ids in BETY

## Sites

- DUKE: 853
- ORNL: 666
- RHIN: 1000000008

## Models

- DALEC: 1000000002
- ED2_git: 2000000005

## Variables

- NPP: 411
- LeafLitter: 1000000046
- WoodyLitter: 1000000047

## Metrics

- timeseries.plot: 1000000001
- residual.plot: 1000000002
- MSE: 1000000003
- MAE: 1000000004
- AME: 1000000005
- RAE: 1000000006
- PPMC: 1000000007
- R2: 1000000008

## Inputs: CDIAC

- DUKE
  - Ambient: 1000008119
  - Elevated: 1000008120
- ORNL
  - Ambient: 1000008121
  - Elevated: 1000008122
- RHIN
  - Ambient: 1000008123
  - Elevated: 1000008124
  
## Example Ensemble IDs

#### 1000004796

- DALEC
- ORNL
- Ambient (met = 1000000645)
- start date: 1998/01/01
- end date: 2008/12/31
- workflow id: 1000002773
- input id: 1000008121

#### 1000004806

- DALEC
- DUKE
- Ambient (met = 1000000649)
- start date: 1996/01/01
- end date: 2007/12/31
- workflow id: 1000002775
- input id: 1000008119

#### 1000473576

- ED2_git
- DUKE
- Ambient (met = 1000000649)
- start date: 1997/06/01
- end date: 2007/06/31
- workflow id: 1000003038
- input id: 1000011181
