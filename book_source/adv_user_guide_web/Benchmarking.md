# Benchmarking

Benchmarking is the process of comparing model outputs against either experimental data or against other model outputs as a way to validate model performance. 
We have a suit of statistical comparisons that provide benchmarking scores as well as visual comparisons that help in diagnosing data-model and/or model-model differences.

There are currently two ways to use the benchmarking tools. 

1. **New Run**: Set up a new model run through pecan interface. After performing the run, immediately load up input data that can be compared to the model outputs. This can be done using the online shiny benchmarking app.

2. **Clone Run**: Set up a new model run in pecan that will use preexisting run settings that have been stored in BETY as  a "reference run" record. This setup is more involved and currently has to be done by hand by editing the pecan.xml document in Rstudio. 

## Before benchmarking

All the data that you want to compare with model runs must be registered in the database. This is currently a step that must be done by hand either from the command line or through the online BETY interface. 
I.e. the data must have three records:
1. An input record (Instructions here)
2. A database file record (Instructions here)
3. A format record (Instructions here)


## Benchmarking in pecan.xml

The `pecan.xml` has an _optional_ benchmarking section. Ultimately, the user should not have to edit this document by hand, but it is still necessary for cloning runs. 

Below are all the possible benchmarking sections. 
If you provide the ensemble id, then you do not need to provide the reference run id and vice versa.

`<benchmarking>` settings:

- `ensemble_id`: the id of the ensemble that you will be using - the settings from this ensemble will be saved in a reference run record and then `ensemble_id` will be replaced with `reference_run_id` 
- `input_id`: the id(s) of the benchmarking data  (required)
- `variable_id`: the id(s) of the variable(s) of interest within the data. If you leave this blank, all variables that are shared between the input and model output will be used. 
- `metric_id`: the id(s) of the metric(s) to be calculated. If you leave this blank, all metrics will be used. 
- `new_run`: TRUE = create new run, FALSE = use existing run (required)

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
   <reference_run_id>1000000050</reference_run_id>
 </benchmark>
```

## 1. New Run

- Setup your run (preferably using the pecan interface). 
- When the run has completed, navigate to the workflow visualization Shiny app. 
- Load model data
  - Make sure that your model output is loading properly (i.e. you can see plots of your data)
- Load benchmarking data
  - Again make sure that you can see the uploaded data plotted alongside the model output. In the future there will be more tools for double checking that your uploaded data is appropriate for benchmarking, but for now you need to do the sanity checks by  hand. 
- Navigate to the Benchmarking tab
  - The first step is to register the new model run as a reference run in the database. Benchmarking cannot be done before this step is completed. When the reference run record has been created, additional menus for benchmarking will appear. 
  - Next from the menu select
    - The variables in the uploaded data that you wish to compare with model output. Note: this list shows all available variables regardless of whether or not there is a comparable variable in the model output. Thus if you select a variable to be benchmarked and it does not show up in the results, double check that it is also in the model output! 
    - The numerical metrics you would like to use in your comparison. 
    - Additional comparison plots that you would like to see.
  - All these selections populate the benchmarking section of the xml witch is then saved in the same location as the original run output. 
- Visualizations of benchmarking results are in progress. 
  - Currently all results are stored in the `benchmarking.output.Rdata` object which is stored in the same folder as the original model run. This loads a list  called `results`  which contains the following:
    - `input` a list element for each input file that was used for benchmarking
      - `bench.results`: a data frame of all numeric benchmarking scores
      - `format`: a data frame that can be used to see how the input data was transformed to make it comparable to the model output. This involves converting from the original variable names and units to the internal pecan standard. 
      - `aligned.dat`: a data frame of the final aligned model and input values. 
  - All plots are saved as pdf files (with names beginning with "benchmarking.")

## 2. Clone Run

It may be the case that you want to exactly recreate a run that was done on a different server. Or that you want to take the settings that were used to set up a run of ED2 at Harvard Forrest and use them to set up a run at the same site but using SIPNET instead. For both of these cases, instead of recreating the runs by hand every time, we are developing a way to "clone" runs. 

The cloning workflow takes in the reference run id and imports all the saved settings from the database. This populates the entire pecan.xml file with the information necessary to do a run. Thus the benchmarking section of the pecan.xml only requires the reference run id and the input id. An ensemble id cannot be put in because the run hasn't happened yet. 

