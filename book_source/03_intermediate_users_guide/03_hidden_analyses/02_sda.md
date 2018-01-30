## State data assimilation (SDA)

`sda.enkf.R` is housed within: **/pecan/modules/assim.sequential/R**
The tree ring tutorial is housed within: **/pecan/documentation/tutorials/StateAssimilation**

### **sda.enkf.R Description**
This is the main ensemble Kalman filter and generalized filter code. Originally, this was just ensemble Kalman filter code. Mike Dietze and Ann Raiho added a generalized ensemble filter to avoid filter divergence. The output of this function will be all the of run outputs, a PDF of diagnostics, and an Rdata object that includes three lists:

* FORECAST will be the ensemble forecasts for each year
* ANALYSIS will be the updated ensemble sample given the NPP observations
* enkf.params contains the prior and posterior mean vector and covariance matrix for each time step.

### **sda.enkf.R Arguments**

* settings - (required) [pecan.SDA.xml][State Data Assimilation Tags Example] settings object

* obs.mean - (required) a list of observation means named with dates in YYYY/MM/DD format

* obs.cov - (required) a list of observation covariances names with dates in YYYY/MM/DD format

* IC - (optional) initial condition matrix (dimensions: ensemble memeber # by state variables). Default is NULL.

* Q - (optional) process covariance matrix (dimensions: state variable by state variables). Defualt is NULL.

### State Data Assimilation Workflow
Before running sda.enkf, these tasks must be completed (in no particular order),

* Read in a [pecan.SDA.xml][State Data Assimilation Tags Example] settings file with tags listed below. i.e. read.settings('pecan.SDA.xml')

* Load data means (obs.mean) and covariances (obs.cov) as lists with PEcAn naming and unit conventions. Each observation must have a date in YYYY/MM/DD format  (optional time) associated with it. If there are missing data, the date must still be represented in the list with an NA as the list object.

* Create initial conditions matrix (IC) that is state variables columns by ensemble members rows in dimension. [sample.IC.MODEL][sample.IC.MODEL.R] can be used to create the IC matrix, but it is not required. This IC matrix is fed into write.configs for the initial model runs.

The main parts of the SDA function are:

Setting up for initial runs:

* Set parameters
  
* Load initial run inputs via [split.inputs.MODEL][split.inputs.MODEL.R]

* Open database connection

* Get new workflow ids
  
* Create ensemble ids

Performing the initial set of runs

Set up for data assimilation

Loop over time

* [read.restart.MODEL][read.restart.MODEL.R] - read model restart files corresponding to start.time and stop.time that you want to assimilate data into
  
* Analysis - There are four choices based on if process variance is TRUE or FALSE and if there is data or not. [See explaination below.][Analysis Options]
  
* [write.restart.MODEL][write.restart.MODEL.R] - This function has two jobs. First, to insert adjusted state back into model restart file. Second, to update start.time, stop.time, and job.sh.
  
* run model
  
Save outputs

Create diagnostics

### State Data Assimilation Tags Example

```
<state.data.assimilation>
  <n.ensemble>25</n.ensemble>
  <process.variance>FALSE</process.variance>
  <sample.parameters>FALSE</sample.parameters>
   <state.variables>
    <variable>
      <variable.name>AGB.pft</variable.name>
      <unit>MgC/ha/yr</unit>
      <min_value>0</min_value>
      <max_value>100000000</max_value>
    </variable>
    <variable>
      <variable.name>TotSoilCarb</variable.name>
      <unit>KgC/m^2</unit>
      <min_value>0</min_value>
      <max_value>100000000</max_value>
    </variable>
  </state.variables>
  <spin.up>
    <start.date>1950/01/01</start.date>
    <end.date>1960/12/31</end.date>
  </spin.up>
  <forecast.time.step>1</forecast.time.step>
  <start.date>1961/01/01</start.date>
  <end.date>2010/12/31</end.date>
 </state.data.assimilation>
```

### State Data Assimilation Tags Descriptions

* **n.ensemble** : [required] Number of ensemble members. Should be much larger than number of state variables you are assimilating.
* **process.variance** : [optional] TRUE/FLASE flag for if process variance should be estimated (TRUE) or not (FALSE). If TRUE, a generalized ensemble filter will be used. If FALSE, an ensemble Kalman filter will be used. Default is FALSE.
* **sample.parameters** : [optional] TRUE/FLASE flag for if parameters should be sampled for each ensemble member or not. This allows for more spread in the initial conditions of the forecast.
* **state.variable** : [required] State variable that is to be assimilated (in PEcAn standard format).
* **spin.up** : [required] start.date and end.date for initial model runs.
* **_NOTE:_** start.date and end.date are distinct from values set in the run tag because initial runs can be done over a subset of the full run.
* **forecast.time.step** : [optional] In the future, this will be used to allow the forecast time step to vary from the data time step.
* **start.date** : [optional] start date of the state data assimilation (in YYYY/MM/DD format) 
* **end.date** : [optional] end date of the state data assimilation (in YYYY/MM/DD format)
* **_NOTE:_** start.date and end.date are distinct from values set in the run tag because this analysis can be done over a subset of the run.

### Model Specific Functions for SDA Workflow

#### read.restart.MODEL.R
The purpose of read.restart is to read model restart files and return a matrix that is site rows by state variable columns. The state variables must be in PEcAn names and units. The arguments are:

* outdir - output directory

* runid - ensemble member run ID

* stop.time - used to determine which restart file to read (in POSIX format)

* settings - [pecan.SDA.xml][State Data Assimilation Tags Example] settings object

* var.names - vector with state variable names with PEcAn standard naming. Example: c('AGB.pft', 'TotSoilCarb')

* params - parameters used by ensemble member (same format as write.configs)

#### write.restart.MODEL.R
This model specific function takes in new state and new parameter matrices from sda.enkf.R after the analysis step and translates new variables back to the model variables. Then, updates start.time, stop.time, and job.sh so that start.model.runs() does the correct runs with the new states. In write.restart.LINKAGES and write.restart.SIPNET, job.sh is updated by using write.configs.MODEL.

* outdir - output directory

* runid - run ID for ensemble member

* start.time - beginning of model run (in POSIX format)

* stop.time - end of model run (in POSIX format)

* settings - [pecan.SDA.xml][State Data Assimilation Tags Example] settings object

* new.state - matrix from analysis of updated state variables with PEcAn names (dimensions: site rows by state variables columns)

* new.params - In the future, this will allow us to update parameters based on states (same format as write.configs)

* inputs - model specific inputs from [split.inputs.MODEL][split.inputs.MODEL.R] used to run the model from start.time to stop.time

* RENAME - [optional] Flag used in write.restart.LINKAGES.R for development.

#### split.inputs.MODEL.R
This model specific function gives the correct met and/or other model inputs to settings$run$inputs. This function returns settings$run$inputs to an inputs argument in sda.enkf.R. But, the inputs will not need to change for all models and should return settings$run$inputs unchanged if that is the case.

* settings - [pecan.SDA.xml][State Data Assimilation Tags Example] settings object

* start.time - start time for model run (in POSIX format)

* stop.time - stop time for model run (in POSIX format)

#### sample.IC.MODEL.R
This model specific function is optional. But, it can be used to create initial condition matrix (IC) with # state variables columns by # ensemble rows. This IC matrix is used for the initial runs in sda.enkf.R in the write.configs.MODEL function.

* ne - number of ensemble members

* state - matrix of state variables to get initial conditions from

* year - used to determine which year to sample initial conditions from 

### Analysis Options
There are four options depending on whether process variance is TRUE/FALSE and whether or not there is data or not.

* If there is no data and process variance = FALSE, there is no analysis step.

* If there is no data and process variance = TRUE, process variance is added to the forecast.

* If there is data and process variance = TRUE, [the generalized ensemble filter][The Generalized Ensemble Filter] is implemented with MCMC.

* If there is data and process variance = FALSE, the Kalman filter is used and solved analytically.

### The Generalized Ensemble Filter
An ensemble filter is a sequential data assimilation algorithm with two procedures at every time step: a forecast followed by an analysis. The forecast ensembles arise from a model while the analysis makes an adjustment of the forecasts ensembles from the model towards the data. An ensemble Kalman filter is typically suggested for this type of analysis because of its computationally efficient analytical solution and its ability to update states based on an estimate of covariance structure. But, in some cases, the ensemble Kalman filter fails because of filter divergence. Filter divergence occurs when forecast variability is too small, which causes the analysis to favor the forecast and diverge from the data. Models often produce low forecast variability because there is little internal stochasticity. Our ensemble filter overcomes this problem in a Bayesian framework by including an estimation of model process variance. This methodology also maintains the benefits of the ensemble Kalman filter by updating the state vector based on the estimated covariance structure.

This process begins after the model is spun up to equilibrium.

The likelihood function uses the data vector $\left(\boldsymbol{y_{t}}\right)$ conditional on the estimated state vector $\left(\boldsymbol{x_{t}}\right)$ such that
  
 $\boldsymbol{y}_{t}\sim\mathrm{multivariate\:normal}(\boldsymbol{x}_{t},\boldsymbol{R}_{t})$
 
where $\boldsymbol{R}_{t}=\boldsymbol{\sigma}_{t}^{2}\boldsymbol{I}$ and $\boldsymbol{\sigma}_{t}^{2}$ is a vector of data variances. To obtain an estimate of the state vector $\left(\boldsymbol{x}_{t}\right)$, we use a process model that incorporates a process covariance matrix $\left(\boldsymbol{Q}_{t}\right)$. This process covariance matrix differentiates our methods from past ensemble filters. Our process model contains the following equations

$\boldsymbol{x}_{t}	\sim	\mathrm{multivariate\: normal}(\boldsymbol{x}_{model_{t}},\boldsymbol{Q}_{t})$

$\boldsymbol{x}_{model_{t}}	\sim	\mathrm{multivariate\: normal}(\boldsymbol{\mu}_{forecast_{t}},\boldsymbol{P}_{forecast_{t}})$

where $\boldsymbol{\mu}_{forecast_{t}}$ is a vector of means from the ensemble forecasts and $\boldsymbol{P}_{forecast_{t}}$ is a covariance matrix calculated from the ensemble forecasts. The prior for our process covariance matrix is $\boldsymbol{Q}_{t}\sim\mathrm{Wishart}(\boldsymbol{V}_{t},n_{t})$ where $\boldsymbol{V}_{t}$ is a scale matrix and $n_{t}$ is the degrees of freedom. The prior shape parameters are updated at each time step through moment matching such that

$\boldsymbol{V}_{t+1}	=	n_{t}\bar{\boldsymbol{Q}}_{t}$

$n_{t+1}	=	\frac{\sum_{i=1}^{I}\sum_{j=1}^{J}\frac{v_{ijt}^{2}+v_{iit}v_{jjt}}{Var(\boldsymbol{\bar{Q}}_{t})}}{I\times J}$

where we calculate the mean of the process covariance matrix $\left(\bar{\boldsymbol{Q}_{t}}\right)$ from the posterior samples at time t. Degrees of freedom for the Wishart are typically calculated element by element where $v_{ij}$ are the elements of $\boldsymbol{V}_{t}$. $I$ and $J$ index rows and columns of $\boldsymbol{V}$. Here, we calculate a mean number of degrees of freedom for $t+1$ by summing over all the elements of the scale matrix $\left(\boldsymbol{V}\right)$ and dividing by the count of those elements $\left(I\times J\right)$. We fit this model sequentially through time in the R computing environment using R package 'rjags.'
