A generic template for adding a new model to PEcAn
==========================================================================

Adding a new model to PEcAn in a few easy steps:
1. add modeltype to BETY
2. add a model and PFT to BETY for use with modeltype
3. implement 3 functions as described below
4. execute pecan with new model

There are 3 functions that will need to be implemented, each of these
functions will need to have MODEL be replaced with the actual modeltype as
it is defined in the BETY database.

* `write.config.MODEL.R`

 This will write the configuratin file as well as the job launcher used by
 PEcAn. There is an example of the job execution script in the template
 folder. The configuration file can also be a template that is found based
 on the revision number of the model. This should use the computed results
 specified in defaults and trait.values to write a configuration file
 based on the PFT and traits found.

* `met2model.MODEL.R`

 This will convert the standard Met CF file to the model specific file
 format. This will allow PEcAn to create metereological files for the
 specific site and model. This will only be called if no meterological
 data is found for that specific site and model combination.

* `model2netcdf.MODEL.R`

 This will convert the model specific output to NACP Intercomparison
 format. After this function is finished PEcAn will use the generated
 output and not use the model specific outputs. The outputs should be
 named YYYY.nc
