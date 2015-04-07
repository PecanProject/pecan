## Tests:

This folder provides some simple tests for each model. They can be run with the following syntax:

```
./workflow.R --settings <pecanfile>

# for example to run the ed workflow:
./workflow.R --settings pecan.ed.xml
```

Since workflow.R is a simple rscript you can pass in standard R flags, such as --vanilla, etc.

## Files:

* workflow.R       : Generic workflow to run PEcAn; it will skip previously completed steps.

## Settings Files 

* pecan.ed.xml     : pecan configuration to run ED on ebifarm.
* pecan.sipnet.xml : pecan configuration to run SIPNET on NIWOT.
* pecan.biocro.xml : pecan configuration to run BIOCRO

## Debugging:

* add `debugonce(somefunction)` to workflow.R after PEcAn.all is loaded
* the run will stop when it gets to the specified function, at which point you can step through.
* Do not commit changes to workflow.R used for debugging

For example, you can add `debugonce(run.meta.analysis)` to `workflow.R` and then 

```{bash}
R --vanilla -- --settings <pecanfile>
```
```{r}
debugonce(run.meta.analysis)
source "workflow.R"
```