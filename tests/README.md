## Tests:

This folder provides some simple tests for each model. They can be run with the following syntax:

```
  R --vanilla -- --settings <pecanfile> < workflow.R 

# for example to run the ed workflow:
  R --vanilla -- --settings pecan.ed.xml < workflow.R 
```

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