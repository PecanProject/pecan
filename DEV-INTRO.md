PEcAn Development
=================

Directory Structure
-------------------

### pecan/

* modules/  Contains the modules that make up PEcAn
* web/		The PEcAn web app to start a run and view outputs.
* models/		Code to create the model specific configurations.
* documentation/	Documentation about what PEcAn is and how to use it.

### Modules (R packages)

* General
** all
** utils
** db
* Analysis
** modules/meta.analysis 
** modules/uncertainty"
** modules/data.land 
** modules/data.atmosphere
** modules/assim.batch 
** modules/assim.sequential 
** modules/priors
* Model Interfaces
** models/ed 
** models/sipnet 
** models/biocro


#### List of modules 

Installing PEcAn
----------------

### Virtual Machine

* Fastest way to get started
* see PEcAn demo ...

### Installing from source

#### From GitHub

```
library(devtools)
install_github("pecan", "PEcAnProject")
```

#### "Makefile"

```
./scripts/build.sh -install # installs all R packages
./scripts/build.sh -h       # list other options

```