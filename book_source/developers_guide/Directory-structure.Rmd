# Directory structure

## Overview of PEcAn repository as of PEcAn 1.2

<!--This needsto be updated to the latest version-->
<!--Should Also include a guideline to structurs so that people know where to put things as they add them-->
```
trunk/             # Overall repository
 +- all            # Dummy Package to load all PEcAn R packages
 +- common         # Core functions
 +- db             # Modules for querying the database
 +- depreciated    # code from PEcAn < 1.1 for reference or merging into new packages
 +- models         # Directory of wrapper scripts for each ecosystem model
    +- ed          # Wrapper scripts for running ED within PEcAn
    +- sipnet      # Wrapper scripts for running SIPNET within PEcAn
    +- ...         # Wrapper scripts for running [MODELX] within PEcAn
 +- modules        # Core modules
    +- allometry
    +- data.atmosphere
    +- data.hydrology
    +- data.land
    +- meta.analysis
    +- priors
    +- rtm
    +- uncertainty
 +- qaqc           # Database and model QAQC scripts
 +- scripts        # R and Shell scripts for use with PEcAn
    +- shell       # Shell scripts, e.g.. install.all.sh, check.all.sh
    +- R           # R scripts e.g. pft.add.spp.R, workflow.R, install.dependencies.R 
    +- fia         # FIA module
 +- utils          # Misc. utility functions
 +- visualization  # Advanced PEcAn visualization module
 +- web            # Main PEcAn website files
 +- documentation  # index_vm.html, references, other misc.
```


## Generic R package structure:

see the R development wiki for more information on writing code and adding data.

```
 +- DESCRIPTION    # short description of the PEcAn library
 +- R/             # location of R source code
 +- man/           # Documentation
 +- tests/         # runall.R script to call all R test scripts
 +- inst/          # location of install scripts and misc.
    +- tests/      # location of PEcAn testing scripts
    +- extdata/    # misc. data files (in misc. formats)
 +- data/          # data used in testing and examples (saved as *.RData or *.rda files)
```
