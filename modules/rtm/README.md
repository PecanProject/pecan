# PEcAn Radiative Transfer Modeling module
**Corresponding author**  
Alexey Shiklomanov  
Dept. of Earth and Environment  
Boston University  
ashiklom@bu.edu  

## Installation
Easiest way to install is via `install_github` from the `devtools` package.

```R
library(devtools)
install_github("ashiklom/pecan", subdir="modules/rtm")
```

If you want a specific branch, do `install_github(..., ref="<branch>")`.

From there, you should be able to load the package in your typical R session.

```R
library(PEcAnRTM)
```

## Basics
In progress...

## TODO
* Finish README!
* Create module for running PROSPECT leaf transmittance and reflectance model for use in meta.analysis and in the ED (and other) model
