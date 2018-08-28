# The Multi-Assumption Architecture and Testbed (MAAT) modelling system PEcAn module
**PEcAn module corresponding author** <br>
Shawn Serbin <br>
Environmental and Climate Sciences Department <br>
Brookhaven National Laboratory <br>
sserbin@bnl.gov <br>

**MAAT model description and technical article:** https://www.geosci-model-dev.net/11/3159/2018/gmd-11-3159-2018.html <br>
**MAAT model author:** Anthony Walker (walkerap@ornl.gov) <br>

## MAAT Installation and usage
**MAAT model source:** https://github.com/walkeranthonyp/MAAT <br>
Follow the instructions found here: https://github.com/walkeranthonyp/MAAT/blob/master/README.md <br>

## MAAT module installation
R dependencies: PEcAn packages - PEcAn.logger, PEcAn.remote, PEcAn.settings, PEcAn.utils. Additional dependencies: lubridate, ncdf4, udunits2, XML, and those within each package <br>

--ncdf4 requires netCDF installed at the system level 

Easiest way to install is via `install_github` from the `devtools` package. <br>

```R
library(devtools)
install_github("PecanProject/pecan", ref = "develop", subdir="base/logger")
install_github("PecanProject/pecan", ref = "develop", subdir="base/remote")
install_github("PecanProject/pecan", ref = "develop", subdir="base/settings")
install_github("PecanProject/pecan", ref = "develop", subdir="base/utils")
install_github("PecanProject/pecan", ref = "develop", subdir="models/maat")
```

If you want a specific branch, do `install_github(..., ref="<branch>")`.

From there, you should be able to load the package in your typical R session.

## MAAT-PEcAn run examples
See [vignettes](vignettes/)
