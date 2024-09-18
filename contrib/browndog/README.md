## Brown Dog Wrapper Scripts

This folder contains wrapper scripts that allow the Brown Dog services (i.e. the DAP and DTS) to call functionality provided with PEcAn.
For more info on how to create convert scripts for the DAP see:

[https://opensource.ncsa.illinois.edu/confluence/display/BD/Adding+Conversions+to+the+DAP+and+Calling+the+DAP](https://opensource.ncsa.illinois.edu/confluence/display/BD/Adding+Conversions+to+the+DAP+and+Calling+the+DAP)

and for the DTS:

*Coming soon!*

------------------------------------------------------------------------------------------------------


To excute PEcAn.R, you need to install PEcAn.data.atmosphere. Please refer to https://pecanproject.github.io/pecan//modules/data.atmosphere/inst/web/index.html
(Use install.packages("devtools") if you don't have this library. 
Use install.packages("udunits2", configure.args='--with-udunits2-lib=/usr/local/lib') if you get "Error: libudunits2.a not found")