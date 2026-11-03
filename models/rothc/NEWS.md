# PEcAn.RothC 0.0.0.9000

Initial development version

* `write.config.RothC()` now reads the `opt_SMDbare` setting for the bare-soil moisture deficit option. It previously used `opt_RMmoist` for both options, so `opt_SMDbare` was ignored (#4217).