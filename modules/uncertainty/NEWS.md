# PEcAn.uncertainty 1.8.1

* `write.ensemble.configs` now respects argument `write.to.db` when settings$database$bety$write is NULL
	(but if settings$database$bety$write is set, it still overrides `write.to.db`).
* Roxygen fixes

# PEcAn.uncertainty 1.8.0

- Added an optional `pfts` argument to `run.sensitivity.analysis()` so that sensitivity analysis and variance decomposition can be run on a subset of PFTs defined in `settings` if desired (#3155).

# PEcAn.uncertainty 1.7.2

* Added a `NEWS.md` file to track changes to the package.
