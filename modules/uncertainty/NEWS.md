# PEcAn.uncertainty 1.9.0.9000

* run.ensemble.analysis() now respects `settings$modeloutdir` rather than assuming an `out/` folder inside `settings$outdir` (@Akash-paluvai, #3722).



# PEcAn.uncertainty 1.9.0

## License change

PEcAn.uncertainty is now distributed under the BSD 3-clause license instead of the NCSA Open Source license.


## Added

* Added the shared `input_design` matrix, generated via new funtion `generate_joint_ensemble_design()`, keeps parameter draws and sampled inputs aligned across `run.write.configs()`, `write.ensemble.configs()`, and `write.sa.configs()`. (@blesson-07, #3535, #3677).
* New function `compute_sobol_indices` for use in global sensitivity analyses (@blesson-07, #3612).


## Changed

* Breaking: `write.ensemble.configs` gains new mandatory arguments `input_design` and `ensemble.size`, and removes argument `samples`. Note that `input_design` and `ensemble.size` are added to the _beginning_ of the argument list, so calls passing `defaults` unnamed as the first arg will break (#3612, #3634).
* Breaking: `input.ens.gen` gains new mandatory argument `ensemble_size`, added between the existing `settings` and `input` args.
* Plotting sensitivity now makes less noise in the console and once again produces a one-page PDF as intended (#3560).
* `write.ensemble.configs` and `write.sa.configs` now generate an ensemble id if one is not provided in a DB-free run (#3654). Runs with DB continue to always generate a new id. Note that multi-site runs with no id provided will now get a separate ensemble ID (and thus generate separate analyses) for each site.
* Documented that `runModule.run.sensitivity.analysis` does not yet work with multisite settings. This will be fixed in a future release.
* The default sampling method of `get.ensemble.samples` has changed from "uniform" to "random" (#3535).



# PEcAn.uncertainty 1.8.1

* `write.ensemble.configs` now respects argument `write.to.db` when settings$database$bety$write is NULL
	(but if settings$database$bety$write is set, it still overrides `write.to.db`).
* Roxygen fixes



# PEcAn.uncertainty 1.8.0

- Added an optional `pfts` argument to `run.sensitivity.analysis()` so that sensitivity analysis and variance decomposition can be run on a subset of PFTs defined in `settings` if desired (#3155).



# PEcAn.uncertainty 1.7.2

* Added a `NEWS.md` file to track changes to the package.
