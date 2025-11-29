# PEcAn.uncertainty 1.8.2

* PEcAn.uncertainty is now distributed under the BSD 3-clause license instead of the NCSA Open Source license.
* Plotting sensitivity now makes less noise in the console and once again
    produces a one-page PDF as intended.
* `write.ensemble.configs` and `write.sa.configs` now generate an ensemble id
    if one is not provided in a DB-free run.
    Runs with DB continue to always generate a new id.
    Note that multi-site runs with no id provided will now get a separate
    ensemble ID (and thus generate separate analyses) for each site.
* Documented that `runModule.run.sensitivity.analysis` does not yet work with
    multisite settings.
    This will be fixed in a future release.


# PEcAn.uncertainty 1.8.1

* `write.ensemble.configs` now respects argument `write.to.db` when settings$database$bety$write is NULL
	(but if settings$database$bety$write is set, it still overrides `write.to.db`).
* Roxygen fixes


# PEcAn.uncertainty 1.8.0

- Added an optional `pfts` argument to `run.sensitivity.analysis()` so that sensitivity analysis and variance decomposition can be run on a subset of PFTs defined in `settings` if desired (#3155).


# PEcAn.uncertainty 1.7.2

* Added a `NEWS.md` file to track changes to the package.
