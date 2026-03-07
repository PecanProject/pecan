# PEcAn.workflow 1.10.0.9000

## Changed
* Sensitivity analysis and ensemble runs now generate separate input design matrices with appropriate dimensions, fixing dimension mismatch errors in multisite workflows. (#3708)
* Generated runs are now stored in a `runs_manifest.csv` file in the output directory instead of modifying `samples.Rdata` (#3708)
* Removed `tests/Rcheck_reference.log`, which was used to ignore historic check messages that have now been fixed.

# PEcAn.workflow 1.10.0

## Changed

Breaking: Ensemble runs now use shared input samples across all sites instead of independent sampling per site.

* `run.write.configs()` now has two new required args `ensemble.size` and `input_design`, and removes `ens.sample.method` (#3535, #3612).
* `runModule.run.write.configs()` has new arg `input_design`, with default NULL meaning to generate one internally (#3535).
* Removed unused functions `create_execute_test_xml()` and `model_specific_tags()`, which depended on deprecated database and web frontend (@S1DDHEY, #3826).


# PEcAn.workflow 1.9.0

* PEcAn.workflow is now distributed under the BSD 3-clause license instead of the NCSA Open Source license.
* It is now easier to run a workflow without a connection to the PEcAn database by setting `settings$database$bety$write` to FALSE (or undefining it entirely), at the obvious cost that runs set up this way are not recorded in the database (@yinghaoSunn, #3398).
* Improved handling of `modellauncher` in `start_model_runs()`,
  including some support for array runs via settings like `<qsub.extra>-t 1-@NJOBS@</qsub.extra>`.

# PEcAn.workflow 1.8.0

* New functions `start_model_runs` and `runModule_start_model_runs`, both moved
  from package `PEcAn.remote` (where they were `start.model.runs` and
  `runModule.start.model.runs`). They kick off the model runs you set up
  earlier in the workflow and handle the details of local or remote execution.

# PEcAn.workflow 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the
  PEcAn packages; please see
  https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
