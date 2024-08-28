# PEcAn.remote 1.8.0.9000

* PEcAn.remote is now distributed under the BSD 3-clause license instead of the NCSA Open Source license.


# PEcAn.remote 1.8.0

## Added

* `start_rabbitmq` is now exported.

## Fixed

* Fixed circular dependency on `PEcAn.DB` by moving functions to other packages
  (see details for individual functions below)

## Removed

* `remote.copy.update`, which was used in only one place and contained typos
  that guaranteed it could never have worked anywhere else.
* `start.model.runs` and `runModule.start.model.runs` have moved to package
  `PEcAn.workflow`, now named `start_model_runs` and
  `runModule_start_model_runs`. The original versions in PEcAn.remote have been
  retained, for now, as stubs that tell you where to find the new versions.
* `stamp_started` and `stamp_finished` have been moved to package `PEcAn.DB`.

# PEcAn.remote 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of
  the PEcAn packages; please see
  https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
