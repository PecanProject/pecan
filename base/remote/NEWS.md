# PEcAn.remote 1.9.1.9000

* `test_remote()` no longer fails with a cryptic `'length = 2' in coercion to 'logical(1)'` error when the remote command produces more than one line of output (for example an SSH warning or login banner alongside the echoed test string). It now reports success if the test string appears anywhere in the output, and otherwise gives the documented error or `FALSE` result, including all the output (#4168).


# PEcAn.remote 1.9.1

* Added keywords and bug reporting URL to DESCRIPTION. No code changes in this release.



# PEcAn.remote 1.9.0

* PEcAn.remote is now distributed under the BSD 3-clause license instead of the NCSA Open Source license.

## Removed

* `start.model.runs` and `runModule.start.model.runs`, defunct since they were
  moved to `PEcAn.workflow` in 2021, have been deleted.


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
