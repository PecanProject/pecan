# PEcAn.benchmark 1.7.5.9000

## Fixed

* `metric_PPMC()`: added `use = "pairwise.complete.obs"` to `stats::cor()` so the Pearson correlation is computed on available pairs rather than returning `NA` whenever any observation is missing. This matches the behaviour of `metric_cor()` in the same package.

* Improved clarity of code examples that are not run at check time.


# PEcAn.benchmark 1.7.5

* Added keywords and bug reporting URL to DESCRIPTION.
* No code changes in this release.


# PEcAn.benchmark 1.7.4

* Removed Browndog support
* Documentation fixes

# PEcAnA.benchmark 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.

