# PEcAn.MA 1.7.5.9000

* New function `PEcAn.MA::meta_analysis_standalone` runs meta-analysis without database or file IO (#3728).

## Fixed

* `pecan.ma()`: removed stray `)` inside the default `logfile` path string that caused the path to be literally `meta-analysis.log)` (with a closing parenthesis in the filename).
* `pecan.ma()`: `sink()` now correctly redirects output to the caller-supplied `logfile` argument instead of always hardcoding the path to `meta-analysis.log` inside `outdir`, making the argument actually effective.


# PEcAn.MA 1.7.5

* Added bug reporting URL and keywords to DESCRIPTION.
* Minor internal code cleanup with no user-visible changes.


# PEcAn.MA 1.7.4

## License change
* PEcAn.MA is now distributed under the BSD three-clause license instead of the NCSA Open Source license.



# PEcAn.MA 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
