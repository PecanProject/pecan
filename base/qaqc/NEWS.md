# PEcAn.qaqc 1.7.5.9000

## Fixed

* `cull_database_entries()` now deletes only the rows belonging to `machine_id` when one is given. A missing comma in the row subset meant the machine filter had no effect, so rows from other machines were deleted too (#4281).

# PEcAn.qaqc 1.7.5

* Added keywords and bug reporting URL to DESCRIPTION. No code changes in this release.



# PEcAn.qaqc 1.7.4

## License change
* PEcAn.qaqc is now distributed under the BSD three-clause license instead of the NCSA Open Source license.



# PEcAn.qaqc 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see 
https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
