# PEcAn.DB 1.7.2.9000

## Added

* New functions `stamp_started` and `stamp_finished`, used to record the start
  and end time of model runs in the database. Both used to live in
  `PEcAn.remote` and were moved to resolve a circular dependency.
* New function `convert_input`, used to convert between formats while reusing
  existing files where possible. It previously lived in package `PEcAn.utils`,
  but was moved here to simplify dependencies. (#3026; @nanu1605)
* `get.trait.data` gains new argument `write` (with default FALSE), passed on to `get.trait.data.pft` (@Aariq, #3065).

# PEcAn.DB 1.7.2

## Removed

* `rename_jags_columns()` has been removed from `PEcAn.DB` but is now available
  in package `PEcAn.MA` (#2805, @moki1202).


# PEcAn.DB 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of
  the PEcAn packages; please see
  https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
