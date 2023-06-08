# PEcAn.DB 1.7.2.9000

## Added

* Shifted `convert.input` function from `PEcAn.utils` to `PEcAn.DB` with a new name `convert_input`to remove circular dependency.
  (#3026; @nanu1605)
* Added a stub function `convert.input`. (#3026; @nanu1605)
* Updated unit conversions throughout PEcAn to use the `units` R package instead of the unmaintained `udunits2`. Note that both `units` and `udunits2` interface with the same underlying compiled code, so the `udunits2` *system library* is still required. (#2989; @nanu1605)
* Fixed a bug in `ud_convert()` where it failed with objects of class "difftime" introduced by refactoring to use the `units` package instead of `udunits` (#3012)