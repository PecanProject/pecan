# PEcAn.settings 1.9.1

## Fixed

* listToXml.MultiSettings now produces valid XML from a MultiSettings with length 1 (#3546) .
* setEnsemblePaths no longer wraps single paths in a list, giving `<path>/your/file.here</path>` instead of the previous `<path><path1>/your/file.here</path1></path>` (#3561).

## Changed

* The `tag` argument to `listToXml()` is now optional and will default to "pecan" if not specified. This sets the name of the root tag of the resulting XML object (#3558).
* The first argument of `listToXml.MultiSettings()` has been renamed from `item` to `x`, and it now accepts but ignores `...`, both for consistency with the generic (#3558).
* `setEnsemblePaths()` no longer restricts the set of values accepted for `input_type` (#3633)


# PEcAn.settings 1.9.0

## Changed

* PEcAn.settings is now distributed under the BSD 3-clause license instead of the NCSA Open Source license.
* `createMultiSiteSettings` now names each site's block of `settings$run` as `site.[siteid]` for easier subsetting (was `settings.[n]`).
* `write.settings` will now return the formatted XML as a string (and not write to any file) when called with `outputfile = NULL`, for consistency with `XML::saveXML` and for convenience when debugging.

## Added

* `createMultiSiteSettings` argument `siteIds` now accepts data frames as well as the previously accepted numeric or character vectors. The data frame should have one site per row, uniquely identified by a mandatory `id` column. All columns of each row will become fields of the resulting `settings$run$site` block.
* New function `setEnsemblePaths` inserts paths to your ensemble inputs (met, poolinitcond, etc) into every site's `inputs` block according to the filename pattern specified in a template string.

## Fixed

* Reading a multi-settings from XML now keeps the site bock names specified in the file instead of defaulting to `settings.[n]`.

## Removed

* Internal helper function `getRunSettings` is no longer exported. As the documentation has long noted, it was not intended to be called directly.



# PEcAn.settings 1.8.0

* Bug fixes for ensemble runs.



# PEcAn.settings 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the
  PEcAn packages; please see
  https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
