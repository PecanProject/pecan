# PEcAn.data.atmosphere 1.7.2.9000

## Fixed

* Extensive code cleanup and fixes to function documentation [@moki1202, ; @meetagrawal, #3212; @aariq, #3055; @Its-Maniaco, #2954]
* Retired packages `maptools` and `rgdal` are no longer imported [@istfer, #3228]
* Unit conversion now uses `PEcAn.utils::ud_convert` to avoid relying on orphaned package `udunits2` [@nanu1695, #2989]
* `download.MERRA()` now respects `overwrite = FALSE` [@meetagrawal09, #3169]
* `download.MERRA()` can now find data from September of 2020 [@KristinaRiemer, #2888]
* `download.Fluxnet2015()` now gives a more useful message when passed a malformed URL [@meetagrawal, #3130]

## Added

* Newly supported met data sources:
	- ERA5 via `ERA5_met_process()` [@DongchenZ, #2886]
	- NOAA GEFS data hosted by the Ecological Forecasting Initiative, via `download_NOAA_GEFS_EFI()` [@helge22a, #3174] (For GEFS data directly from NOAA, continue to use `download.NOAA_GEFS`)
* Integration tests of met download from CRUNCEP (#3203), ERA5 (#3207), and Ameriflux (#3208) [@meetagrawal09]

## Changed

* Download of Ameriflux data now uses the `amerifluxr` package, replacing old custom download code [@HenriKajasilta, #2907]
* `metgapfill()` can now handle partial years [@istfer, #2878]

## Removed

*  Helper function `robustly` has moved to package PEcAn.utils [@meetagrawal, #3096]


# PEcAn.data.atmosphere 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see
https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
