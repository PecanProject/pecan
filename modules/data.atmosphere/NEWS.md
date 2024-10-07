# PEcAn.data.atmosphere 1.8.0.9000

## Fixed

* `download.AmerifluxLBL` no longer wrongly re-fetches raw zipfiles when `overwrite = FALSE`

# PEcAn.data.atmosphere 1.8.0

## Fixed

* Extensive code cleanup and fixes to function documentation [@moki1202, ; @meetagrawal, #3212; @aariq, #3055; @Its-Maniaco, #2954]
* Retired packages `maptools` and `rgdal` are no longer imported [@istfer, #3228]
* Unit conversion now uses `PEcAn.utils::ud_convert` to avoid relying on orphaned package `udunits2` [@nanu1695, #2989]
* `download.MERRA()` now respects `overwrite = FALSE` [@meetagrawal09, #3169]
* `download.MERRA()` can now find data from September of 2020 [@KristinaRiemer, #2888]
* `download.Fluxnet2015()` now gives a more useful message when passed a malformed URL [@meetagrawal, #3130]
* `cfmet.downscale.hourly` now produces the correct number of rows for timesteps other than 1 hour (#3270). Note that at extreme timesteps the downscaling algorithm may or may not give _sensible_ values; this patch just ensures it gives the number of values you asked for.

## Added

* Newly supported met data sources:
	- ERA5 via `ERA5_met_process()` [@DongchenZ, #2886]
	- NOAA GEFS data hosted by the Ecological Forecasting Initiative, via `download_NOAA_GEFS_EFI()` [@helge22a, #3174] (For GEFS data directly from NOAA, continue to use `download.NOAA_GEFS`)
* Integration tests of met download from CRUNCEP (#3203), ERA5 (#3207), and Ameriflux (#3208) [@meetagrawal09]

## Changed

* Download of Ameriflux data now uses the `amerifluxr` package, replacing old custom download code [@HenriKajasilta, #2907]
* `metgapfill()` can now handle partial years [@istfer, #2878]
* Downscaling and `load.cfmet` now return plain data frames rather than `data.table`s. Package `data.table` is no longer a dependency.

## Removed

*  Helper function `robustly` has moved to package PEcAn.utils [@meetagrawal, #3096]
*  Helper function `db.site.lat.lon` has been removed. Users should use `PEcAn.DB::query.site(id, con)[c("lat", "lon")]` instead [@Sweetdevil144, #3308]


# PEcAn.data.atmosphere 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see
https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
