## PEcAn variable names

See https://github.com/PecanProject/pecan/wiki/Adding-an-Input-Converter#met-data

General Note: dates in the database should be datatime (preferably with timezone), and datetime passed around in PEcAn should be of type POSIXlt.

# Met Data

The standard met data inputs should be of the form:

Converters from a raw to standard format go in `/modules/data.atmosphere/R`; converters from standard to model-specific go in `models/<mymodel>/R`.

* For a number of common gridded products (NARR, CRUNCEP, ISIMIP), there are bash scripts for converting and rechunking in this repository: https://github.com/ebimodeling/model-drivers; these are not generalized, but may be merged into PEcAn at some point.

Examples:
* NARR:
* CRUNCEP:
* ISIMIP: 

Names should be `met2CF.<sourcename>` and `met2model.<modelname>`.

## Dimensions:

|CF standard-name | units |
|:------------------------------------------|:------|
| time | days since 1700-01-01 00:00:00 UTC|
| longitude | degrees_east|
| latitude |degrees_north|

## The variable names should be `standard_name`

| CF standard-name                          | units | bety         | isimip       | cruncep | narr  | ameriflux |
|:------------------------------------------|:------|:-------------|:-------------|:--------|:------|:----------|
| **air_temperature**                       | K     | airT         | tasAdjust    | tair    | air   | TA (C)    |
| air_temperature_max                       | K     |              | tasmaxAdjust | NA      | tmax  |           |
| air_temperature_min                       | K     |              | tasminAdjust | NA      | tmin  |           |
| **air_pressure**                          | Pa    | air_pressure |              |         |       | PRESS (KPa) |
| mole_fraction_of_carbon_dioxide_in_air    | mol/mol |            |              |         |       | CO2       |
| moisture_content_of_soil_layer            | kg m-2 |             |              |         |       |           |
| soil_temperature                          | K     | soilT        |              |         |       | TS1 *(NOT DONE)* |
| relative_humidity                         | % | relative_humidity | rhurs       | NA      | rhum  | RH        |
| **specific_humidity**                     | 1 | specific_humidity | NA          | qair    | shum  | CALC(RH)  |
| water_vapor_saturation_deficit            | Pa    | VPD          |              |         |       | VPD *(NOT DONE)*     |
| **surface_downwelling_longwave_flux_in_air** | W m-2 | same      | rldsAdjust   | lwdown  | dlwrf | Rgl       |
| **surface_downwelling_shortwave_flux_in_air**| W m-2 |solar_radiation|rsdsAdjust| swdown  | dswrf | Rg        |
| surface_downwelling_photosynthetic_photon_flux_in_air | mol m-2 s-1 | PAR |     |         |       | PAR *(NOT DONE)*          |
| **precipitation_flux**                    |  kg m-2 s-1 | cccc   | prAdjust     | rain    | acpc  | PREC (mm/s)          |
|                                           | degrees | wind_direction |          |         |       | WD        |
| wind_speed                                | m/s   | Wspd         |              |         |       | WS        |
| **eastward_wind**                         | m/s   | eastward_wind |             |         |       | CALC(WS+WD) |
| **northward_wind**                        | m/s   | northward_wind |            |         |       | CALC(WS+WD) |

* preferred variables indicated in bold
* wind_direction has no CF equivalent and should not be converted, instead the met2CF functions should convert wind_direction and wind_speed to eastward_wind and northward_wind
* variable names are from [MsTMIP](http://nacp.ornl.gov/MsTMIP_variables.shtml), but lowercase to be consistent with the MsTMIP drivers.
* standard_name is CF-convention standard names
* units can be converted by udunits, so these can vary (e.g. the time denominator may change with time frequency of inputs)
* soil moisture for the full column, rather than a layer, is soil_moisture_content

For the standardized files, we are using CF standard names as variable names.

For example, in the [MsTMIP-CRUNCEP](https://www.betydb.org/inputs/280) data, the variable `rain` should be `precipitation_rate`.
We want to standardize the units as well as part of the `met2CF.<product>` step. I believe we want to use the CF "canonical" units but retain the MsTMIP units any time CF is ambiguous about the units.

The key is to process each type of met data (site, reanalysis, forecast, climate scenario, etc) to the exact same standard. This way every operation after that (extract, gap fill, downscale, convert to a model, etc) will always have the exact same inputs. This will make everything else much simpler to code and allow us to avoid a lot of unnecessary data checking, tests, etc being repeated in every downstream function.
