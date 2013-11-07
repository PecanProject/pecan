# Extract global NCEP data to one file per grid point



## extract ncep data to csv:

`Globalmet.R` takes global yearly met data and output one file (1948-2012) per lat x lon grid point.

The grid is 1.9 degree

To submit each gridpoint as a separate job:

```bash
./qsubncep.sh
```

Outputs:

* Specific Humidity (provides daily mean, min, max)
** shum:long_name = "mean Daily Specific Humidity at 2 m" ;
** shum:units = "kg/kg" ;
* Relative Humidity
** rhum:long_name = "mean Daily relative humidity at sigma level 995" ;
** rhum:units = "%" ;
* Precipitation
** prate:long_name = "mean Daily Precipitation Rate at surface" ;
** prate:units = "Kg/m^2/s" ;
* Wind
** uwnd:long_name = "mean Daily u-wind at 10 m" ;
** uwnd:units = "m/s" ;
** vwnd:long_name = "mean Daily v-wind at 10 m" ;
** vwnd:units = "m/s" ;
** wind = sqrt(vwnd^2 + uwnd^2)
* Temperature
** air:long_name = "mean Daily Air temperature at 2 m" ;
** air:units = "degK" ;
* Solar Radiation
** dswrf:long_name = "mean Daily Downward Solar Radiation Flux at surface" ;
** dswrf:units = "W/m^2" ;


## convert ncep csv daily to hourly inputs:

`met2csv.R` converts daily to hourly, uwind and vwind to wind, and converts some units. 
To submit each gridpoint as a separate job:

```bash
./qmet2csv.sh
```
