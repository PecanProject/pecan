# Combining NCEP files

## _New method_ 

[concatenate inputs across variables and years `concatenate_ncep.sh`](https://github.com/PecanProject/pecan/blob/master/modules/data.atmosphere/inst/scripts/ncep/concatenate_ncep.sh)


## _Old method_ Extract global NCEP data to one file per grid point

This method is inefficient and does not scale. New approach is to take advantage of netcdf concatenation and parallel IO. Retained for posterity.

NOTE: This "works" but at the global scale creates a lot of files an I have been working on just creating a single netCDF file and extracting data directly. But this is useful for smaller regions.


### extract ncep data to csv:

`Globalmet.R` takes global yearly met data and output one file (1948-2012) per lat x lon grid point.

The grid is 1.9 degree

To submit each gridpoint as a separate job:

```bash
./qsubncep.sh
```


### convert ncep csv daily to hourly inputs:

`met2csv.R` converts daily to hourly, uwind and vwind to wind, and converts some units. 
To submit each gridpoint as a separate job:

```bash
./qmet2csv.sh
```
