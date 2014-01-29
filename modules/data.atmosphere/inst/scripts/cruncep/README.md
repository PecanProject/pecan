# Processing Cru-ncep data from MsTMIP

There are a number of different files in here, variously trying to do the following, but on different servers (including clusters that use modules and queuing systems). But the key steps are here:

### downloading from ornl

```
nohup ionice -n 7 wget -r -nH --cut-dirs=6 ftp://nacp.ornl.gov/synthesis/2009/frescati/model_driver/cru_ncep/analysis &
```

### transfer from pecandev to ebi-cluster

```
ionice -c2 -n7 rsync --rsync-path="ionice -c2 -n7 rsync" -routi --progress ./* ebi-cluster:met/cruncep/
```

### unzip

_note_ these are i/o and memory-intensive (need at least 16GB RAM; works better with 64!)

* [`gunzipall.sh`](https://github.com/PecanProject/pecan/blob/master/modules/data.atmosphere/inst/scripts/cruncep/gunzipall.sh) unzips files nicely

### concatenate

* [`weather.sh`](https://github.com/PecanProject/pecan/blob/master/modules/data.atmosphere/inst/scripts/cruncep/weather.sh)

### rechunk

* [`permute_all.sh`](https://github.com/PecanProject/pecan/blob/master/modules/data.atmosphere/inst/scripts/cruncep/permute_all.sh) attempts to chunk for fast reading of time series (`permute.sh` does this for 1975-2010)
