# Processing Cru-ncep data from MsTMIP / NACP repository at ORNL

There are a number of different files in here, variously trying to do the following, but on different servers (including clusters that use modules and queuing systems). But the key steps are here:

## downloading from ornl

```
nohup ionice -n 7 wget -r -nH --cut-dirs=6 ftp://nacp.ornl.gov/synthesis/2009/frescati/model_driver/cru_ncep/analysis &
```

## transfer from pecandev to ebi-cluster

```
ionice -c2 -n7 rsync --rsync-path="ionice -c2 -n7 rsync" -routi --progress ./* ebi-cluster:met/cruncep/
```

## unzip

_note_ these are i/o and memory-intensive (need at least 16GB RAM; works better with 64!)

* [`gunzipall.sh`](https://github.com/PecanProject/pecan/blob/master/modules/data.atmosphere/inst/scripts/cruncep/gunzipall.sh) unzips files nicely

## CRUNCEP - Rechunking the files

1) Concatenation of variable  files along the time dimension

For example the following command uses the NCO operator  ncrcat to concentate the yearly files for qair ( air specific humditiy) for the years 1901-2010 . The output file ncrcat_qair.nc is in netcdf-4 format.

    ncrcat --no_tmp_fl -4 -O -h -n 110,4,1 cruncep_qair_1901.nc  ncrcat_qair.nc

This operation is more dependent  on good disk I/O than processor speed

2) Rechunk dimensions and convert the  time dimension from unlimited to fixed.

Access to file created in 1.  is very very slow because the  dimension time is unlimited  so netcdf-4 reads  a single  record at a time. The nccopy utility in the netcdf-4 package is used to rechunk and convert the time dim to limited.

    nccopy -k 3 -u -c time/2000,lat/1,lon/720 ncrcat_qair.nc qairf.nc

This command may take 3-4 hours per variable. Typical input dims:
Time=160708   lat=360, lon=720  


3) Permute the dimensions from (time,lat,lon) to (lat,lon,time).

We wish to speeed up access for a given lat/lon spot  - by permuting the dims from (time,lat,lon) to (lat,lon,time)  it means all the variable  values we require are contiguous in file and so access is super quick.

    ncpdq --no_tmp_fl -h -O -a lat,lon,time qairf.nc qair.nc

The memory requirements for this a command are approx 2*var_size.
For example the qiar file (size 157G) required 310G on “blacklight” and took about 4 hours to permute. There is no obvious  way to speed up this step as ncpdq is not threaded. If memory is limited then the alternative is to break the file up along the lat dim – repermute , then reconcatenate along the lat dim.

4) Append together the individual files from
   
This step is to combine files into a single script, for ease of programming the applications that use it (e.g. to have one large rather than many small files). 
It should not affect the access speed.

Example scripts:

1-2) <b>bigStart.sh </b><br>
  this script performs steps i) and 2) <br> 
  It operates on the vars in the directory specified by the env Var $WKDIR.<br>
  e.g<br>
  export WKDIR=/home/groups/ebimodeling/met/cruncep/in/qair<br>
  ./bigStart.sh <br>

3) <b>bigPermute.sh </b> <br>
  this script permforms step 3). <br> 
  It operates on the concatenated file "(ncrcat_${VAR}1.nc" in the directory specified by $WKDIR.And outputs the file ${VAR}.nc.<br>
  e.g<br>
  export WKDIR=/home/groups/ebimodeling/met/cruncep/in/qair<br>
  ./bigPermute.sh<br>


4) <b>bigAppend.sh</b><br> 
  This script permforms step 4). <br>
  It operates on the directory "/home/groups/ebimodeling/met/cruncep/" <br>
  The output file is /home/groups/ebimodeling/met/cruncep/all.nc <br>
  
  


