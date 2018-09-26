Installing JULES
==========================================================================

1) Download JULES source code
https://jules.jchmr.org/software-and-documentation/previous-releases/jules-v4.2

2) Download FCM
```
wget https://github.com/metomi/fcm/archive/2015.05.0.tar.gz
tar -xvzf 2015.05.0.tar.gz
```

3) Configure netCDF
```
cd jules-vn4.2/etc/fcm-make/
vi make.cfg
```
modify the following settings and save
```
# NetCDF settings
$JULES_NETCDF{?} = actual
$JULES_NETCDF_PATH{?} = /usr
```
4) Compile
```
cd ../..
../fcm-2015.05.0/bin/fcm make -f etc/fcm-make/make.cfg --new
```

5) Run test example
```
cd examples/point_loobos
../../build/bin/jules.exe
```

6) Add local executable to BETY database
* Create new File record for the relevant machine with File name = jules.exe and File path = /path/to/jules/build/bin
* Got to Model and search for the version of JULES you are running (currently 4.2)
* Edit Record > View Related Files and search for the File record you just created in order to link the new model executable to the existing Model record
* Go to PEcAn web interface to verify correct JULES version shows up in pull down menu
