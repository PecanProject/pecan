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
../../../fcm-2015.05.0/bin/fcm make -f make.cfg --new
```

5) Run test example
```
cd ../../examples/point_loobos
../../build/bin/jules.exe
```