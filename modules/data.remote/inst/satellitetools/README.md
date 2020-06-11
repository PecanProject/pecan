# satellitetools

Currently has tools for getting Sentinel-2 data from Google Earth Engine and computing biophysical parameters using SNAP Biophysical processor code(python implemetation).

Warning: the data is currently retrieved with 10m resolution (scale=10), so the 20m resolution bands are resampled.

See gee_example.py for simple example of usage.

Old codes include old version gee. Stored because it had some Landsat-8 processing that is not yet implemented in new gee.py.

TODO: Add option for specifying the request spatial resolution.

Dependencies (non-standard packages, might still miss something...):
geopandas
ee
pandas
numpy
xarray
scipy
