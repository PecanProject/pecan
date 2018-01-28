#!/usr/bin/env Rscript

# This script is used to install the `hdf5r` package on Travis, using custom 
# HDF5 binaries located in the `hdf5_bin_trusty` directory.

install.packages(
  "hdf5r",
  configure.args = c(
    "--hdf5-libraries=hdf5_bin_trusty/lib",
    "--hdf5-includes=hdf5_bin_trusty/include"
  )
)
