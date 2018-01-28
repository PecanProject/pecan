#!/usr/bin/env Rscript

# This script is used to install the `hdf5r` package on Travis, using custom 
# HDF5 binaries located in the `hdf5_bin_trusty` directory.

install.packages(
  "hdf5r",
  configure.args = c(
    "--with-hdf5=hdf5_bin_trusty"
  )
)
