#!/bin/bash

HDF5_VERSION=1.10.1
HDF5_RELEASE_URL="https://support.hdfgroup.org/ftp/HDF5/releases"

wget "$HDF5_RELEASE_URL/hdf5-${HDF5_VERSION%.*}/hdf5-$HDF5_VERSION/src/hdf5-$HDF5_VERSION.tar.gz"
tar -xzf "hdf5-$HDF5_VERSION.tar.gz"
pushd "hdf5-$HDF5_VERSION"
./configure
make install
popd
