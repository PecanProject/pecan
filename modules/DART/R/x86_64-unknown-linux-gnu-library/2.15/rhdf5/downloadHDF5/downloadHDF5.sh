#!/bin/bash

# The rhdf5 package will contain a complete installation of the HDF5 C library.
# No further downloads from HDF5 are necessary. 
# This file contains code for bioconductor administrators.

if [ "$1" = "" ]; then
   echo "Please give version number as parameter"
   exit 0
fi

VERSION=$1
WEBDIR=http://www.hdfgroup.org/ftp/HDF5/prev-releases/hdf5-${VERSION}
# WEBDIR=http://www.hdfgroup.org/ftp/HDF5/current
echo "download HDF5-$VERSION from $WEBDIR"
echo "#############"
echo "## download source for linux and mac"
echo "#############"
mkdir source
rm -f src/hdf5small.tgz
cd source
wget ${WEBDIR}/src/hdf5-${VERSION}.tar.gz
tar -xzf hdf5-${VERSION}.tar.gz
cd hdf5-${VERSION}
SCRIPT="source(\"../../inst/downloadHDF5/replacestr.R\")"
echo ${SCRIPT}
echo ${SCRIPT} | R --arch=x86_64 --slave --vanilla
rm -rf c++
rm -rf examples
rm -rf fortran
rm -rf hl
rm -rf perform
rm -rf release_docs
rm -rf test
rm -rf testpar
rm -rf tools
rm -rf vms
rm -rf windows
cd ..
mv hdf5-${VERSION} hdf5
tar -czf hdf5small.tgz hdf5
cd ..
mv source/hdf5small.tgz src/
echo "#############"
echo "## download precompiled win32 libraries"
echo "#############"
mkdir win32
cd win32
wget ${WEBDIR}/bin/windows/HDF5-${VERSION}_win_x86.zip
unzip -q HDF5-${VERSION}_win_x86.zip
cd ..
rm -rf src/libwin
mkdir src/libwin
mkdir src/libwin/i386
cp win32/dll/hdf5dll.dll src/libwin/i386
cp win32/dll/szip.dll src/libwin/i386
cp win32/dll/zlib1.dll src/libwin/i386
echo "#############"
echo "## download precompiled win64 libraries"
echo "#############"
mkdir win64
cd win64
wget ${WEBDIR}/bin/windows/HDF5-${VERSION}_win_x64.zip
unzip -q HDF5-${VERSION}_win_x64.zip
cd ..
mkdir src/libwin/x64
cp win64/dll/hdf5dll.dll src/libwin/x64/hdf5dll-64.dll
cp win64/dll/szip.dll src/libwin/x64/szip-64.dll
cp win64/dll/zlib1.dll src/libwin/x64/zlib1-64.dll
echo "#############"
echo "## download static cygwin libraries to obtain windows header files"
echo "#############"
mkdir static
cd static
wget ${WEBDIR}/bin/windows/hdf5-${VERSION}_cygwin_x86_static.zip
unzip -q hdf5-${VERSION}_cygwin_x86_static.zip
sed "s/#define H5_HAVE_FEATURES_H 1/\/\*\ #define H5_HAVE_FEATURES_H 1\*\//g" include/H5pubconf.h > tmp.txt
mv -f tmp.txt include/H5pubconf.h
cd ..
rm -rf src/includeWin
mkdir src/includeWin
mv static/include/* src/includeWin

# cleanup
rm -rf win32
rm -rf win64
rm -rf static
rm -rf source

