
The rhdf5 package will contain a complete installation of the HDF5 C library.
No further downloads from HDF5 are necessary. 

###############################
Only for bioconductor-administrator:

To prepare the R-package rhdf5, call the bash script download.sh from the base
directory rhdf5.

download.sh will download all necessary files from www.hdfgroup.org/HDF5.
configure scripts and Makefiles will be updated as needed for the HDF5 package using
the R-script replacestr.R. 
The HDF5 source package will be light-weighted.








download from 




sed 's/-install_name \$rpath\$soname/\$soname/g' hdf5-1.8.7/configure > tmp.txt


change

in file configure.in:
AC_CONFIG_FILES

in file configure:
ac_config_files

in file configure:
change all appearances of
"-install_name \$rpath\$soname"
to
"-install_name \$soname"

in file configure:
change
  library_names_spec='${libname}${release}${major}$shared_ext ${libname}$shared_ext'
  soname_spec='${libname}${release}${major}$shared_ext'
to
  library_names_spec='${libname}${release}$shared_ext ${libname}$shared_ext'
  soname_spec='${libname}${release}$shared_ext'

in file configure:
change 
  shrext = ".so" for all shrext that contain a ".dylib"

in file Makefile.in:
DIST_SUBDIRS
und SUBDIRS

in file src/Makefile.in and src/Makefile.ac and src/Makefile:
change PACKAGE = hdf5ForBioC
change lib_LTLIBRARIES = libhdf5ForBioC.la
change target libhdf5.la to libhdf5ForBioC.la.



