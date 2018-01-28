#!/usr/bin/env Rscript

# This script is used to install the `hdf5r` package on Travis, using custom 
# HDF5 binaries located in the `hdf5_bin_trusty` directory.

hdf5_dir <- list.files(".", "hdf5_bin")

if (length(hdf5_dir) > 1) {
  stop("Multiple HDF5 directories found")
} else if (length(hdf5_dir) < 1) {
  stop("No HDF5 directories found")
}

hdf5_full_dir <- normalizePath(hdf5_dir)

Sys.setenv("HDF5_DIR" = hdf5_full_dir)

hdf_full_path <- file.path(hdf5_full_dir, "bin", "h5cc")
hdf_config_arg <- paste0("--with-hdf5=", hdf_full_path)
install.packages("hdf5r", lib = "test_library", configure.args = hdf_config_arg)
