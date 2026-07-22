# Load the shared HLS tilewise framework. Called by load_hls() in pkg_root.R.
#
# Product implementations (MSLSP, NDTI) live in mslsp-extract/ and ndti-extract/;
# those packages source this framework via bootstrap.R.

.hls_pkg_root <- hls_pkg_root()
Sys.setenv(HLS_ROOT = .hls_pkg_root)
.hls_lib <- file.path(.hls_pkg_root, "_lib")

source(file.path(.hls_lib, "extract_summary_core.R"))
source(file.path(.hls_lib, "tilewise_core.R"))
source(file.path(.hls_lib, "parcel_tilemap.R"))
