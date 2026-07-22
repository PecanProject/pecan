# Load mslsp-extract libraries. Called by load_mslsp_extract() in pkg_root.R.

.mslsp_root <- mslsp_extract_pkg_root()
Sys.setenv(MSLSP_EXTRACT_ROOT = .mslsp_root)
.mslsp_lib <- file.path(.mslsp_root, "scripts", "_lib")

source(file.path(.mslsp_lib, "paths.R"))

.hls_shared <- hls_shared_lib_dir()
if (!dir.exists(.hls_shared)) {
  stop("HLS shared library not found: ", .hls_shared,
       " (set HLS_SHARED_LIB or CCMMF_ROOT)")
}

source(file.path(.hls_shared, "extract_summary_core.R"))
source(file.path(.hls_shared, "tilewise_core.R"))
source(file.path(.hls_shared, "parcel_tilemap.R"))
source(file.path(.mslsp_lib, "tilewise_mslsp_implementation.R"))
source(file.path(.mslsp_lib, "mslsp_combine.R"))
source(file.path(.mslsp_lib, "mslsp_run.R"))
