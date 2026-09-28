# Load tillage/extract libraries. Called by load_ndti_extract() in pkg_root.R.

.ndti_root <- ndti_extract_pkg_root()
Sys.setenv(TILLAGE_ROOT = dirname(.ndti_root))
.ndti_lib <- file.path(.ndti_root, "scripts", "R")

source(file.path(.ndti_lib, "paths.R"))

.hls_shared <- hls_shared_lib_dir()
if (!dir.exists(.hls_shared)) {
  stop("HLS shared library not found: ", .hls_shared,
       " (set HLS_SHARED_LIB or CCMMF_ROOT)")
}

source(file.path(.hls_shared, "extract_summary_core.R"))
source(file.path(.hls_shared, "tilewise_core.R"))
source(file.path(.hls_shared, "parcel_tilemap.R"))
source(file.path(.ndti_lib, "ndti_combine.R"))
source(file.path(.ndti_lib, "tilewise_ndti_implementation.R"))
source(file.path(.ndti_lib, "ndti_run.R"))
