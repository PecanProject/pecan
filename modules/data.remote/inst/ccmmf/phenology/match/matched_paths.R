# Canonical match output directory (LandIQ v4.1.2 gap-filled product assignments).
# Override with CCMMF_MATCHED_DIR.

matched_landiq_dir <- function(path_management = Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")) {
  out <- Sys.getenv("CCMMF_MATCHED_DIR", "")
  if (nzchar(trimws(out))) {
    return(normalizePath(out, mustWork = FALSE))
  }
  file.path(path_management, "phenology", "matched_landiq_mslsp_v4.1.2")
}
