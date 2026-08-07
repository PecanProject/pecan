# Canonical match output directory (LandIQ v4.1.2 gap-filled product assignments).
# Override with MATCHED_DIR.

matched_landiq_dir <- function(path_management = NULL) {
  out <- Sys.getenv("MATCHED_DIR", "")
  if (nzchar(trimws(out))) {
    return(normalizePath(out, mustWork = FALSE))
  }
  if (is.null(path_management) || !nzchar(trimws(as.character(path_management)))) {
    path_management <- trimws(Sys.getenv("MANAGEMENT", ""))
    if (!nzchar(path_management)) {
      root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
      if (!nzchar(root)) {
        stop("Set MATCHED_DIR, MANAGEMENT, or CCMMF_ROOT.")
      }
      path_management <- file.path(root, "management")
    }
  }
  file.path(path_management, "phenology", "matched_landiq_mslsp_v4.1.2")
}
