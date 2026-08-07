# Canonical match output directory (LandIQ v4.1.2 gap-filled product assignments).
# Override with MATCHED_DIR.

matched_landiq_dir <- function(path_inventory = NULL) {
  out <- Sys.getenv("MATCHED_DIR", "")
  if (nzchar(trimws(out))) {
    return(normalizePath(out, mustWork = FALSE))
  }
  if (is.null(path_inventory) || !nzchar(trimws(as.character(path_inventory)))) {
    path_inventory <- trimws(Sys.getenv("PRODUCTS_INVENTORY", ""))
    if (!nzchar(path_inventory)) {
      root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
      if (!nzchar(root)) {
        stop("Set MATCHED_DIR, PRODUCTS_INVENTORY, or CCMMF_ROOT.")
      }
      path_inventory <- file.path(root, "products", "inventory")
    }
  }
  file.path(path_inventory, "phenology", "matched_landiq_mslsp_v4.1.2")
}
