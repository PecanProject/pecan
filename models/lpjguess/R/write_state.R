#' Extract nested value from a state list using flat key
#'
#' @param state A nested list (usually the model.state$state)
#' @param key A flat string like "Gridcell/Stand/1/Patch/1/Vegetation/Individuals/3/cmass_leaf"
#' @return The value stored at that nested position
#' @keywords internal
#' @author Yinghao Sun
extract_from_state_by_key <- function(state, key) {
  # Optional: remove "Gridcell/" prefix
  key <- sub("^Gridcell/", "", key)
  
  parts <- strsplit(key, "/")[[1]]
  val <- state
  
  for (p in parts) {
    if (is.null(val)) {
      warning("NULL reached prematurely at: ", p)
      return(NULL)
    }
    
    # Case 1: numeric index
    if (grepl("^[0-9]+$", p)) {
      idx <- as.integer(p)
      if (idx > length(val)) {
        warning("Index out of bounds: ", idx)
        return(NULL)
      }
      val <- val[[idx]]
      
      # Case 2: named element (case-insensitive match)
    } else {
      val_names <- names(val)
      match_idx <- which(tolower(val_names) == tolower(p))
      
      if (length(match_idx) == 0) {
        warning("Name not found (case-insensitive): ", p)
        return(NULL)
      }
      
      val <- val[[match_idx[1]]]  # use first match
    }
  }
  
  return(val)
}


#' Write updated variables into a copy of the original LPJ-GUESS .state file
#'
#' @param State_updated    A list containing updated state variables, position list and size list (get from read_binary)
#' @param outdir           Path to a directory containing the `0.state` and `meta.bin` files.
#' 
#' @return No return value. Writes files to disk as side effect.
#' @author Yinghao Sun
#' @export
write_binary_LPJGUESS <- function(State_updated, outdir) {
  
  # Build full paths to source files
  src_state <- file.path(outdir, "0.state")
  meta_file <- file.path(outdir, "meta.bin")
  
  # back-up
  bak_state <- file.path(outdir, "bak.state")
  file.copy(src_state, bak_state, overwrite = TRUE)
  
  # a copy to the temporary file
  new_state <- file.path(outdir, "new.state")
  file.copy(src_state, new_state, overwrite = TRUE)
  
  # # Ensure output directory exists
  # dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  # 
  # # Copy template files to output directory so we don't overwrite it
  # file.copy(c(meta_file, original_state), to = output_dir, overwrite = TRUE)
  # 
  # # Open copied 0.state file for binary modification
  # state_path <- file.path(outdir, "0.state")
  # con <- file(state_path, open = "r+b")
  
  # Open temporary new.state file for binary modification
  con <- file(new_state, open = "r+b")
  
  # A named list of byte positions for each variable (generated during reading)
  pos_list <- State_updated$pos_list
  # A named list of writeBin sizes for each variable (same keys as pos_list)
  siz_list <- State_updated$siz_list
  
  # Loop over all keys
  for (key in names(pos_list)) {
    value <- extract_from_state_by_key(State_updated$state, key)
    pos <- pos_list[[key]]
    size <- siz_list[[key]]
    
    # Seek and write
    seek(con, where = pos, origin = "start")
    writeBin(object = value, con = con, size = size)
  }
  
  close(con)
  
  # Atomic substitution
  file.rename(new_state, src_state)   # After success, bak is still there and can be manually deleted
}
