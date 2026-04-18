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





## ------------------------------------------------------------
## Flexible writer that supports changing number_of_individuals
## by rebuilding 0.state in a streaming-replay manner.
##
## Requirements:
## - State_updated is the same structure you already pass:
##     list(state = Gridcell_updated, pos_list = pos_list, siz_list = siz_list)
## - extract_from_state_by_key() is your existing helper in write_state.R
## ------------------------------------------------------------
#' Copy raw bytes between two binary connections
#'
#' Internal helper to copy an exact number of bytes from a source binary
#' connection to a destination binary connection, in chunks.
#'
#' @param src_con    A binary connection opened for reading (e.g., `file("x", "rb")`).
#' @param dst_con    A binary connection opened for writing (e.g., `file("y", "wb")`).
#' @param nbytes     Integer. Number of bytes to copy.
#' @param chunk      Integer. Chunk size in bytes for streaming copy. Default is 1 MiB.
#' 
#' @return Invisibly returns `TRUE`. Called for side effects (writing to `dst_con`).
#' @keywords internal
#' @author Yinghao Sun
#' @noRd
.copy_raw_bytes <- function(src_con, dst_con, nbytes, chunk = 1024^2) {
  if (nbytes <= 0) return(invisible(TRUE))
  while (nbytes > 0) {
    this_n <- min(chunk, nbytes)
    buf <- readBin(src_con, what = "raw", n = this_n)
    if (length(buf) != this_n) stop("Unexpected EOF while copying raw bytes.")
    writeBin(buf, dst_con)
    nbytes <- nbytes - this_n
  }
  invisible(TRUE)
}

#' Test whether a key is the `number_of_individuals` field key
#'
#' Internal helper. Determines whether a given `pos_list` key corresponds to
#' `.../Vegetation/number_of_individuals`.
#'
#' @param key Character scalar. A state key string (e.g., names of `pos_list`).
#'
#' @return Logical scalar. `TRUE` if `key` ends with `/Vegetation/number_of_individuals`, else `FALSE`.
#' @keywords internal
#' @author Yinghao Sun
#' @noRd
.is_nind_key <- function(key) {
  grepl("/Vegetation/number_of_individuals$", key)
}


#' Parse stand and patch indices from a `number_of_individuals` key
#'
#' Internal helper. Extracts numeric stand/patch indices from keys like:
#' `Gridcell/Stand/<s>/Patch/<p>/Vegetation/number_of_individuals`.
#'
#' @param key Character scalar. A state key string.
#'
#' @return A list with elements `stand` (integer) and `patch` (integer),
#'   or `NULL` if the pattern does not match.
#' @keywords internal
#' @author Yinghao Sun
#' @noRd
.parse_stand_patch_from_nind_key <- function(key) {
  # key format:
  # Gridcell/Stand/<s>/Patch/<p>/Vegetation/number_of_individuals
  m <- regexec("^Gridcell/Stand/([0-9]+)/Patch/([0-9]+)/Vegetation/number_of_individuals$", key)
  mm <- regmatches(key, m)[[1]]
  if (length(mm) != 3) return(NULL)
  list(stand = as.integer(mm[2]), patch = as.integer(mm[3]))
}

#' Find an "Individuals/1" schema from pos/siz lists
#'
#' Internal helper. Locates any set of keys belonging to `.../Vegetation/Individuals/1/...`
#' and uses them to infer the per-field binary schema: field suffix order and element size.
#' This schema is then reused to write individuals for any index `i = 1..N`.
#'
#' @param pos_list Named list or vector. Byte offsets for each key in the binary state file.
#'   Usually `State_updated$pos_list` produced by your reader.
#' @param siz_list Named list or vector. Byte size per element for each key.
#'   Usually `State_updated$siz_list` produced by your reader.
#'
#' @return A `data.frame` with columns:
#'   - `suffix`: character, the part after `.../Vegetation/Individuals/1/`
#'   - `size`: numeric, byte size per element for that field
#'   Returns `NULL` if no Individuals/1 keys are found.
#' @keywords internal
#' @author Yinghao Sun
#' @noRd
.find_any_indiv1_schema <- function(pos_list, siz_list) {
  # Find any ".../Vegetation/Individuals/1/..." keys and use them as schema
  keys <- names(pos_list)
  hit <- grepl("/Vegetation/Individuals/1/", keys)
  if (!any(hit)) return(NULL)
  
  keys1 <- keys[hit]
  # pick one stand/patch based on the first match
  k0 <- keys1[1]
  # extract prefix up to Individuals/1/
  prefix <- sub("(.*?/Vegetation/Individuals/1/).*", "\\1", k0)
  in_one <- startsWith(keys, prefix)
  kset <- keys[in_one]
  ord <- order(as.numeric(unlist(pos_list[kset])))
  kset <- kset[ord]
  
  suffix <- sub(prefix, "", kset, fixed = TRUE)
  sizes  <- as.numeric(unlist(siz_list[kset]))
  
  data.frame(
    suffix = suffix,
    size   = sizes,
    stringsAsFactors = FALSE
  )
}

#' Write the Individuals block for a given stand/patch using an inferred schema
#'
#' Internal helper. Writes `Individuals/1..N` sequentially into an already-open
#' destination binary connection, using the schema inferred from an existing
#' `Individuals/1` block (field order + element sizes).
#'
#' @param dst_con A binary connection opened for writing.
#' @param Gridcell The updated state object (your parsed `Gridcell` list).
#' @param stand_i Integer. Stand index (1-based, consistent with your state list).
#' @param patch_i Integer. Patch index (1-based).
#' @param schema A `data.frame` produced by `.find_any_indiv1_schema()` with columns
#'   `suffix` and `size`.
#'
#' @return Invisibly returns `TRUE`. Called for side effects (writing bytes to `dst_con`).
#' @author Yinghao Sun
#' @keywords internal
#' @noRd
.write_individuals_block <- function(dst_con, Gridcell, stand_i, patch_i, schema) {
  # Gridcell$Stand[[stand_i]]$Patch[[patch_i]]$Vegetation$Individuals
  inds <- Gridcell[["Stand"]][[stand_i]][["Patch"]][[patch_i]][["Vegetation"]][["Individuals"]]
  new_n <- length(inds)
  
  # write each individual by replaying the "Individuals/1" schema
  base_prefix <- paste0("Gridcell/Stand/", stand_i,
                        "/Patch/", patch_i,
                        "/Vegetation/Individuals/")
  
  for (ii in seq_len(new_n)) {
    prefix_i <- paste0(base_prefix, ii, "/")
    for (rr in seq_len(nrow(schema))) {
      key_i <- paste0(prefix_i, schema$suffix[rr])
      val <- extract_from_state_by_key(Gridcell, key_i)
      if (is.null(val)) stop("Missing value for key: ", key_i)
      
      # writeBin writes the whole vector; size is per element
      writeBin(val, dst_con, size = schema$size[rr], endian = "little")
    }
  }
  
  invisible(TRUE)
}

#' Write LPJ-GUESS binary state allowing cohort count changes
#'
#' A robust writer for LPJ-GUESS `0.state` that supports changes in the number of
#' cohorts/individuals (i.e., variable-length Individuals blocks). When no cohort
#' count changes are detected, it can fall back to a fast in-place writer
#' (e.g., your existing `write_binary_LPJGUESS()` that uses stored offsets).
#'
#' If cohort count changes are detected, this function rebuilds a new `0.state`
#' by streaming-replay: it copies unchanged raw bytes from the original state,
#' writes updated fields, and fully rewrites the Individuals blocks for affected
#' stand/patches so downstream offsets remain correct.
#'
#' @param State_updated A list containing:
#'   - `state`: updated Gridcell object (parsed state tree)
#'   - `pos_list`: named list/vector of byte offsets for each key
#'   - `siz_list`: named list/vector of byte sizes per element for each key
#' @param outdir Character scalar. Directory containing `0.state` and `meta.bin`.
#' @param use_fast_inplace_if_possible Logical. If `TRUE`, uses the existing
#'   in-place writer when no `number_of_individuals` changes are detected.
#' @param verbose Logical. If `TRUE`, prints minimal progress messages.
#'
#' @return Invisibly returns `TRUE` on success. Side effect:
#'   - Creates `bak.state` as a backup of the original `0.state`
#'   - Writes a new `0.state` (possibly via `0.state.tmp` and rename)
#'
#' @author Yinghao Sun
#' @export
write_binary_LPJGUESS_flexible <- function(State_updated, outdir,
                                           use_fast_inplace_if_possible = TRUE,
                                           verbose = FALSE) {
  Gridcell <- State_updated$state
  pos_list <- State_updated$pos_list
  siz_list <- State_updated$siz_list
  
  meta_file  <- file.path(outdir, "meta.bin")
  state_file <- file.path(outdir, "0.state")
  bak_file   <- file.path(outdir, "bak.state")
  tmp_file   <- file.path(outdir, "0.state.tmp")
  
  if (!file.exists(state_file)) stop("0.state not found: ", state_file)
  if (!file.exists(meta_file))  stop("meta.bin not found: ", meta_file)
  
  ok <- file.copy(state_file, bak_file, overwrite = TRUE)
  if (!ok) stop("Failed to create backup state: ", bak_file)
  
  keys_nind <- names(pos_list)[vapply(names(pos_list), .is_nind_key, logical(1))]
  if (length(keys_nind) == 0) {
    if (verbose) message("No number_of_individuals keys; fallback to in-place writer.")
    return(write_binary_LPJGUESS(State_updated, outdir))
  }
  
  # ---- scan old_n vs new_n (use a dedicated connection name!)
  zz_scan <- file(bak_file, "rb")
  on.exit(try(close(zz_scan), silent = TRUE), add = TRUE)
  
  file_size <- file.info(bak_file)$size
  
  # Precompute sorted key positions once
  all_keys <- names(pos_list)
  all_pos  <- as.numeric(unlist(pos_list[all_keys]))
  ord <- order(all_pos)
  all_keys <- all_keys[ord]
  all_pos  <- all_pos[ord]
  
  # helper: schema for a specific stand/patch (Individuals/1)
  .schema_for_patch <- function(stand_i, patch_i) {
    pref <- paste0("Gridcell/Stand/", stand_i, "/Patch/", patch_i,
                   "/Vegetation/Individuals/1/")
    hit <- startsWith(names(pos_list), pref)
    if (!any(hit)) return(NULL)
    ks <- names(pos_list)[hit]
    ks <- ks[order(as.numeric(unlist(pos_list[ks])))]
    data.frame(
      suffix = sub(pref, "", ks, fixed = TRUE),
      size   = as.numeric(unlist(siz_list[ks])),
      stringsAsFactors = FALSE
    )
  }
  
  # fallback global schema (any Individuals/1 anywhere)
  schema_global <- .find_any_indiv1_schema(pos_list, siz_list)
  if (is.null(schema_global)) {
    stop("Cannot find any Individuals/1 schema in pos_list.")
  }
  
  mods <- list()
  changed <- FALSE
  
  for (k in keys_nind) {
    pos_nind <- as.numeric(pos_list[[k]])
    seek(zz_scan, where = pos_nind, origin = "start")
    old_n <- readBin(zz_scan, integer(), 1, size = 4, endian = "little")
    new_n <- extract_from_state_by_key(Gridcell, k)
    if (length(new_n) != 1) stop("Invalid new number_of_individuals at key: ", k)
    new_n <- as.integer(new_n)
    
    if (!identical(old_n, new_n)) {
      changed <- TRUE
      sp <- .parse_stand_patch_from_nind_key(k)
      if (is.null(sp)) stop("Failed to parse stand/patch from key: ", k)
      
      # locate old individuals byte-range in source file
      prefix_inds <- paste0("Gridcell/Stand/", sp$stand, "/Patch/", sp$patch,
                            "/Vegetation/Individuals/")
      
      ind_hit <- startsWith(all_keys, prefix_inds)
      
      if (old_n > 0 && any(ind_hit)) {
        # old block starts at first Individuals key position
        start_ind <- min(all_pos[ind_hit])
        
        # old block ends at the next key position after the last Individuals key
        idx_last <- max(which(ind_hit))
        end_ind <- if (idx_last < length(all_pos)) all_pos[idx_last + 1] else file_size
      } else {
        # old_n == 0 => empty block
        start_ind <- pos_nind + 4
        end_ind   <- pos_nind + 4
      }
      
      sch <- .schema_for_patch(sp$stand, sp$patch)
      if (is.null(sch)) sch <- schema_global
      
      mods[[length(mods) + 1]] <- list(
        key = k,
        stand = sp$stand, patch = sp$patch,
        pos_nind = pos_nind,
        start_ind = start_ind,
        end_ind = end_ind,
        old_n = old_n, new_n = new_n,
        schema = sch
      )
    }
  }
  
  if (use_fast_inplace_if_possible && !changed) {
    if (verbose) message("No cohort count change detected; using fast in-place writer.")
    return(write_binary_LPJGUESS(State_updated, outdir))
  }
  
  if (verbose) {
    message("Rebuilding 0.state by binary splice for ", length(mods), " patch(es).")
  }
  
  # sort mods by nind position ascending (important for multiple edits)
  mods <- mods[order(vapply(mods, `[[`, numeric(1), "pos_nind"))]
  
  # ---- rebuild by splice: copy raw everywhere except nind+Individuals segments
  zz_src <- file(bak_file, "rb")
  zz_dst <- file(tmp_file, "wb")
  on.exit({
    try(close(zz_src), silent = TRUE)
    try(close(zz_dst), silent = TRUE)
  }, add = TRUE)
  
  src_cursor <- 0
  
  for (m in mods) {
    # copy [src_cursor, pos_nind)
    seek(zz_src, where = src_cursor, origin = "start")
    .copy_raw_bytes(zz_src, zz_dst, m$pos_nind - src_cursor)
    
    # skip old nind (4 bytes) in src, write new nind to dst
    seek(zz_src, where = m$pos_nind, origin = "start")
    readBin(zz_src, integer(), 1, size = 4, endian = "little")
    writeBin(as.integer(m$new_n), zz_dst, size = 4, endian = "little")
    
    # copy bytes between nind and old individuals start (usually 0)
    src_cursor <- m$pos_nind + 4
    if (m$start_ind < src_cursor) stop("start_ind < pos_nind+4 for ", m$key)
    
    seek(zz_src, where = src_cursor, origin = "start")
    .copy_raw_bytes(zz_src, zz_dst, m$start_ind - src_cursor)
    
    # write NEW Individuals block
    .write_individuals_block(zz_dst, Gridcell, m$stand, m$patch, m$schema)
    
    # skip OLD Individuals bytes in src
    src_cursor <- m$end_ind
  }
  
  # copy tail
  seek(zz_src, where = src_cursor, origin = "start")
  .copy_raw_bytes(zz_src, zz_dst, file_size - src_cursor)
  
  close(zz_src); close(zz_dst)
  
  # atomic replace
  if (file.exists(state_file)) file.remove(state_file)
  ok <- file.rename(tmp_file, state_file)
  if (!ok) stop("Failed to replace 0.state. tmp at: ", tmp_file)
  
  invisible(TRUE)
}


