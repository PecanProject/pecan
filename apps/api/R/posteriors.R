library(dplyr)

#' Search for Posteriors containing wildcards for filtering
#' @param pft_id PFT Id (character)
#' @param offset
#' @param limit
#' @return Information about Posteriors based on pft
#' @author Nihar Sanda
#* @get /
searchPosteriors <- function(req, pft_id = NA, host_id = NA, offset = 0, limit = 50, res) {
  if (!limit %in% c(10, 20, 50, 100, 500)) {
    res$status <- 400
    return(list(error = "Invalid value for parameter"))
  }
  
  posteriors <- tbl(global_db_pool, "posteriors") %>%
    select(everything())
  
  posteriors <- tbl(global_db_pool, "dbfiles") %>%
    select(file_name, file_path, container_type, id = container_id, machine_id) %>%
    inner_join(posteriors, by = "id") %>%
    filter(container_type == "Posterior") %>%
    select(-container_type)
  
  posteriors <- tbl(global_db_pool, "machines") %>%
    select(hostname, machine_id = id) %>%
    inner_join(posteriors, by = "machine_id")
  
  posteriors <- tbl(global_db_pool, "pfts") %>%
    select(pft_name = name, pft_id = id) %>%
    inner_join(posteriors, by = "pft_id")
  
  if (!is.na(pft_id)) {
    posteriors <- posteriors %>%
      filter(pft_id == !!pft_id)
  }
  
  if (!is.na(host_id)) {
    posteriors <- posteriors %>%
      filter(machine_id == !!host_id)
  }
  
  qry_res <- posteriors %>%
    select(-pft_id, -machine_id) %>%
    distinct() %>%
    arrange(id) %>%
    collect()
  
  if (nrow(qry_res) == 0 || as.numeric(offset) >= nrow(qry_res)) {
    res$status <- 404
    return(list(error = "Posterior(s) not found"))
  } else {
    has_next <- FALSE
    has_prev <- FALSE
    if (nrow(qry_res) > (as.numeric(offset) + as.numeric(limit))) {
      has_next <- TRUE
    }
    if (as.numeric(offset) != 0) {
      has_prev <- TRUE
    }
    
    qry_res <- qry_res[(as.numeric(offset) + 1):min((as.numeric(offset) + as.numeric(limit)), nrow(qry_res)), ]
    
    result <- list(posteriors = qry_res)
    result$count <- nrow(qry_res)
    if (has_next) {
      if (grepl("offset=", req$QUERY_STRING, fixed = TRUE)) {
        result$next_page <- paste0(
          req$rook.url_scheme, "://",
          req$HTTP_HOST,
          "/api/posteriors",
          req$PATH_INFO,
          substr(req$QUERY_STRING, 0, stringr::str_locate(req$QUERY_STRING, "offset=")[[2]]),
          (as.numeric(limit) + as.numeric(offset)),
          "&limit=",
          limit
        )
      } else {
        result$next_page <- paste0(
          req$rook.url_scheme, "://",
          req$HTTP_HOST,
          "/api/posteriors",
          req$PATH_INFO,
          substr(req$QUERY_STRING, 0, stringr::str_locate(req$QUERY_STRING, "limit=")[[2]] - 6),
          "offset=",
          (as.numeric(limit) + as.numeric(offset)),
          "&limit=",
          limit
        )
      }
    }
    if (has_prev) {
      result$prev_page <- paste0(
        req$rook.url_scheme, "://",
        req$HTTP_HOST,
        "/api/workflows",
        req$PATH_INFO,
        substr(req$QUERY_STRING, 0, stringr::str_locate(req$QUERY_STRING, "offset=")[[2]]),
        max(0, (as.numeric(offset) - as.numeric(limit))),
        "&limit=",
        limit
      )
    }
    
    return(result)
  }
}

#################################################################################################

#' Download the posterior specified by the id
#' @param id Posterior id (character)
#' @param filename Optional filename specified if the id points to a folder instead of file (character)
#' If this is passed with an id that actually points to a file, this name will be ignored
#' @return Posterior file specified by user
#' @author Nihar Sanda
#* @serializer contentType list(type="application/octet-stream")
#* @get /<posterior_id>
downloadPosterior <- function(posterior_id, filename = "", req, res) {
  db_hostid <- PEcAn.DB::dbHostInfo(global_db_pool)$hostid
  
  # This is just for temporary testing due to the existing issue in dbHostInfo()
  db_hostid <- ifelse(db_hostid == 99, 99000000001, db_hostid)
  
  posterior <- tbl(global_db_pool, "dbfiles") %>%
    select(file_name, file_path, container_id, machine_id, container_type) %>%
    filter(machine_id == !!db_hostid) %>%
    filter(container_type == "Posterior") %>%
    filter(container_id == !!posterior_id) %>%
    collect()
  
  if (nrow(posterior) == 0) {
    res$status <- 404
    return()
  } else {
    # Generate the full file path using the file_path & file_name
    filepath <- paste0(posterior$file_path, "/", posterior$file_name)
    
    # If the id points to a directory, check if 'filename' within this directory has been specified
    if (dir.exists(filepath)) {
      # If no filename is provided, return 400 Bad Request error
      if (filename == "") {
        res$status <- 400
        return()
      }
      
      # Append the filename to the filepath
      filepath <- paste0(filepath, filename)
    }
    
    # If the file doesn't exist, return 404 error
    if (!file.exists(filepath)) {
      res$status <- 404
      return()
    }
    
    # Read the data in binary form & return it
    bin <- readBin(filepath, "raw", n = file.info(filepath)$size)
    return(bin)
  }
}
