#' Summarize netcdf variables into text file
#'
#' @param nc_dir directory to scan for netCDF files (including in subdirs)
#' @param write_mode how many files to write:
#'  "paired" creates files named *.nc.var` alongside every `*.nc`,
#'  "collected" writes a single `<nc_dir>/nc_vars.txt` that lists every variable
#'    defined in any nc file in the directory
#' @importFrom rlang .data
#' @export
nc_write_varfiles <- function(nc_dir,
                              write_mode = c("paired", "collected")) {

  ncfiles <- list.files(
    nc_dir,
    pattern = "\\.nc$",
    recursive = TRUE,
    full.names = TRUE
  )
  vartables <- lapply(ncfiles, nc_longnames)

  write_mode <- match.arg(write_mode)
  if (write_mode == "paired") {
    varfiles <- paste0(ncfiles, ".var")
    purrr::walk2(vartables, varfiles, nc_write_varfile)
    return(NULL)
  } else {
    vartables |>
      do.call(what = "rbind") |>
      unique() |>
      dplyr::arrange(.data$name) |>
      nc_write_varfile(file.path(nc_dir, "nc_vars.txt"))
  }
}



nc_longnames <- function(ncfile) {
  nc <- ncdf4::nc_open(ncfile)
  on.exit(ncdf4::nc_close(nc))

  # A named vector: c(name1 = "longname_1", ...)
  nm <- sapply(nc$var, `[[`, "longname")

  data.frame(name = names(nm), longname = nm)
}


nc_write_varfile <- function(df, varfile) {
  utils::write.table(
    x = df,
    file = varfile,
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE
  )
}
