
#' Creates an absolute path to a folder.
#'
#' This will take a folder and make it into an absolute folder name. It
#' will normalize the path and prepend it with the current working folder
#' if needed to get an absolute path name.
#'
#' @param folder folder for file paths.
#' @author Rob Kooper
#' @return absolute path
#' @export
#' @examples
#' full.path('pecan')
full.path <- function(folder) {
  # normalize pathname
  folder <- normalizePath(folder, mustWork = FALSE)

  # add cwd if needed
  if (substr(folder, 1, 1) != "/") {
    folder <- file.path(getwd(), folder)
    folder <- normalizePath(folder, mustWork = FALSE)
  }

  return(invisible(folder))
} # full.path
