##' Check two lists. Identical does not work since one can be loaded
##' from the database and the other from a CSV file.
##'
##' @name check.lists
##' @title Compares two lists
##' @param x first list
##' @param y second list
##' @param filename one of "species.csv" or "cultivars.csv"
##' @return true if two lists are the same
##' @author Rob Kooper
##'
check.lists <- function(x, y, filename = "species.csv") {
  if (nrow(x) != nrow(y)) {
    return(FALSE)
  }
  if(filename == "species.csv"){
    cols <- c('id', 'genus', 'species', 'scientificname')
  } else if (filename == "cultivars.csv") {
    cols <- c('id', 'specie_id', 'species_name', 'cultivar_name')
  } else {
    return(FALSE)
  }
  xy_match <- vapply(cols, function(i) identical(as.character(x[[i]]), as.character(y[[i]])), logical(1))
  return(all(unlist(xy_match)))
}
