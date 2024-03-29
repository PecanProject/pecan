##' met.process.stage
##' @export
##'
##' @param input.id bety db for input format
##' @param raw.id format id for the raw met data
##' @param con database connection
##'
##' @author Elizabeth Cowdery
met.process.stage <- function(input.id, raw.id, con) {
  
  format.id <- PEcAn.DB::db.query(paste("SELECT format_id from inputs where id =", input.id), con)[[1]]
  cf.id     <- 33
  
  if (format.id == raw.id && format.id != cf.id) {
    stage <- list(download.raw = FALSE, met2cf = TRUE, standardize = TRUE, 
                  met2model = TRUE, id.name = "raw.id")
  } else if (format.id == cf.id) {
    # we will still do the standardization since extracting/gapfilling etc 
    # more than once makes no difference
    stage <- list(download.raw = FALSE, met2cf = FALSE, standardize = TRUE, 
                  met2model = TRUE, id.name = "cf.id")
  } else {
    # assume the only other option is a model format so nothing needs to be done
    stage <- list(download.raw = FALSE, met2cf = FALSE, standardize = FALSE, 
                  met2model = FALSE, id.name = "model.id")
  }
  return(invisible(stage))
} # met.process.stage
