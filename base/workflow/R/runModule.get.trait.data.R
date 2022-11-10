##' Get trait data for all PFTs in a settings list
##'
##' @param settings PEcAn configuration list. Must have class `Settings` or
##'   `MultiSettings`
##' @export
runModule.get.trait.data <- function(settings) {
  if (is.null(settings$meta.analysis)) return(settings) ## if there's no MA, there's no need for traits
  if (PEcAn.settings::is.MultiSettings(settings)) {
    pfts <- list()
    pft.names <- character(0)
    for (i in seq_along(settings)) {
      pfts.i <- settings[[i]]$pfts
      if (!is.list(pfts.i)) {
        PEcAn.logger::logger.severe("settings[[i]]$pfts is not a list")
      }
      pft.names.i <- sapply(pfts.i, function(x) x$name)
      ind <- which(pft.names.i %in% setdiff(pft.names.i, pft.names))
      pfts <- c(pfts, pfts.i[ind])
      pft.names <- sapply(pfts, function(x) x$name)
    }

    PEcAn.logger::logger.info(paste0(
      "Getting trait data for all PFTs listed by any Settings object in the list: ",
      paste(pft.names, collapse = ", ")
    ))

    modeltype <- settings$model$type
    dbfiles <- settings$database$dbfiles
    database <- settings$database$bety
    forceupdate <-
      ifelse(is.null(settings$meta.analysis$update),
             FALSE,
             settings$meta.analysis$update)
    write <- settings$database$bety$write
    settings$pfts <-
      PEcAn.DB::get.trait.data(
        pfts = pfts,
        modeltype = modeltype,
        dbfiles = dbfiles,
        database = database,
        forceupdate = forceupdate,
        write = write
      )
    return(settings)
  } else if (PEcAn.settings::is.Settings(settings)) {
    pfts <- settings$pfts
    if (!is.list(pfts)) {
      PEcAn.logger::logger.severe("settings$pfts is not a list")
    }
    modeltype <- settings$model$type
    dbfiles <- settings$database$dbfiles
    database <- settings$database$bety
    forceupdate <-
      ifelse(is.null(settings$meta.analysis$update),
             FALSE,
             settings$meta.analysis$update)
    write <- settings$database$bety$write
    settings$pfts <-
      PEcAn.DB::get.trait.data(
        pfts = pfts,
        modeltype = modeltype,
        dbfiles = dbfiles,
        database = database,
        forceupdate = forceupdate,
        write = write
      )
    return(settings)
  } else {
    stop("runModule.get.trait.data only works with Settings or MultiSettings")
  }
}
