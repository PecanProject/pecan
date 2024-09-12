##' Change treatments to sequential integers
##'
##' Assigns all control treatments the same value, then assigns unique treatments
##' within each site. Each site is required to have a control treatment.
##' The algorithm (incorrectly) assumes that each site has a unique set of experimental
##' treatments. This assumption is required by the data in BETYdb that does not always consistently name treatments or quantity them in the managements table. Also it avoids having the need to estimate treatment by site interactions in the meta analysis model. This model uses data in the control treatment to estimate model parameters so the impact of the assumption is minimal.
##' @name assign.treatments
##' @title assign.treatments
##' @param data input data
##' @return dataframe with sequential treatments
##' @export
##' @author David LeBauer, Carl Davidson, Alexey Shiklomanov
assign.treatments <- function(data){
  data$trt_id[which(data$control == 1)] <- "control"
  sites <- unique(data$site_id)
  # Site IDs may be returned as `integer64`, which the `for` loop
  # type-coerces to regular integer, which turns it into gibberish.
  # Looping over the index instead prevents this type coercion.
  for (si in seq_along(sites)) {
    ss <- sites[[si]]
    site.i <- data$site_id == ss
    #if only one treatment, it's control
    if (length(unique(data$trt_id[site.i])) == 1) data$trt_id[site.i] <- "control"
    if (!"control" %in% data$trt_id[site.i]){
      PEcAn.logger::logger.severe(paste0(
        "No control treatment set for site_id ", unique(data$site_id[site.i]),
        " and citation id ", unique(data$citation_id[site.i]), ".\n",
        "Please set control treatment for this site / citation in database.\n"
      ))
    }
  }
  return(data)
}

drop.columns <- function(data, columns){
  return(data[, which(!colnames(data) %in% columns)])
}
##=============================================================================#
