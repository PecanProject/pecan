#' Read BioCro config file
#'
#' @param config.file Path to XML file
#' @return list of run configuration parameters for PEcAn
#' @export
#' @author David LeBauer
read.biocro.config <- function(config.file = "config.xml") {
  config <- XML::xmlToList(XML::xmlTreeParse(file = config.file,
                                   handlers = list(comment = function(x) { NULL }),
                                   asTree = TRUE))
  if(utils::packageVersion('BioCro') < 1.0){
    config$pft$canopyControl$mResp <- unlist(strsplit(config$pft$canopyControl$mResp, split = ","))
  }
  if(!is.null(config$pft$initial_values)){
    config$pft$initial_values <- lapply(config$pft$initial_values, as.numeric)
  }
  if(!is.null(config$pft$parameters)){
    config$pft$parameters <- lapply(config$pft$parameters, as.numeric)
  }
  return(config)
}  # read.biocro.config
