##' Read BioCro config file
##'
##' @title Read BioCro Config 
##' @param config.file 
##' @return list of run configuration parameters for PEcAn
##' @export
##' @author David LeBauer
read.biocro.config <- function(config.file = "config.xml"){
    config <- xmlToList(
        xmlTreeParse(
            file = config.file,
            handlers = list("comment" = function(x){NULL}),
            asTree = TRUE)
        )
    config$pft$canopyControl$mResp <- ## hacky way of importing a vector from xml to a list
        unlist(
            strsplit(
                config$pft$canopyControl$mResp, split = ","))
    return(config)
}
