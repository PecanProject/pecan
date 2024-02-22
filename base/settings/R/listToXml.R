#' A generic function to convert list to XML
#'
#' @param x list to be converted
#' @param ... arguments passed to methods
#' @export
listToXml <- function(x, ...) {
  UseMethod("listToXml")
} # listToXml


#' Convert List to XML
#'
#' Can convert list or other object to an xml object using xmlNode
#' @title List to XML
#' @param x object to be converted.
#'   Despite the function name, need not actually be a list
#' @param ... further arguments.
#'   Used to set the element name of the created XML object,
#'   which is taken from an argument named `tag` if present,
#'   or otherwise from the first element of `...`
#' @return xmlNode
#' @export
#' @author David LeBauer, Carl Davidson, Rob Kooper
listToXml.default <- function(x, ...) {
  args <- list(...)
  if (length(args) == 0) {
    PEcAn.logger::logger.error("no tag provided")
  } else if ("tag" %in% names(args)) {
    tag <- args$tag
  } else {
    tag <- args[[1]]
  }
  # just a textnode, or empty node with attributes
  if (typeof(x) != "list") {
    if (length(x) > 1) {
      xml <- XML::xmlNode(tag)
      for (name in names(x)) {
        XML::xmlAttrs(xml)[[name]] <- x[[name]]
      }
      return(xml)
    } else {
      return(XML::xmlNode(tag, x))
    }
  }
  
  # create the node
  if (identical(names(x), c("text", ".attrs"))) {
    # special case a node with text and attributes
    xml <- XML::xmlNode(tag, x[["text"]])
  } else {
    # node with child nodes
    xml <- XML::xmlNode(tag)
    for (i in seq_along(x)) {
      if (is.null(names(x)) || names(x)[i] != ".attrs") {
        xml <- XML::append.xmlNode(xml, listToXml(x[[i]], names(x)[i]))
      }
    }
  }
  
  # add attributes to node
  attrs <- x[[".attrs"]]
  for (name in names(attrs)) {
    XML::xmlAttrs(xml)[[name]] <- attrs[[name]]
  }
  return(xml)
} # listToXml.default
