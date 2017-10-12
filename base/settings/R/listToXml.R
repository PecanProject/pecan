##' @export
listToXml <- function(x, ...) {
  UseMethod("listToXml")
} # listToXml


#--------------------------------------------------------------------------------------------------#
##' Convert List to XML
##'
##' Can convert list or other object to an xml object using xmlNode
##' @title List to XML
##' @param item object to be converted. Despite the function name, need not actually be a list
##' @param tag xml tag
##' @return xmlNode
##' @export
##' @author David LeBauer, Carl Davidson, Rob Kooper
listToXml.default <- function(item, tag) {

  # just a textnode, or empty node with attributes
  if (typeof(item) != "list") {
    if (length(item) > 1) {
      xml <- XML::xmlNode(tag)
      for (name in names(item)) {
        XML::xmlAttrs(xml)[[name]] <- item[[name]]
      }
      return(xml)
    } else {
      return(XML::xmlNode(tag, item))
    }
  }

  # create the node
  if (identical(names(item), c("text", ".attrs"))) {
    # special case a node with text and attributes
    xml <- XML::xmlNode(tag, item[["text"]])
  } else {
    # node with child nodes
    xml <- XML::xmlNode(tag)
    for (i in seq_along(item)) {
      if (is.null(names(item)) || names(item)[i] != ".attrs") {
        xml <- XML::append.xmlNode(xml, listToXml(item[[i]], names(item)[i]))
      }
    }
  }

  # add attributes to node
  attrs <- item[[".attrs"]]
  for (name in names(attrs)) {
    XML::xmlAttrs(xml)[[name]] <- attrs[[name]]
  }
  return(xml)
} # listToXml.default
