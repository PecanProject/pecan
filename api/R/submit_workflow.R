#' Post complete settings list as RabbitMQ message
#'
#' @param settings PEcAn settings list object
#' @param rabbitmq_hostname RabbitMQ server hostname (character.
#'   Default = `"localhost"`)
#' @param rabbitmq_port RabbitMQ server port (character or numeric.
#'   Default = option `pecanapi.docker_port`)
#' @param rabbitmq_user RabbitMQ user name (character. Default =
#'   option `pecanapi.rabbitmq_user`)
#' @param rabbitmq_password RabbitMQ password (character. Default =
#'   option `pecanapi.rabbitmq_password`)
#' @param rabbitmq_prefix Complete RabbitMQ API prefix. If `NULL`
#'   (default), this is constructed from the other arguments. If this
#'   argument is not `NULL`, it overrides all other arguments except
#'   `httr_auth` and `settings`.
#' @param httr_auth Whether or not to use [httr::authenticate] to
#'   generate CURL authentication (default = `TRUE`). If `FALSE`, you
#'   must pass the authentication as part of the RabbitMQ hostname or prefix.
#' @param https Whether or not to use `https`. If `FALSE`, use `http`.
#'   Default = option `pecanapi.docker_https`
#' @return Curl `POST` output, parsed by [httr::content]
#' @author Alexey Shiklomanov
#' @export
submit_workflow <- function(settings,
                            rabbitmq_hostname = getOption("pecanapi.docker_hostname"),
                            rabbitmq_frontend = getOption("pecanapi.docker_rabbitmq_frontend"),
                            rabbitmq_port = getOption("pecanapi.docker_port"),
                            rabbitmq_user = getOption("pecanapi.rabbitmq_user"),
                            rabbitmq_password = getOption("pecanapi.rabbitmq_password"),
                            rabbitmq_prefix = NULL,
                            httr_auth = TRUE,
                            https = getOption("pecanapi.docker_https")) {
  if (is.numeric(rabbitmq_port)) rabbitmq_port <- as.character(rabbitmq_port)
  # Create xml object
  settings_xml <- listToXML(settings, "pecan")
  settings_xml_string <- XML::toString.XMLNode(settings_xml)
  settings_json <- jsonlite::toJSON(
    list(pecan_xml = settings_xml_string, folder = settings[["outdir"]]),
    auto_unbox = TRUE
)
  bod_raw <- list(
    properties = list(delivery_mode = 2),
    routing_key = "pecan",
    payload = settings_json,
    payload_encoding = "string"
  )
  auth <- NULL
  if (httr_auth) {
    auth <- httr::authenticate(rabbitmq_user, rabbitmq_password)
  } 
  bod <- jsonlite::toJSON(bod_raw, auto_unbox = TRUE)
  if (is.null(rabbitmq_prefix)) {
    httpstring <- "http"
    if (https) httpstring <- "https"
    base_url <- sprintf("%s://%s:%s", httpstring, rabbitmq_hostname, rabbitmq_port)
    rabbitmq_prefix <- paste0(base_url, rabbitmq_frontend)
  }
  result <- httr::POST(
    paste0(rabbitmq_prefix, "/api/exchanges/%2F//publish"),
    auth,
    body = bod
  )
  follow_url <- sprintf("%s/pecan/05-running.php?workflowid=%s",
                        base_url, as.character(settings[["workflow"]][["id"]]))
  message("Follow workflow progress from your browser at:\n", follow_url)
  httr::content(result)
}

#' Convert List to XML
#'
#' Can convert list or other object to an xml object using xmlNode
#' @param item object to be converted. Despite the function name, need not actually be a list
#' @param tag xml tag
#' @return xmlNode
#' @author David LeBauer, Carl Davidson, Rob Kooper
listToXML <- function(item, tag) {

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
        xml <- XML::append.xmlNode(xml, listToXML(item[[i]], names(item)[i]))
      }
    }
  }

  # add attributes to node
  attrs <- item[[".attrs"]]
  for (name in names(attrs)) {
    XML::xmlAttrs(xml)[[name]] <- attrs[[name]]
  }
  return(xml)
} # listToXML
