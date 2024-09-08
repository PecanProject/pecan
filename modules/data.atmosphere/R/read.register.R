##' @name read.register
##' @title read.register
##' @export
##' @param register.xml path of xml file
##' @param con betydb connection
##' 
##' @author Betsy Cowdery
read.register <- function(register.xml, con) {
  
  register <- XML::xmlToList(XML::xmlParse(register.xml))
  PEcAn.logger::logger.debug(as.data.frame(register))
  
  # check scale
  if (is.null(register$scale)) {
   PEcAn.logger::logger.error("Scale is not defined")
  } else {
    if (register$scale == "regional" & is.null(register$siteid)) {
     PEcAn.logger::logger.warn("Region site id is not defined")
    }
  }
  
  # check format format is not defined
  if (is.null(register$format)) {
   PEcAn.logger::logger.error("Format is not defined")
  } else if (is.null(register$format$inputtype)) {
   PEcAn.logger::logger.error("Input type is not defined")  #Ultimatly can get this from the format table in betydb
  } else {
    # format is defined
    if ((is.null(register$format$id) & is.null(register$format$name) & is.null(register$format$mimetype))
        | 
        (is.null(register$format$id) & is.null(register$format$name)) 
        |
        (is.null(register$format$id) & is.null(register$format$mimetype))) {
     PEcAn.logger::logger.error("Not enough format info")
    } else if ((!is.null(register$format$id) & is.null(register$format$name)) 
               | 
               (!is.null(register$format$id) & is.null(register$format$mimetype))) {
      # Retrieve format name and mimetype from the database
      query.format.info <- PEcAn.DB::db.query(
        paste(
          "SELECT name, type_string AS mimetype",
          "FROM formats JOIN mimetypes ON formats.mimetype_id = mimetypes.id",
          "WHERE formats.id = ", register$format$id),
        con
      )
      
      register$format$name <- query.format.info$name
      register$format$mimetype <- query.format.info$mimetype

    } else if (is.null(register$format$id) & !is.null(register$format$name) & !is.null(register$format$mimetype)) {
      register$format$id <- PEcAn.DB::db.query(
        paste0(
          "SELECT formats.id FROM formats JOIN mimetypes ON formats.mimetype_id = mimetypes.id ",
          "WHERE name = '", register$format$name,
               "' AND type_string = '", register$format$mimetype, "'"), con)[[1]]
    }
  }
  return(invisible(register))
} # read.register
