##' @name read.register
##' @title read.register
##' @export
##' @param register.xml path of xml file
##' @param con betydb connection
##' 
##' @author Betsy Cowdery
read.register <- function(register.xml, con) {
  
  library(PEcAn.DB)
  library(PEcAn.utils)
  
  register <- XML::xmlToList(XML::xmlParse(register.xml))
  print(as.data.frame(register))
  
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
   PEcAn.logger::logger.error("Browndog input type is not defined")  #Ultimatly can get this from the format table in betydb
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
      register$format$name <- db.query(paste("SELECT name from formats where id = ", register$format$id), 
                                       con)[[1]]
      register$format$mimetype <- db.query(paste("SELECT mime_type from formats where id = ", 
                                                 register$format$id), con)[[1]]
    } else if (is.null(register$format$id) & !is.null(register$format$name) & !is.null(register$format$mimetype)) {
      register$format$id <- db.query(paste0("SELECT id from formats where name = '", register$format$name, 
                                            "' and mime_type = '", register$format$mimetype, "'"), con)[[1]]
    }
  }
  return(invisible(register))
} # read.register
