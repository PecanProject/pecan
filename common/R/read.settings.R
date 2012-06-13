#--------------------------------------------------------------------------------------------------#
#' 
#' @name read.settings.R
#' @title Generic function to open and parse PEcAn XML config/settings file 
#'
#' NEED MORE HERE. WORKING DRAFT OF SCRIPT TO OPEN PECAN SETTINGS
#'
#'
#==================================================================================================#


#---------------- Load requirements for function. -------------------------------------------------#
# Info: Load required libraries for running this function inside the PEcAn workflow.
#' @import XML
if (!require(XML)) stop("Package XML is not available...")      # R XML library
#--------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------#
# INTERNAL FUNCTIONS DO NOT EXPORT
#--------------------------------------------------------------------------------------------------#
# merge 2 xml documents
xmlMerge <- function(xml1, xml2) {
  if (is.null(xml2)) {
    return(xml1)
  }
  if (is.null(xml1)) {
    return(xml2)
  }
  
  xmlMergeNodes(xmlRoot(xml1), xmlRoot(xml2))
  return(xml1)
}

# merge 2 nodes, this is called recursively
xmlMergeNodes <- function(node1, node2) {
  # first replace all attributes from node2 to node1
  if (!is.null(xmlAttrs(node2))) {
    addAttributes(node=node1, .attrs=xmlAttrs(node2), append=TRUE)   
  }
  
  # add all nodes in node2 that are not in node1
  kidsnames <- names(node2)[!(names(node2) %in% names(node1))]
  if (length(kidsnames) > 0) {
    addChildren(node1, kids=xmlChildren(node2)[kidsnames])
  }
  
  # loop through all nodes in common
  for(name in names(node2)[names(node2) %in% names(node1)]) {
    if ((length(names(xmlChildren(node1[[name]]))) == 1) && (names(xmlChildren(node1[[name]])) == "text")) {
      addAttributes(node=node1[[name]], .attrs=xmlAttrs(node2[[name]]), append=TRUE)
      addAttributes(node=node2[[name]], .attrs=xmlAttrs(node1[[name]]), append=FALSE)
      replaceNodes(node1[[name]], node2[[name]])
    } else {
      xmlMergeNodes(xmlChildren(node1)[[name]], xmlChildren(node2)[[name]])         
    }
  }
}

#--------------------------------------------------------------------------------------------------#
# EXTERNAL FUNCTIONS
#--------------------------------------------------------------------------------------------------#

#' Loads PEcAn settings file
#' 
#' This will load the PEcAn settings file in the following order,
#' merging their values and overriding any values that are specified
#' in a file later in the order
#' 
#' \enumerate{
#' \item /etc/pecan.xml}
#' \item ~/.pecan.xml}
#' \item environment variable PECAN_SETTINGS
#' \item passed as file argument to function
#' \item passed as command line argument using --settings
#' }
#' @param inputfile the PEcAn settings file to be merged with the others.
#' @param outputfile the name of file to which the settings will be
#'        written inside the outputdir.
#' @return list of all settings as loaded from the XML file(s)
#' @export
#' @import XML
#' @author Shawn Serbin
#' @author Rob Kooper
#' @examples
#' \dontrun{
#' settings <- read.settings()
#' settings <- read.settings(file="willowcreek.xml")
#' }
read.settings <- function(inputfile=NULL, outputfile="pecan.xml"){
  settings.xml <- NULL
  
  # 1 load /etc/pecan.xml
  if (file.exists("/etc/pecan.xml")) {
    settings.xml <- xmlMerge(settings.xml, xmlParse("/etc/pecan.xml"))
  }
  
  # 2 merge ~/.pecan.xml
  if (file.exists("~/.pecan.xml")) {
    settings.xml <- xmlMerge(settings.xml, xmlParse("~/.pecan.xml"))
  }
  
  # 3 merge pecan.xml
  if (file.exists("pecan.xml")) {
    settings.xml <- xmlMerge(settings.xml, xmlParse("pecan.xml"))
  }
  print(settings.xml)
  
  # 4 merge PECAN_SETTINGS
  if (file.exists(Sys.getenv("PECAN_SETTINGS"))) {
    settings.xml <- xmlMerge(settings.xml, xmlParse(Sys.getenv("PECAN_SETTINGS")))
  }
  
  # 5 merge file  
  if (!is.null(inputfile) && file.exists(inputfile)) {
    settings.xml <- xmlMerge(settings.xml, xmlParse(inputfile))
  }
  
  # 6 merge command line arguments
  loc <- which(commandArgs() == "--settings")
  if (length(loc) != 0) {
    for(idx in loc) {
      if (!is.null(commandArgs()[idx+1]) && file.exists(commandArgs()[idx+1])) {
        settings.xml <- xmlMerge(settings.xml, xmlParse(commandArgs()[idx+1]))
      }
    }
  }
  settings.list <- xmlToList(settings.xml)
  
  # 6a create main output folder
  if (!file.exists(settings.list$outdir)) {
    dir.create(settings.list$outdir, recursive=TRUE)
  }
  
  # 6b either save or load pecan.xml for main output folder
  settings.output <- file.path(outputfile, settings.list$outdir)
  if (file.exists(settings.output)) {
    settings.xml <- xmlParse(settings.output)
    settings.list <- xmlToList(settings.xml)
  } else {
    saveXML(settings.xml, file=settings.output)
  }
  
  # setup Rlib from settings
  if(!is.null(settings.list$Rlib)){ 
    .libPaths(settings.list$Rlib)
  }
   
  # Return settings file as a list
  return(settings.list)
}

#==================================================================================================#

####################################################################################################
### EOF.  End of R script file.  						
####################################################################################################
