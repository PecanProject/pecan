##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------
library(XML)

##--------------------------------------------------------------------------------------------------#
## INTERNAL FUNCTIONS DO NOT EXPORT
##--------------------------------------------------------------------------------------------------#
                                        #
##'  merge 2 xml documents
##'
##' combines content from xml1 and xml2. In the case of a conflict (both xml1 and xml2 have the same node), node in xml2 replaces node in xml1 
##' @title xmlMerge
##' @param xml1 first xml list 
##' @param xml2 second xml list
##' @export
##' @return xml object with 
##' @author Rob Kooper
xmlMerge <- function(xml1, xml2) {
  if (is.null(xml2)) {
    return(xml1)
  }
  
  ## TODO no merging for now, it will simply return latest file
  ## TODO see https://ebi-forecast.igb.illinois.edu/redmine/issues/1091
  return(xml2)
  
  ##  if (is.null(xml1)) {
  ##    return(xml2)
  ##  }
  ##   
  ##  xmlMergeNodes(xmlRoot(xml1), xmlRoot(xml2))
  ##  return(xml1)
}


##' merge 2 nodes, this is called recursively
##'
##' @title xmlMergeNodes
##' @param node1 
##' @param node2 
##' @return merged nodes
##' @author Rob Kooper
xmlMergeNodes <- function(node1, node2) {
  ## first replace all attributes from node2 to node1
  if (!is.null(xmlAttrs(node2))) {
    addAttributes(node=node1, .attrs=xmlAttrs(node2), append=TRUE)   
  }
  
  ## add all nodes in node2 that are not in node1
  kidsnames <- names(node2)[!(names(node2) %in% names(node1))]
  if (length(kidsnames) > 0) {
    addChildren(node1, kids=xmlChildren(node2)[kidsnames])
  }
  
  ## loop through all nodes in common
  for(name in names(node2)[names(node2) %in% names(node1)]) {
    if ("XMLInternalCommentNode" %in% class(node1[[name]])) {
      next
    }
    if ((length(names(xmlChildren(node1[[name]]))) == 1) &&( names(xmlChildren(node1[[name]])) == "text")) {
      addAttributes(node=node1[[name]], .attrs=xmlAttrs(node2[[name]]), append=TRUE)
      addAttributes(node=node2[[name]], .attrs=xmlAttrs(node1[[name]]), append=FALSE)
      replaceNodes(node1[[name]], node2[[name]])
    } else {
      xmlMergeNodes(xmlChildren(node1)[[name]], xmlChildren(node2)[[name]])         
    }
  }
}

##' Sanity checks. Checks the settings file to make sure expected fields exist.
##'
##' Expected fields in settings file are:
##' - pfts with at least one pft defined
##' - database
##' - model
##' - run with the following fields
##' -- site with id
##' -- host with name
##' @title Check Settings
##' @param settings settings file
##' @return error if minimum fields are not provided
##' @author Rob Kooper
check.settings <- function(settings) {
  if (!is.null(settings$nocheck)) {
    logger.info("Not doing sanity checks of pecan.xml")
    return(0)
  }

  # check database information
  if (is.null(settings$database)) {
    stop("No database information specified.")
  }
  if (is.null(settings$database$userid)) {
    stop("No userid specified for the database")
  }
  if (is.null(settings$database$passwd)) {
    stop("No passwd specified for the database")
  }
  if (is.null(settings$database$name)) {
    stop("No name specified for the database")
  }
  if (is.null(settings$database$name)) {
    settings$database$location = "localhost"
    logger.info("Setting localhost for database location.")
  }

  # TODO do a quick connection

  # TODO check userid and userpassword

  # check to make sure run information is filled out
  if (is.null(settings$run$host$name)) {
    stop("Did not set hostname in run section.")
  }
  if (settings$run$host$name != "localhost") {
    if (is.null(settings$run$host$rundir)) {
      stop("not rundir specified on remote machine.")
    }
    if (is.null(settings$run$host$outdir)) {
      stop("not outdir specified on remote machine.")
    }
  }

  # make sure there are pfts defined
  if (is.null(settings$pfts) || (length(settings$pfts) == 0)) {
    stop("No PFTS specified.")
  }
}

listToXml <- function(item, tag){
  if(typeof(item)!='list')
    return(xmlNode(tag, item))
  xml <- xmlNode(tag)
  for(i in seq(length(item))) {
    if (names(item[i]) == ".attrs") {
      for(name in names(item$.attrs)) {
        xmlAttrs(xml)[[name]] <- item$.attrs[[name]]
      }
    } else {
      xml <- append.xmlNode(xml, listToXml(item[[i]], names(item[i])))
    }
  }
  return(xml)
}

##--------------------------------------------------------------------------------------------------#
## EXTERNAL FUNCTIONS
##--------------------------------------------------------------------------------------------------#

##' Loads PEcAn settings file
##' 
##' This will load the PEcAn settings file in the following order,
##' merging their values and overriding any values that are specified
##' in a file later in the order
##' 
##' \enumerate{
##' \item {/etc/pecan.xml}{global file for all users}
##' \item {~/.pecan.xml}{settings for all projects for the user}
##' \item {PECAN_SETTINGS}{environment variable PECAN_SETTINGS pointing to a specific file}
##' \item {inputfile}{passed as argument to function}
##' \item {--settings <file>}{passed as command line argument using --settings}
##' }
##' @param inputfile the PEcAn settings file to be merged with the others.
##' @param outputfile the name of file to which the settings will be
##'        written inside the outputdir.
##' @return list of all settings as loaded from the XML file(s)
##' @export
##' @import XML
##' @author Shawn Serbin
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' settings <- read.settings()
##' settings <- read.settings(file="willowcreek.xml")
##' test.settings.file <- system.file("tests/test.xml", package = "PEcAn.all")
##' settings <- read.settings(test.settings.file)
##' }
read.settings <- function(inputfile=NULL, outputfile="pecan.xml"){
  xml <- NULL
  
  ## 1 load /etc/pecan.xml
  if (file.exists("/etc/pecan.xml")) {
    xml <- xmlMerge(xml, xmlParse("/etc/pecan.xml"))
  }
  
  ## 2 merge ~/.pecan.xml
  if (file.exists("~/.pecan.xml")) {
    xml <- xmlMerge(xml, xmlParse("~/.pecan.xml"))
  }
  
  ## 3 merge pecan.xml
  if (file.exists("pecan.xml")) {
    xml <- xmlMerge(xml, xmlParse("pecan.xml"))
  }
  
  ## 4 merge PECAN_SETTINGS
  if (file.exists(Sys.getenv("PECAN_SETTINGS"))) {
    xml <- xmlMerge(xml, xmlParse(Sys.getenv("PECAN_SETTINGS")))
  }
  
  ## 5 merge file
  if(!is.null(inputfile) | class(inputfile) == "character"){
    if(any(c(file.exists(inputfile), grepl("<pecan>", inputfile)))){ 
      xml <- xmlMerge(xml, xmlParse(inputfile))
    }
  }
  ## 6 merge command line arguments
  loc <- which(commandArgs() == "--settings")
  if (length(loc) != 0) {
    for(idx in loc) {
      if (!is.null(commandArgs()[idx+1]) && file.exists(commandArgs()[idx+1])) {
        xml <- xmlMerge(xml, xmlParse(commandArgs()[idx+1]))
      }
    }
  }
  
  ## make sure something was loaded
  if (is.null(xml)) {
    logger.error("Did not find any settings file to load.")
    stop("Did not find any settings file to load.")
  }
  
  ## convert the xml to a list for ease and return
  settings <<- xmlToList(xml)

  ## do a sanity check
  check.settings(settings)
  
  ## crate the outputfolder
  if (is.null(settings$outdir)) {
    settings$outdir <- tempdir()
    logger.warn("No output folder specified, using", settings$outdir)
  } else {
    logger.debug("output folder =", settings$outdir)
  }
  if (!file.exists(settings$outdir) && !dir.create(settings$outdir, recursive=TRUE)) {
    logger.error("Could not create folder", settings$outdir)
    stop("Could not create out folder.")
  }
  
  ## create the model configuration folder
  if(!is.null(settings$run$host$name) && (settings$run$host$name == "localhost")) {
    if (is.null(settings$run$host$rundir)) {
      if (is.null(settings$rundir)) {
        settings$rundir <- file.path(settings$outdir, "run")
      }
      settings$run$host$rundir <- settings$rundir
      logger.info("No output folder for model configuration using", settings$run$host$rundir)
    } else {
      if (is.null(settings$rundir)) {
        settings$rundir <- settings$run$host$rundir
      }
      logger.debug("model configuration folder =", settings$run$host$rundir)
    }
    if (settings$rundir != settings$run$host$rundir) {
      settings$rundir <- settings$run$host$rundir
      logger.warn("rundir does not match run$host$rundir, setting to", settings$rundir)
    }
    if (!file.exists(settings$run$host$rundir) && !dir.create(settings$run$host$rundir, recursive=TRUE)) {
      logger.error("Could not create folder", settings$run$host$rundir)
      stop("Could not create model configuration folder.")
    }

    ## create the model output folder
    if (is.null(settings$run$host$outdir)) {
      if (is.null(settings$modeloutdir)) {
        settings$modeloutdir <- file.path(settings$outdir, "out")
      }
      settings$run$host$outdir <- settings$modeloutdir
      logger.info("No output folder for model runs using", settings$run$host$outdir)
    } else {
      if (is.null(settings$modeloutdir)) {
        settings$modeloutdir <- settings$run$host$outdir
      }
      logger.debug("model output folder =", settings$run$host$outdir)
    }
    if (settings$modeloutdir != settings$run$host$outdir) {
      settings$modeloutdir <- settings$run$host$outdir
      logger.warn("modeloutdir does not match run$host$outdir, setting to", settings$modeloutdir)
    }
    if (settings$modeloutdir != settings$run$host$outdir) {
      settings$modeloutdir <- settings$run$host$outdir
      logger.warn("modeloutdir does not match run$host$outdir, setting to", settings$modeloutdir)
    }
    if (!file.exists(settings$run$host$outdir) && !dir.create(settings$run$host$outdir, recursive=TRUE)) {
      logger.error("Could not create folder", settings$run$host$outdir)
      stop("Could not create model output folder.")
    }

  } else {
    ## create the run folder to store run configurations
    if (is.null(settings$rundir)) {
      settings$rundir <- file.path(settings$outdir, "run")
      logger.warn("No run folder specified, using", settings$rundir)
    } else {
      logger.debug("run folder =", settings$rundir)
    }
    if (!file.exists(settings$rundir) && !dir.create(settings$rundir, recursive=TRUE)) {
      logger.error("Could not create folder", settings$rundir)
      stop("Could not create run folder.")
    }

    ## create the model output folder to store run outputs
    if (is.null(settings$modeloutdir)) {
      settings$modeloutdir <- file.path(settings$outdir, "out")
      logger.warn("No modeloutdir folder specified, using", settings$modeloutdir)
    } else {
      logger.debug("modeloutdir folder =", settings$modeloutdir)
    }
    if (!file.exists(settings$modeloutdir) && !dir.create(settings$modeloutdir, recursive=TRUE)) {
      logger.error("Could not create folder", settings$modeloutdir)
      stop("Could not create modeloutdir folder.")
    }
  }

  ## create the PFT folders
  for (i in 1:sum(names(unlist(settings$pfts)) == "pft.name")) {
    if (is.null(settings$pfts[i]$pft$outdir)) {
      settings$pfts[i]$pft$outdir <- file.path(settings$outdir, "pft", settings$pfts[i]$pft$name)
      logger.info("No output folder specified for", settings$pfts[i]$pft$name, "will use", settings$pfts[i]$pft$outdir);
    }
    out.dir <- settings$pfts[i]$pft$outdir
    logger.debug("Storing pft", settings$pfts[i]$pft$name, "in", out.dir)
    if (!file.exists(out.dir) && !dir.create(out.dir, recursive=TRUE)) {
      logger.error("Could not create folder", out.dir)
      stop("Could not create pft folders.")
    }
  }
  
  ## create the workflow
  if (!'workflow' %in% names(settings)) {
    con <- try(query.base.con(settings), silent=TRUE)
    if(is.null(settings$model$id)) {
      settings$model$id <- -999
    }
    if(!is.character(con)){
      query.base(paste("INSERT INTO workflows (site_id, model_id, hostname, start_date, end_date, started_at, created_at, folder) values ('",
                       settings$run$site$id, "','", settings$model$id, "', '", settings$run$host$name, "', '",
                       settings$run$start.date, "', '", settings$run$end.date, "', NOW(), NOW(), '", dirname(settings$outdir), "')", sep=''), con)
      settings$workflow$id = query.base(paste("SELECT LAST_INSERT_ID() AS ID"), con)[['ID']]
      dbDisconnect(con)
    }
  }

  ## save the merged pecan.xml
  if (is.null(outputfile)) {
    outputfile="pecan.xml"
  }
  output <- file.path(settings$outdir, outputfile)
  if (file.exists(output)) {
    logger.warn(paste("File already exists [", output, "] file will be overwritten"))
  } 
  saveXML(listToXml(settings, "pecan"), file=output)
  
  ## setup Rlib from settings
  if(!is.null(settings$Rlib)){ 
    .libPaths(settings$Rlib)
  }
  
  ## Return settings file as a list
  invisible(settings)
}
##=================================================================================================#

####################################################################################################
### EOF.  End of R script file.  						
####################################################################################################
