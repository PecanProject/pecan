#' DataONE download 
#'
#' @param id "The identifier of a package, package metadata or other package member" -- dataone r
#' @param filepath path to where files will be stored
#' @param CNode 
#' @param lazyLoad "A logical value. If TRUE, then only package member system metadata is downloaded and not data. The default is FALSE." -- dataone R 
#' @param quiet "A 'logical'. If TRUE (the default) then informational messages will not be printed." -- dataone R
#' 
#' @author Liam P Burke, \email{lpburke@@bu.edu}
#' @description Adapts the dataone::getDataPackage workflow to allow users to download data from the DataONE federation by simply entering the doi or associated package id 
#'
#' @export
#'

#' @examples 
#' /dontrun{
#' dataone_download(id = "doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87", filepath = "/fs/data1/pecan.data/dbfiles/")
#' }

dataone_download = function(id, filepath = "/fs/data1/pecan.data/dbfiles/", CNode = "PROD", lazyLoad = FALSE, quiet = F){ 
  ### automatically retrieve mnId
  cn <- dataone::CNode(CNode) 
  locations <- dataone::resolve(cn, pid = id) 
  mnId <- locations$data[1,"nodeIdentifier"] 
  
  ### begin D1 download process
  d1c <- dataone::D1Client("PROD", mnId)
  pkg <- dataone::getDataPackage(d1c, id = id, lazyLoad = lazyLoad, quiet = quiet, limit = "1GB") 
  files <- datapack::getValue(pkg, name="sysmeta@formatId")
  n <- length(files) # number of files

  # make new directory within this directory
  newdir <- file.path(filepath, paste0("DataOne_", gsub("/", "-", id)))
  dir.create(newdir)
  
  # extract the data
  for(i in 1:n){ 
    pkgMember <- datapack::getMember(pkg, names(files[i])) # extract data from dataPackage
    data <- datapack::getData(pkgMember) 
    
    if(files[i][1] == "text/csv"){ # format is stored as the first entry of the list "files"
      base::writeLines(rawToChar(data), paste0(newdir, "/", "Data", i, ".csv")) #  If csv, add .csv 
      
    } else if(files[i][1] == "text/xml"){
      base::writeLines(rawToChar(data), paste0(newdir, "/", "MetaData", i, ".xml")) # If XML add .xml and add Metadata title
      
    } else 
      base::writeLines(rawToChar(data), paste0(newdir, "/", "MetaData", i)) # If other, process without extension (typically eml)
  }
 
}



