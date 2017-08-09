#' DataONE download 
#'
#' @param id "The identifier of a package, package metadata or other package member" -- dataone r
#' @param destdir Name the file that will be created to store the data.
#' @param CNode 
#' @param lazyLoad "A logical value. If TRUE, then only package member system metadata is downloaded and not data. The default is FALSE." -- dataone R 
#' @param quiet "A 'logical'. If TRUE (the default) then informational messages will not be printed." -- dataone R
#' 
#' @author Liam P Burke, \email{lpburke@@bu.edu}
#' @description Adapts the dataone::getDataPackage workflow to allow users to download data from the DataONE federation by simply entering the doi or associated package id 
#'
#' @export
#'
#' @examples doi_download(id = doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87, destdir = LTER)
doi_download = function(id, destdir = "MyDataFile", CNode = "PROD", lazyLoad = FALSE, quiet = F){ 
  ### automatically retrieve mnId
  cn <- dataone::CNode(CNode) 
  locations <- dataone::resolve(cn, pid = id) 
  mnId <- locations$data[1,"nodeIdentifier"] 
  
  ### begin D1 download process
  d1c <- dataone::D1Client("PROD", mnId)
  pkg <- dataone::getDataPackage(d1c, id = id, lazyLoad = lazyLoad, quiet = quiet, limit = "1MB") # what is the standard limit for pecan downloads?
  files <- datapack::getValue(pkg, name="sysmeta@formatId")
  n <- length(files) # number of files
  ### create a list containing a readable version of the formats
  formats <- list()
  # add more formats as we come across them
  for(i in 1:n){
    if(files[[i]] == "text/csv"){
      formats[i] <- ".csv"
      
    }else if(files[[i]] == "text/xml"){
      formats[i] <- ".xml"
      
    }else{
      formats[i] <- ".xml" # this is for the unknown type... Not sure if this is a universal fix... Please advise best practices here.
    } 
  }
  
  ### read data in the packets individually
  filenames <- names(files) # list of all files because they are stored as headers by default
  
  # filepath & create new directory with timestamp -- same for all files
  fp <- paste("~/downloads/data/", destdir, "_", Sys.time(), "/", sep = "") # what should the destination directory/ filepath be for pecan?
  dir.create(fp)
  
  
  for(i in 1:n){
    pkgMember <- datapack::getMember(pkg, filenames[i])
    data <- datapack::getData(pkgMember)
    base::writeLines(rawToChar(data), paste(fp, "file_", i, formats[[i]], sep = "")) # file naming is an issue... How to proceed?
  }
}