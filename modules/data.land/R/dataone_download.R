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
#' \dontrun{
#' dataone_download(id = "doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87", filepath = "/fs/data1/pecan.data/dbfiles/")
#' }

dataone_download = function(id, filepath = "/fs/data1/pecan.data/dbfiles/", CNode = "PROD", lazyLoad = FALSE, quiet = F){ 
  ### Check for wget functionality
  test <- try(system2("wget", "--version", stderr = TRUE))
  if (class(test) == "try-error") {
    PEcAn.logger::logger.severe("wget system utility is not available on this system. Please install it to use this functionality.")
  }

  ### automatically retrieve mnId
  cn <- dataone::CNode(CNode) 
  locations <- dataone::resolve(cn, pid = id) 
  mnId <- locations$data[1,"nodeIdentifier"] 
  
  ### begin D1 download process
  d1c <- dataone::D1Client("PROD", mnId)
  pkg <- dataone::getDataPackage(d1c, id = id, lazyLoad = lazyLoad, quiet = quiet, limit = "1GB") 
  files <- datapack::getValue(pkg, name="sysmeta@formatId")
  n <- length(files) # number of files

  ### make new directory within this directory
  newdir <- file.path(filepath, paste0("DataOne_", gsub("/", "-", id)))
  dir.create(newdir)
  
  ### download the data with wget 
  # '--header='  spoofs the user agent so that we avoid authentication errors. DataONE is now actively preventing web scraping. 
  for(i in 1:n){
    system(paste("cd", newdir, "&&", "{", "wget",  "--header='User-Agent: Mozilla/5.0 (Windows NT 5.1; rv:23.0) Gecko/20100101 Firefox/23.0'", "--content-disposition", names(files)[i], "; cd -; }")) # cd to newdir, download files with wget, cd back
  }
 
}



