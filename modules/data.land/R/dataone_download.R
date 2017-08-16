#' DataONE download 
#'
#' @param id "The identifier of a package, package metadata or other package member" -- dataone r
#' @param username used to create a user-specific destdir
#' @param CNode 
#' @param lazyLoad "A logical value. If TRUE, then only package member system metadata is downloaded and not data. The default is FALSE." -- dataone R 
#' @param quiet "A 'logical'. If TRUE (the default) then informational messages will not be printed." -- dataone R
#' 
#' @author Liam P Burke, \email{lpburke@@bu.edu}
#' @description Adapts the dataone::getDataPackage workflow to allow users to download data from the DataONE federation by simply entering the doi or associated package id 
#'
#' @export
#'
#' @examples doi_download(id = "doi:10.6073/pasta/63ad7159306bc031520f09b2faefcf87", username = "Guest")

dataone_download = function(id, username, CNode = "PROD", lazyLoad = FALSE, quiet = F){ 
  ### automatically retrieve mnId
  cn <- dataone::CNode(CNode) 
  locations <- dataone::resolve(cn, pid = id) 
  mnId <- locations$data[1,"nodeIdentifier"] 
  
  ### begin D1 download process
  d1c <- dataone::D1Client("PROD", mnId)
  pkg <- dataone::getDataPackage(d1c, id = id, lazyLoad = lazyLoad, quiet = quiet, limit = "1MB") # what is the standard limit for pecan downloads?
  files <- datapack::getValue(pkg, name="sysmeta@formatId")
  n <- length(files) # number of files
  
  # fileath to /dbfiles 
  fp <- "/fs/data1/pecan.data/dbfiles/"
  
  # make new directory within this directory
  newdir <- paste(fp, "NewData_", username, sep = "")
  system(paste("mkdir", newdir))
  
  # switch to new directory -- unsure if I should do this in R or in unix
  # system(paste("cd", fp, sep = " ")) 
  setwd(newdir)
  
  for(i in 1:n){
    rename <- paste("File", i, Sys.time(), sep = "_") # new file name
    system(paste("wget", "-O", rename, names(files)[i])) # download files with wget
  }
  system("ls") # checks that files were downloaded to 
  
  # Naming could still be improved to include part of title or URL
  }
}