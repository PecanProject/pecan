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

dataone_download = function(id, filepath = "/fs/data1/pecan.data/dbfiles/", CNode = "PROD", lazyLoad = FALSE, quiet = F){ 
  ### automatically retrieve mnId
  cn <- dataone::CNode(CNode) 
  locations <- dataone::resolve(cn, pid = id) 
  mnId <- locations$data[1,"nodeIdentifier"] 
  
  ### begin D1 download process
  d1c <- dataone::D1Client("PROD", mnId)
  pkg <- dataone::getDataPackage(d1c, id = id, lazyLoad = lazyLoad, quiet = quiet, limit = "1MB") # what is the standard limit for pecan downloads?
  files <- datapack::getValue(pkg, name="sysmeta@formatId")
  n <- length(files) # number of files

  # make new directory within this directory
  newdir <- file.path(filepath, paste0("DataOne_", gsub("/", "-", id)))
  dir.create(newdir)
  
  for(i in 1:n){
    rename <- paste(i, basename(names(files[i])), sep="_") # new file name
    system(paste("cd", newdir, "&&", "{", "wget", "-O", rename, names(files)[i], "; cd -; }")) # cd to newdir, download files with wget, cd back
  }
  list.files(newdir) # checks that files were downloaded to 
  
  # Naming could still be improved to include part of title 
  }
