##' Function to save intermediate rds file
##' 
##' @name write_veg
##' @title write_veg
##' @export
write_veg <- function(outfolder, start_date, veg_info, source){
  
  start_year    <- lubridate::year(start_date)
  out_file      <- paste(source, start_year, "veg", "rds", sep = ".")
  out_file_full <- file.path(outfolder, out_file)
  
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  
  saveRDS(veg_info, file = out_file_full)
  
  return(out_file_full)
  
} # write_veg


##' Remove dead trees
##' need to handle at least two cases: i) remove by code ii) remove by mortality status
##' 
##' @name remove_dead_trees
##' @title remove_dead_trees
##' @param tree.info dataframe 
##' @param look.col 
##' @export
##' @author Istem Fer
remove_dead_trees <- function(tree.info, look.col, by.code = TRUE){
  
  
  # TODO: needs developing
  
  if(by.code){
    # remove by code
    dead_tree_codes <- c("2TB", "SNAG", "DEAD")
    fobs <- tree.info[!(tree.info[[look.col]] %in% dead_tree_codes), ]
  }else{
    # remove by mortality status?
  }
  
  return(fobs)
  
} # remove_dead_trees