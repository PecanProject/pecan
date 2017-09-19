##' @name partition_roots
##' @title partition_roots
##' @description Given a vector of root size thresholds (lower bound of each) and a vector of corresponding root carbon values, partition_roots checks if the input can be partitioned along the .002 m threshold between fine and coarse roots and returns a list containing the summed values for fine and coarse. If there are fewer than two thresholds or none within .0005 m of .002 m, returns NULL. Meant to be used in conjunction with standard variable root_carbon_content with rtsize dimension, extracted from netcdf.
##' @export
##'
##' @param roots vector of root carbon values in kg C m-2
##' @param rtsize vector of lower bounds of root size class thresholds in m, length greater than one and equal to roots. Must contain threshold within .0005 m of .002 m
##' @return list containing summed fine root and coarse root carbon (2 values)
##' @author Anne Thomas
##' 
partition_roots <- function(roots, rtsize){
  if(length(rtsize) > 1 && length(rtsize) == length(roots)){
    threshold <- .002
    epsilon <- .0005
    #find index of threshold in rtsize closest to .002
    rtsize_thresh_idx <- which.min(sapply(rtsize-threshold,abs)) 
    rtsize_thresh <- rtsize[rtsize_thresh_idx]
    if(abs(rtsize_thresh-threshold) > epsilon){
      PEcAn.logger::logger.error(paste("Closest rtsize to fine root threshold of", threshold, "m (", rtsize_thresh, 
                                      ") is greater than", epsilon, 
                                      "m off; fine roots can't be partitioned. Please improve rtsize dimensions."))
      return(NULL)
    } else{
      #sum fine roots from lowest group through group below threshold and coarse from group including threshold to the highest
      fine.roots <- sum(roots[1:rtsize_thresh_idx-1])
      coarse.roots <- sum(roots) - fine.roots
      if(fine.roots >= 0 && coarse.roots >= 0){
        PEcAn.logger::logger.info("Using partitioned root values", fine.roots, "for fine and", coarse.roots, "for coarse.")
        return(list(fine.roots = fine.roots, coarse.roots = coarse.roots))
      } else{
        PEcAn.logger::logger.error("Roots could not be partitioned (fine or coarse is less than 0).")
        return(NULL)
      }
    }
  } else {
    PEcAn.logger::logger.error("Inadequate or incorrect number of levels of rtsize associated with roots; please ensure roots and rtsize lengths match and are greater than 1.")
    return(NULL)
  }
}