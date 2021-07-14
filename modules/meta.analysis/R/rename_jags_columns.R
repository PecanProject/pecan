#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-----------------------------------------------------------------------------#
##' renames the variables within output data frame trait.data
##'
##' @param data data frame to with variables to rename
##'
##' @seealso used with \code{\link[PEcAn.MA]{jagify}};
##' @export
##' @author David LeBauer
rename_jags_columns <- function(data) {
  
  # Change variable names and calculate obs.prec within data frame
  # Swap column names; needed for downstream function pecan.ma()
  colnames(data)[colnames(data) %in% c("greenhouse", "ghs")] <- c("ghs", "greenhouse")
  colnames(data)[colnames(data) %in% c("site_id", "site")] <- c("site", "site_id")

  stat <- NULL
  n <- NULL
  trt_id <- NULL
  citation_id <- NULL
  transformed <-  transform(data,
                            Y        = mean,
                            se       = stat,
                            obs.prec = 1 / (sqrt(n) * stat) ^2,
                            trt      = trt_id,
                            cite     = citation_id)
  
  # Subset data frame
  selected <- subset(transformed, select = c('Y', 'n', 'site', 'trt', 'ghs', 'obs.prec',
                                             'se', 'cite',
                                             "greenhouse", "site_id", "treatment_id", "trt_name", "trt_num")) # add original # original versions of greenhouse, site_id, treatment_id, trt_name
  # Return subset data frame
  return(selected)
}
##=============================================================================#
