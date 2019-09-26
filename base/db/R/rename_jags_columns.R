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
  transformed <-  transform(data,
                            Y        = mean,
                            se       = stat,
                            obs.prec = 1 / (sqrt(n) * stat) ^2,
                            trt      = trt_id,
                            site     = site_id,
                            cite     = citation_id,
                            ghs      = greenhouse)
  
  # Subset data frame
  selected <- subset(transformed, select = c('Y', 'n', 'site', 'trt', 'ghs', 'obs.prec',
                                             'se', 'cite'))
  # Return subset data frame
  return(selected)
}
##=============================================================================#
