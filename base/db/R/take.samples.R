#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-----------------------------------------------------------------------------#
##' sample from normal distribution, given summary stats
##'
##' @name take.samples
##' @title Sample from normal distribution, given summary stats
##' @param summary data.frame with values of mean and sd
##' @param sample.size number of samples to take
##' @return sample of length sample.size
##' @author David LeBauer, Carl Davidson
##' @export
##' @examples
##' ## return the mean when stat = NA
##' take.samples(summary = data.frame(mean = 10, stat = NA))
##' ## return vector of length \code{sample.size} from N(mean,stat)
##' take.samples(summary = data.frame(mean = 10, stat = 10), sample.size = 10)
##'
take.samples <- function(summary, sample.size = 10^6){
  if(is.na(summary$stat)){
    ans <- summary$mean
  } else {
    set.seed(0)
    ans <- stats::rnorm(n = sample.size, mean = summary$mean, sd = summary$stat)
  }
  return(ans)
}
##=============================================================================#
