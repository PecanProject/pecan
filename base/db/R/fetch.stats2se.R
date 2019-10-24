#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##--------------------------------------------------------------------------------------------------#
##' Queries data from the trait database and transforms statistics to SE
##'
##' Performs query and then uses \code{transformstats} to convert miscellaneous statistical summaries
##' to SE
##' @name fetch.stats2se
##' @title Fetch data and transform stats to SE
##' @param connection connection to trait database
##' @param query to send to databse
##' @return dataframe with trait data
##' @seealso used in \code{\link{query.trait.data}}; \code{\link{transformstats}} performs transformation calculations
##' @author <unknown>
fetch.stats2se <- function(connection, query){
  transformed <- PEcAn.utils::transformstats(db.query(query = query, con = connection))
  return(transformed)
}