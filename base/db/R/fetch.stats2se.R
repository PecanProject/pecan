##' Queries data from the trait database and transforms statistics to SE
##'
##' Performs query and then uses \code{transformstats} to convert miscellaneous statistical summaries
##' to SE
##' @param connection connection to trait database
##' @param query to send to databse
##' @return dataframe with trait data
##' @seealso used in \code{\link{query.trait.data}};
##'   \code{\link[PEcAn.utils]{transformstats}} performs transformation calculations
##' @author <unknown>
fetch.stats2se <- function(connection, query) {
  transformed <- PEcAn.utils::transformstats(db.query(query = query, con = connection))
  return(transformed)
}
