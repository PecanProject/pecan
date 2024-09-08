##' Function to query yields data from database for specific species and convert stat to SE
##'
##' @name query.yields
##' @title Query yield data and transform stats to SE by calling \code{\link{fetch.stats2se}};
##' @param trait yield trait to query
##' @param spstr species to query for yield data
##' @param extra.columns other query terms to pass in. Optional
##' @param con database connection
##' @param ids_are_cultivars if TRUE, spstr contains cultivar IDs, otherwise they are species IDs
##' @param ... extra arguments
##' @seealso used in \code{\link{query.trait.data}}; \code{\link{fetch.stats2se}}; \code{\link{transformstats}} performs transformation calculations
##' @author <unknown>
query.yields <- function(trait = 'yield', spstr, extra.columns = '', con = NULL,
                         ids_are_cultivars = FALSE, ...){
  
  member_column <- if (ids_are_cultivars) {"cultivar_id"} else {"specie_id"}
  
  if(!is.null(extra.columns)) {
    if(!is.character(extra.columns) || length(extra.columns) != 1) {
      PEcAn.logger::logger.severe("`extra.columns` must be a string")
    }
  }

  query <- paste("select
            yields.id, yields.citation_id, yields.site_id, treatments.name,
            yields.date, yields.time, yields.cultivar_id, yields.specie_id,
            yields.mean, yields.statname, yields.stat, yields.n,
            variables.name as vname,
            month(yields.date) as month,",
                if(extra.columns != '') { paste(extra.columns, ",", sep = "") } else {""},
                 "treatments.control, sites.greenhouse
          from yields
            left join treatments on  (yields.treatment_id = treatments.id)
            left join sites on (yields.site_id = sites.id)
            left join variables on (yields.variable_id = variables.id)
          where ", member_column, " in (", spstr,");", sep = "")
  if(!trait == 'yield'){
    query <- gsub(";", paste(" and variables.name in ('", trait,"');", sep = ""), query)
  }
  
  return(fetch.stats2se(connection = con, query = query))
}