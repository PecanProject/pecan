##' Function to query data from database for specific species and convert stat to SE
##'
##' @name query.data
##' @title Query data and transform stats to SE by calling \code{\link{fetch.stats2se}};
##' @param trait trait to query from the database
##' @param spstr IDs of species to query from, as a single comma-separated string
##' @param con database connection
##' @param extra.columns other query terms to pass in. If unspecified, retrieves latitude and longitude
##' @param ids_are_cultivars if TRUE, ids is a vector of cultivar IDs, otherwise they are species IDs
##' @param ... extra arguments
##' @param store.unconverted determines whether or not a copy of the mean and stat fields are returned with _unconverted appended to the column names
##' @seealso used in \code{\link{query.trait.data}}; \code{\link{fetch.stats2se}}; \code{\link{transformstats}} performs transformation calculations
##' @author David LeBauer, Carl Davidson
query.data <- function(
              trait,
              spstr,
              con,
              extra.columns = paste(
                "ST_X(ST_CENTROID(sites.geometry)) AS lon,",
                "ST_Y(ST_CENTROID(sites.geometry)) AS lat, "),
              store.unconverted = FALSE,
              ids_are_cultivars = FALSE,
              ...) {
  id_type <- if (ids_are_cultivars) {"cultivar_id"} else {"specie_id"}

  query <- paste("select
              traits.id, traits.citation_id, traits.site_id, traits.treatment_id,
              treatments.name, traits.date, traits.time, traits.cultivar_id, traits.specie_id,
              traits.mean, traits.statname, traits.stat, traits.n, variables.name as vname,
              extract(month from traits.date) as month,",
                 extra.columns,
                 "treatments.control, sites.greenhouse
              from traits
              left join treatments on  (traits.treatment_id = treatments.id)
              left join sites on (traits.site_id = sites.id)
              left join variables on (traits.variable_id = variables.id)
            where ", id_type, " in (", spstr,")
            and variables.name in ('", trait,"');", sep = "")
  result <- fetch.stats2se(connection = con, query = query)
  
  if(store.unconverted) {
    result$mean_unconverted <- result$mean
    result$stat_unconverted <- result$stat
  }
  
  return(result)
}