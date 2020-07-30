library(dplyr)

#' Retrieve the details of a PEcAn site, based on site_id
#' @param site_id Site ID (character)
#' @return Site details
#' @author Tezan Sahu
#* @get /<site_id>
getSite <- function(site_id, res){
  
  dbcon <- PEcAn.DB::betyConnect()
  
  site <- tbl(dbcon, "sites") %>%
    select(-created_at, -updated_at, -user_id, -geometry) %>%
    filter(id == !!site_id)
  
  
  qry_res <- site %>% collect()
  
  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Site not found"))
  }
  else {
    # Convert the response from tibble to list
    response <- list()
    for(colname in colnames(qry_res)){
      response[colname] <- qry_res[colname]
    }
    return(response)
  }
}

#################################################################################################

#' Search for PEcAn sites containing wildcards for filtering
#' @param sitename Site name search string (character)
#' @param ignore_case Logical. If `TRUE` (default) use case-insensitive search otherwise, use case-sensitive search
#' @return Site subset matching the site search string
#' @author Tezan Sahu
#* @get /
searchSite <- function(sitename="", ignore_case=TRUE, res){
  dbcon <- PEcAn.DB::betyConnect()
  
  sites <- tbl(dbcon, "sites") %>%
    select(id, sitename) %>%
    filter(grepl(!!sitename, sitename, ignore.case=ignore_case)) %>%
    arrange(id)
  
  
  qry_res <- sites %>% collect()
  
  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Site(s) not found"))
  }
  else {
    return(list(sites=qry_res, count = nrow(qry_res)))
  }
}