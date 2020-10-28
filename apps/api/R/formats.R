library(dplyr)

#' Retrieve the details of a PEcAn format, based on format_id
#' @param format_id Format ID (character)
#' @param dbcon Database connection object. Default is global database pool.
#' @return Format details
#' @author Tezan Sahu
#* @get /<format_id>
getFormat <- function(format_id, res, dbcon = global_db_pool){
  
  Format <- tbl(dbcon, "formats") %>%
    select(format_id = id, name, notes, header, mimetype_id) %>%
    filter(format_id == !!format_id)
  
  Format <- tbl(dbcon, "mimetypes") %>%
    select(mimetype_id = id, mimetype = type_string) %>%
    inner_join(Format, by = "mimetype_id") %>%
    select(-mimetype_id)
  
  qry_res <- Format %>% collect()
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Format not found"))
  }
  else {
    # Convert the response from tibble to list
    response <- list()
    for(colname in colnames(qry_res)){
      response[colname] <- qry_res[colname]
    }
    
    format_vars <- tbl(dbcon, "formats_variables") %>%
      select(name, unit, format_id, variable_id) %>%
      filter(format_id == !!format_id)
    format_vars <- tbl(dbcon, "variables") %>%
      select(variable_id = id, description, units) %>%
      inner_join(format_vars, by="variable_id") %>%
      mutate(unit = ifelse(unit %in% "", units, unit)) %>%
      select(-variable_id, -format_id, -units) %>%
      collect()
    
    response$format_variables <- format_vars
    return(response)
  }
}

#########################################################################

#' Search for PEcAn format(s) containing wildcards for filtering
#' @param format_name Format name search string (character)
#' @param mimetype Mime type search string (character)
#' @param ignore_case Logical. If `TRUE` (default) use case-insensitive search otherwise, use case-sensitive search
#' @param dbcon Database connection object. Default is global database pool.
#' @return Formats subset matching the model search string
#' @author Tezan Sahu
#* @get /
searchFormats <- function(format_name="", mimetype="", ignore_case=TRUE, res,
                          dbcon = global_db_pool){
  format_name <- URLdecode(format_name)
  mimetype <- URLdecode(mimetype)
  
  Formats <- tbl(dbcon, "formats") %>%
    select(format_id = id, format_name=name, mimetype_id) %>%
    filter(grepl(!!format_name, format_name, ignore.case=ignore_case))
  
  Formats <- tbl(dbcon, "mimetypes") %>%
    select(mimetype_id = id, mimetype = type_string) %>%
    inner_join(Formats, by = "mimetype_id") %>%
    filter(grepl(!!mimetype, mimetype, ignore.case=ignore_case)) %>%
    select(-mimetype_id) %>%
    arrange(format_id)
  
  qry_res <- Formats %>% collect()
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Format(s) not found"))
  }
  else {
    return(list(formats=qry_res, count = nrow(qry_res)))
  }
}
