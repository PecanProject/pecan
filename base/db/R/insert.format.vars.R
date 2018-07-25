#' Insert Format and Format-Variable Records 
#'
#' @param con Bety connection object
#' @param formats_variables_df A 'data.frame' consisting of entries that correspond to columns in the formats-variables table
#' @param formats_df A 'data.frame' consisting of entries that correspond to columns in the formats table
#' @author Liam Burke (liam.burke24@gmail.com)
#'
#' @return Data frame: Inner join of SQL table and input data frame (as unevaluated "lazy query" table) 
#' @export
#'
#' @examples
#' \dontrun{
#' bety <- betyConnect
#'   insert.format.vars(con = bety$con, formats_df, formats_variables_df)
#' }
#' 
#' 
insert.format.vars <- function(con, formats_df, formats_variables_df = NULL){
  if(is.null(formats_variables_df)){
    ## Only insert format record
    inserted_formats <- db_merge_into(formats_df, "formats", con = con, by = c("name", "mimetype_id")) ## Make sure to include a 'by' argument
      return(inserted_formats)
    
  }else{
    ## Insert format record 
    inserted_formats <- db_merge_into(formats_df, "formats", con = con,  by = c("name", "mimetype_id")) 
    
    ## Insert Format-Variable record
    inserted_formats_variables <- db_merge_into(formats_variables_df, "formats_variables", con = con,  by = c("variable_id", "name"))
      return(inserted_formats, inserted_formats_variables)
  }
    
}