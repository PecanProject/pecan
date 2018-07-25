#' Insert Format and Format-Variable Records 
#'
#' @param con Bety connection object
#' @param formats_variables_df 
#' @param formats_df 
#'
#' @return
#' @export
#'
#' @examples
insert.format.vars <- function(con, formats_df, formats_variables_df = NULL){
  if(is.null(format_variables_df)){
    ## Just make the format record
    inserted_formats <- db_merge_into(formats_df, "formats", con = con) ## Make sure to include a 'by' argument
      return(inserted_formats)
    
  }else{
    ## Make the format record and the formatvariable record 
    inserted_formats <- db_merge_into(formats_df, "formats", con = con) ## Make sure to include a 'by' argument
    
    inserted_formats_variables <- db_merge_into(formats_variables_df, "formats_variables" )
  }
    return(inserted_formats, inserted_formats_variables)
}