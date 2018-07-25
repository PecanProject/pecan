#' Insert Format and Format-Variable Records 
#'
#' @param con 
#' @param format_name 
#' @param header 
#' @param skip 
#' @param mimetype_id 
#' @param format_notes 
#' @param format_variables_df 
#'
#' @return
#' @export
#'
#' @examples
insert.format.vars <- function(con, format_name, header, skip, mimetype_id, format_notes = "", format_variables_df = NULL){
  if(is.null(format_variables_df)){
    ## Just make the format record
    
    
    
    
  }else{
    ## Make the format record and the formatvariable record 
    
    
  }
}