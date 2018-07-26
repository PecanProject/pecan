#' Insert Format and Format-Variable Records 
#'
#' @param con Bety connection object
#' @param format_notes Character string used to 
#' @param format_name 
#' @param header 
#' @param skip 
#' @param mimetype_id 
#' @param formats_variables_df A 'data.frame' consisting of entries that correspond to columns in the formats-variables table
#'
#' @author Liam Burke (liam.burke24@gmail.com)
#' @return format id
#' @export
#' @examples
#' \dontrun{
#' bety <- betyConnect
#'   insert.format.vars(con = bety$con, formats_df, formats_variables_df)
#' }
insert.format.vars <- function(con, format_notes, format_name, header, skip, mimetype_id, formats_variables_df = NULL){
  
    formats_df <- data.frame(
      header = header,
      skip = skip,
      mimetype_id = mimetype_id,
      notes = format_notes,
      name = format_name
    )

    ## Insert format record
    inserted_formats <- db_merge_into(formats_df, "formats", con = con, by = c("name", "mimetype_id")) ## Make sure to include a 'by' argument
      inserted_formats
    
  if(!is.null(formats_variables_df)){
    ## Insert Format-Variable record
    inserted_formats_variables <- db_merge_into(formats_variables_df, "formats_variables", con = con,  by = c("variable_id", "name"))
      return(inserted_formats, inserted_formats_variables)
  }
    
}

### For Testing###
bety <- betyConnect()
con = bety$con
format_notes = "Testing 1,2,3,4,5,6"
format_name = "New Name 1235678"
header = as.character(FALSE)
skip = ""
mimetype_id = 1090
#######