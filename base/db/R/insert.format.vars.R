#' Insert Format and Format-Variable Records 
#'
#' @param con SQL connection to BETYdb
#' @param format_notes Character string that describes format.
#' @param format_name Character string 
#' @param header Boolean that indicates the presence of a header in the format 
#' @param skip Integer that indicates the number of lines to skip in the header. Defaults to "" 
#' @param mimetype_id Integer 
#' @param formats_variables_df A 'data.frame' consisting of entries that correspond to columns in the formats-variables table
#' @details It is very important that the formats_variables_df 'data.frame' be structured in a specific format so that the SQL query functions properly. First, all arguments should be passed as vectors so that each entry will correspond with a specific row.
#' Variables: 
#' \enumerate{
#' \item variable_id: vector of integers
#' \item name: vector of character strings
#' \item unit: vector of character strings
#' \item storage_type: vector of character strings
#' \item column_number: vector of integers
#' }
#' @author Liam Burke (liam.burke24@gmail.com)
#' @return format_id
#' @export
#' @examples
#' \dontrun{
#' bety <- betyConnect
#' 
#' formats_variables_df <- tibble::tibble(
#'        variable_id = c(327, 2.98e+02), # integer 
#'        name = c("New Name", "Other New Name"), # 
#'        unit = c("Yoonits", "Other Units"),
#'        storage_type = c("%j", "%Y"),
#'        column_number = c(as.integer(22), as.integer(24)),
#'  )
#'   insert.format.vars(con = bety$con, format_notes = "Info about format", format_name = "New Format", header = TRUE, skip = 2, mimetype_id = 1090, formats_variables_df)
#' }
insert.format.vars <- function(con, format_notes, format_name, header, skip = "", mimetype_id, formats_variables_df = NULL){
  
    formats_df <- tibble::tibble(
      header = as.character(header),
      skip = skip,
      mimetype_id = mimetype_id,
      notes = format_notes,
      name = format_name,
      stringsAsFactors = FALSE
    )

    ## Insert format record
    inserted_formats <- db_merge_into(formats_df, "formats", con = con, by = c("name", "mimetype_id")) ## Make sure to include a 'by' argument
    format_id <- dplyr::pull(inserted_formats, id)
    
  if(!is.null(formats_variables_df)){
    ## Insert format_id into 
    n <- nrow(formats_variables_df)
    format_id_df <- matrix(data = NA, nrow = n, ncol = 1)
    for(i in 1:n){
      format_id_df[i,1] <- format_id
    }
    colnames(format_id_df) <- "format_id"
    
    ## Make query data.frame
    formats_variables_input <- cbind(format_id_df, formats_variables_df) 
    
    ## Insert Format-Variable record
    inserted_formats_variables <- db_merge_into(formats_variables_input, "formats_variables", con = con,  by = c("variable_id", "name"))
  }
    return(format_id)
    
}