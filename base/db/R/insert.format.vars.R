#' Insert Format and Format-Variable Records 
#'
#' @param con SQL connection to BETYdb
#' @param name The name of the format. Type: character string.
#' @param notes Additional description of the format: character string.
#' @param header Boolean that indicates the presence of a header in the format. Defaults to "TRUE".
#' @param skip Integer that indicates the number of lines to skip in the header. Defaults to 0. 
#' @param mimetype_id The id associated with the mimetype of the format. Type: integer. 
#' @param formats_variables_df A 'data.frame' consisting of entries that correspond to columns in the formats-variables table. See Details for further information. 
#' @details It is very important that the formats_variables_df 'data.frame' be structured in a specific format so that the SQL query functions properly. First, all arguments should be passed as vectors so that each entry will correspond with a specific row.
#' \describe{
#' \item{variable_id}{(Required) Vector of integers.}
#' \item{name}{(Optional) Vector of character strings. Need only be specified if the} 
#' \item{unit}{(Optional) Vector of character strings. }
#' \item{storage_type}{(Optional) Vector of character strings}
#' \item{column_number}{(Optional) Vector of integers that determines the} 
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
insert.format.vars <- function(con, name, mimetype_id, notes = NULL, header = TRUE, skip = 0, formats_variables_df = NULL){
 
   #Test if skip is an integer
  if(!is.integer(skip)){
  PEcAn.logger::logger.error(
    "Skip must be an Integer"
  )}
  
  #Test if header is a Boolean
  if(!rapportools::is.boolean("ghe")){
    PEcAn.logger::logger.error(
      "Header must be a Boolean"
  )}
  
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
    format_id_df <- matrix(data = format_id, nrow = n, ncol = 1)
    colnames(format_id_df) <- "format_id"
    
    ## Make query data.frame
    formats_variables_input <- cbind(format_id_df, formats_variables_df) 
    
    ## Insert Format-Variable record
    inserted_formats_variables <- db_merge_into(formats_variables_input, "formats_variables", con = con,  by = c("variable_id", "name"))
  }
    return(format_id)
    
}
