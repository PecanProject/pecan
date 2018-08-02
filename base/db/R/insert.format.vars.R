#' Insert Format and Format-Variable Records 
#'
#' @param con SQL connection to BETYdb
#' @param format_name The name of the format. Type: character string.
#' @param mimetype_id The id associated with the mimetype of the format. Type: integer. 
#' @param header Boolean that indicates the presence of a header in the format. Defaults to "TRUE".
#' @param skip Integer that indicates the number of lines to skip in the header. Defaults to 0. 
#' @param formats_variables A 'tibble' consisting of entries that correspond to columns in the formats-variables table. See Details for further information. 
#' @param notes Additional description of the format: character string.
#' @param suppress Boolean that suppresses or allows a test for an existing variable id. This test is inconvenient in applications where the variable_ids are already known. 
#' @details The formats_variables argument must be a 'tibble' and be structured in a specific format so that the SQL query functions properly. All arguments should be passed as vectors so that each entry will correspond with a specific row. All empty values should be specified as NA.  
#' \describe{
#' \item{variable_id}{(Required) Vector of integers.}
#' \item{name}{(Optional) Vector of character strings. The variable name in the imported data need only be specified if it differs from the BETY variable name.} 
#' \item{unit}{(Optional) Vector of type character string. Should be in a format parseable by the udunits library and need only be secified if the units of the data in the file differ from the BETY standard.}
#' \item{storage_type}{(Optional) Vector of character strings. Storage type need only be specified if the variable is stored in a format other than would be expected (e.g. if numeric values are stored as quoted character strings). Additionally, storage_type stores POSIX codes that are used to store any time variables (e.g. a column with a 4-digit year would be \%Y). See also \code{[base::strptime]}}
#' \item{column_number}{Vector of integers that list the column numbers associated with variables in a dataset. Required for text files that lack headers.}}
#' @author Liam Burke (liam.burke24@gmail.com)
#' @return format_id
#' @export
#' @examples
#' \dontrun{
#' bety <- PEcAn.DB::betyConnect()
#' 
#' formats_variables_tibble <- tibble::tibble(
#'        variable_id = c(411, 135, 382), 
#'        name = c("NPP", NA, "YEAR"),  
#'        unit = c("g C m-2 yr-1", NA, NA),
#'        storage_type = c(NA, NA, "%Y"),
#'        column_number = c(2, NA, 4),
#'  )
#'   insert.format.vars(con = bety$con, format_name = "LTER-HFR-103", mimetype_id = 1090, notes = "NPP from Harvard Forest.", header = FALSE, skip = 0, formats_variables = formats_variables_tibble)
#' }
insert.format.vars <- function(con, format_name, mimetype_id, notes = NULL, header = TRUE, skip = 0, formats_variables = NULL, suppress = TRUE){
  
  # Test if name is a character string
  if(!is.character(format_name)){
    PEcAn.logger::logger.error(
      "Name must be a character string"
    )
  }
  
  # Test if format name already exists
  name_test <- dplyr::tbl(con, "formats") %>% dplyr::select(id, name) %>% dplyr::filter(name %in% format_name) %>% collect()
  name_test_df <- as.data.frame(name_test)
  if(!is.null(name_test_df[1,1])){
    PEcAn.logger::logger.error(
      "Name already exists"
    )
  }
  
   #Test if skip is an integer
  if(!is.character(skip)){
  PEcAn.logger::logger.error(
    "Skip must be of type character"
    )
  }
  
  # Test if header is a Boolean
  if(!is.logical(header)){
    PEcAn.logger::logger.error(
      "Header must be of type Boolean"
    )
  }
  
  # Test if notes are a character string
  if(!is.character(notes)&!is.null(notes)){
    PEcAn.logger::logger.error(
      "Notes must be of type character"
    )
  }
  
  ######## Formats-Variables tests ###############
  if(!is.null(formats_variables)){
    for(i in 1:nrow(formats_variables)){
      if(!is.numeric(formats_variables[[i,"variable_id"]])){
        PEcAn.logger::logger.error(
          "variable_id must be an integer"
        )
      }

      if(suppress == FALSE){
        ## Test if variable_id already exists ##
        var_id_test <- dplyr::tbl(con, "variables") %>% dplyr::select(id) %>% dplyr::filter(id %in% formats_variables[[i, "variable_id"]]) %>% dplyr::collect(id)
        if(!is.null(var_id_test[1,1])){
          PEcAn.logger::logger.error(
            "variable_id already exists"
          )
        }
      }

      if(!is.character(formats_variables[[i, "name"]])&!is.na(formats_variables[[i, "name"]])){
        PEcAn.logger::logger.error(
          "Variable name must be of type character or NA"
        )
      }
      if(!is.character(formats_variables[[i, "unit"]])&!is.na(formats_variables[[i, "unit"]])){
        PEcAn.logger::logger.error(
          "Units must be of type character or NA"
        )
      }
      if(!is.character(formats_variables[[i, "storage_type"]])&!is.na(formats_variables[[i, "storage_type"]])){
        PEcAn.logger::logger.error(
          "storage_type must be of type character or NA"
        )
      }
      if(!is.numeric(formats_variables[[i, "column_number"]])&!is.na(formats_variables[[i, "column_number"]])){
        PEcAn.logger::logger.error(
          "column_number must be of type numeric or NA"
        )
      }
    }
    
    ## convert NA to "" for inserting into db ##
    formats_variables[is.na(formats_variables)] <- ""
    
    ###  udunit tests ###
    for(i in 1:nrow(formats_variables)){
      u1 <- formats_variables[1,"unit"]
      u2 <- dplyr::tbl(con, "variables") %>% dplyr::select(id, units) %>% dplyr::filter(id %in% formats_variables[[1, "variable_id"]]) %>% dplyr::pull(units)
      
      if(!udunits2::ud.is.parseable(u1)){
        PEcAn.logger::logger.error(
          "Units not parseable. Please enter a unit that is parseable by the udunits library."
        )
      }
      # Grab the bety units and 
      if(!udunits2::ud.are.convertible(u1, u2)){
        PEcAn.logger::logger.error(
          "Units are not convertable."
        )
      }
    }
  }
  
    formats_df <- tibble::tibble(
      header = as.character(header),
      skip = skip,
      mimetype_id = mimetype_id,
      notes = notes,
      name = format_name,
      stringsAsFactors = FALSE
    )

    ## Insert format record
    inserted_formats <- db_merge_into(formats_df, "formats", con = con, by = c("name", "mimetype_id")) ## Make sure to include a 'by' argument
    format_id <- dplyr::pull(inserted_formats, id)
    
  if(!is.null(formats_variables)){
    ## Insert format_id into 
    n <- nrow(formats_variables)
    format_id_df <- matrix(data = format_id, nrow = n, ncol = 1)
    colnames(format_id_df) <- "format_id"
    
    ## Make query data.frame
    formats_variables_input <- cbind(format_id_df, formats_variables) 
    
    ## Insert Format-Variable record
    inserted_formats_variables <- db_merge_into(formats_variables_input, "formats_variables", con = con,  by = c("variable_id"))
  }
    return(format_id)
    
}
