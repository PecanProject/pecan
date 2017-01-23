#' Match BETY species ID.
#'
#' Parses species codes in input data and matches them with the BETY species ID.
#' 
#' @param input_codes Character vector of species codes
#' @param format_name Species code format name (character, length 1)
#' @param betydb \code{dplyr} \code{src} object containing BETY connection 
#' @param custom_translation_path File path to translation
#' @return \code{data.frame} containing the following columns:
#' \describe{
#'  \item{\code{input_code}}{Character provided as input}
#'  \item{\code{bety_species_id}}{Big integer species ID, unique and specific to BETY}
#'  \item{\code{genus}}{Genus part of Latin name, from BETY}
#'  \item{\code{species}}{Species part of Latin name, from BETY}
#' }
#' @author Alexey Shiklomanov <ashiklom@bu.edu>
#' @examples
#' betydb <- dplyr::src_postgres(dbname = 'bety',
#'                        user = 'bety',
#'                        password = 'bety',
#'                        host = 'localhost')
#' input_codes <- c('ACRU', 'PIMA', 'TSCA')
#' format_name <- 'usda'
#' match_species_id(input_codes = input_codes,
#'                  format_name = format_name,
#'                  betydb = betydb)
#'                  
#' @export
match_species_id <- function(input_codes, format_name, betydb = NULL, custom_translation_path = NULL, ...) {
    # Relate format names to BETY columns
    formats_dict <- c('usda' = 'Symbol',
                      'fia' = 'spcd',
                      'latin_name' = 'scientificname', 
                      'custom' = 'custom')
    if (!format_name %in% names(formats_dict)) {
        stop('format_name "', format_name, '" not found. ',
             'Please use one of the following: ',
             paste(names(formats_dict), collapse = ', '))
    }
    if (!is.null(custom_translation_path)) {
        if (!file.exists(custom_translation_path)) {
            stop('No file found for path: ', custom_translation_path,
                 ' Make sure this file is present on this machine')
        }
        # TODO: Fill in with correct load_data arguments
        #translation <- PEcAn.utils::load_data(file = custom_translation_path, ...)
        if (!'input_code' %in% colnames(translation)) {
            stop('Translation table missing "input_code" column.')
        }
    } else {
        column <- formats_dict[format_name]
        translation <- dplyr::tbl(betydb, 'species')
        filter_cri <- lazyeval::interp(~ col %in% codes, 
                                       col = as.name(column),
                                       codes = input_codes)
        translation <- dplyr::filter_(translation, filter_cri)
        translation <- dplyr::select_(translation, 'bety_species_id' = 'id', 
                                      'genus', 'species', 'input_code' = column)
        translation <- dplyr::collect(translation)
    }
    input_table <- data.frame(input_code = input_codes, stringsAsFactors = FALSE)
    merge_table <- dplyr::left_join(input_table, translation)
    return(merge_table)
} # match_species_id

