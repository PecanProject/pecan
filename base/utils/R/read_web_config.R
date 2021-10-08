#' Read `config.php` file into an R list
#'
#' @author Alexey Shiklomanov, Michael Dietze, Rob Kooper
#' @param php.config Path to `config.php` file
#' @param parse Logical. If `TRUE` (default), try to parse numbers and
#'   unquote strings.
#' @param expand Logical. If `TRUE` (default), try to perform some
#'   variable substitutions.
#' @return Named list of variable-value pairs set in `config.php`
#' @export
#' @examples
#' \dontrun{
#' # Read Docker configuration and extract the `dbfiles` and output folders.
#' docker_config <- read_web_config(file.path("..", "..", "docker", "web", "config.docker.php"))
#' docker_config[["dbfiles_folder"]]
#' docker_config[["output_folder"]]
#' }
read_web_config <- function(php.config = "../../web/config.php",
                            parse = TRUE,
                            expand = TRUE) {

  config <- readLines(php.config)
  config <- config[grep("^\\$", config)]  ## find lines that begin with $ (variables)

  rxp <- paste0("^\\$([[:graph:]]+?)[[:space:]]*",
                "=[[:space:]]*(.*?);?(?:[[:space:]]*//+.*)?$")
  rxp_matches <- regexec(rxp, config, perl = TRUE)
  results <- regmatches(config, rxp_matches)
  list_names <- vapply(results, `[[`, character(1), 2, USE.NAMES = FALSE)
  config_list <- lapply(results, `[[`, 3)
  names(config_list) <- list_names

  # Convert to numeric if possible
  if (parse) {
    # Remove surrounding quotes
    config_list <- lapply(config_list, gsub,
                          pattern = "\"(.*?)\"", replacement = "\\1")

    # Try to convert numbers to numeric
    config_list <- lapply(
      config_list,
      function(x) tryCatch(as.numeric(x), warning = function(e) x)
    )
  }

  if (expand) {
    # Replace $output_folder with its value, and concatenate strings
    chr <- vapply(config_list, is.character, logical(1))
    config_list[chr] <- lapply(config_list[chr], gsub,
                          pattern = "\\$output_folder *\\. *",
                          replacement = config_list[["output_folder"]])
  }
  config_list
}
