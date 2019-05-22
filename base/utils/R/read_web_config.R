#' Read `config.php` file into an R list
#'
#' @author Alexey Shiklomanov, Michael Dietze, Rob Kooper
#' @param php.config Path to `config.php` file
#' @param parse Logical. If `TRUE` (default), try to parse numbers and
#'   unquote strings.
#' @param expand Logical. If `TRUE` (default), try to perform some
#'   variable substitutions (only done if parse = TRUE).
#' @return Named list of variable-value pairs set in `config.php`
#' @export
#' @examples
#' # Read Docker configuration and extract the `dbfiles` and output folders.
#' docker_config <- read_web_config(file.path("..", "..", "docker", "web", "config.docker.php"))
#' docker_config[["dbfiles_folder"]]
#' docker_config[["output_folder"]]
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

  # always remove the \"
  config_list <- lapply(config_list, gsub,
                        pattern = "\"(.*?)\"", replacement = "\\1")

  # parse data and cleanup
  if (parse) {
    # try to conver to number/boolean (as.numeric(FALSE) == 0)
    config_list <- lapply(config_list, function(s) {
      s <- tryCatch(as.numeric(s), warning = function(e) {
        b = as.logical(s)
        ifelse(is.na(b), s, b)
      })
    })

    if (expand) {
      of <- config_list[["output_folder"]]
      if (!is.null(of)) {
        modify <- grep("\\$output_folder *\\. *", config_list)
        # gsub will force all fields to character
        config_list[modify] <- lapply(config_list[modify], gsub,
                                      pattern = "\\$output_folder *\\. *", replacement = of)
      }
    }
  }

  config_list
}
