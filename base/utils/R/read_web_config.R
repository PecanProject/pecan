#' read_web_config
#'
#' @author Michael Dietze and Rob Kooper
#' @param php.config Path to `config.php`
#'
#' @return config.list
#' @export
#'
#' 
read_web_config = function(php.config = "../../web/config.php") {
  
  ## Read PHP config file for webserver
  config <- scan(php.config, what = "character", sep = "\n")
  config <- config[grep("^\\$", config)]  ## find lines that begin with $ (variables)
  config <- sub("$", "", config, fixed = TRUE)  ## remove $
  config <- sub(";", "", config, fixed = TRUE)  ## remove ;
  config <- sub("false", "FALSE", config, fixed = TRUE)  ##  Boolean capitalization
  config <- sub("true", "TRUE", config, fixed = TRUE)  ##  Boolean capitalization
  config <- config[-grep("$", config, fixed = TRUE)]  ## lines with variable references fail
  config <- config[-grep("exec", config, fixed = TRUE)]  ## lines 'exec' fail
  config.list <- eval(parse(text = paste("list(", paste0(config[1:14], collapse = ","), ")")))
  
  return(config.list)
}