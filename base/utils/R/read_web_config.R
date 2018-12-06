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

  ## replacements
  config <- gsub("^\\$", "", config)  ## remove leading $
  config <- gsub(";.*$", "", config)  ## remove ; and everything afterwards
  config <- sub("false", "FALSE", config, fixed = TRUE)  ##  Boolean capitalization
  config <- sub("true", "TRUE", config, fixed = TRUE)  ##  Boolean capitalization
  config <- gsub(pattern = "DIRECTORY_SEPARATOR",replacement = "/",config)

  ## subsetting
  config <- config[!grepl("exec", config, fixed = TRUE)]  ## lines 'exec' fail
  config <- config[!grepl("dirname", config, fixed = TRUE)]  ## lines 'dirname' fail
  config <- config[!grepl("array", config, fixed = TRUE)]  ## lines 'array' fail

  ##references
  ref <- grep("$", config, fixed = TRUE)
  if(length(ref) > 0){
    refsplit = strsplit(config[ref],split = " . ",fixed=TRUE)[[1]]
    refsplit = sub(pattern = '\"',replacement = "",x = refsplit)
    refsplit = sub(pattern = '$',replacement = '\"',refsplit,fixed=TRUE)
    config[ref] <- paste0(refsplit,collapse = "")  ## lines with variable references fail
  }

  ## convert to list
  config.list <- eval(parse(text = paste("list(", paste0(config, collapse = ","), ")")))

  ## replacements
  config.list <- lapply(X = config.list,FUN = sub,pattern="output_folder",replacement=config.list$output_folder,fixed=TRUE)

  return(config.list)
}

