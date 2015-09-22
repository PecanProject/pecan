##' @title PEcAn Network Status
##' @author Michael Dietze
##' 
##'
##' 

args <- commandArgs(TRUE)

library(PEcAn.DB)

## settings
network.file = "Network.RData"
php.config   = "config.example.php"
config = scan(php.config,what="character",sep="\n")
config = config[grep("^\\$",config)]  ## find lines that begin with $ (variables)
config = sub("$","",config,fixed = TRUE) ## remove $
config = sub(";","",config,fixed = TRUE) ## remove ;
config = sub("false","FALSE",config,fixed = TRUE) ##  Boolean capitalization
config = sub("true","TRUE",config,fixed = TRUE) ##  Boolean capitalization
config = config[-grep("$",config,fixed=TRUE)] ## lines with variable references fail
config = config[-grep("exec",config,fixed=TRUE)] ## lines 'exec' fail
config.list = eval(parse(text=paste('list(', paste0(config[1:14],collapse=","), ')'))) 


## load previous state or initialize
if(file.exists(network.file)){
  load(network.file)
} else {

}

## Make sure node info is up-to-date
con = 
