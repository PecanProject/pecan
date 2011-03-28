##' Read ED output 
##'
##' Extract ED output for specific variables from an hdf5 file
##' @title 
##' @param filename 
##' @param variables 
##' @return single value of AGB from  filename for all plants
read.output.ed <- function(filename, variables = c("AGB_CO", "NPLANT")){
  data <- hdf5load(filename, load = FALSE)[variables]
  if(c("AGB_CO", "NPLANT") %in% variables)) {
   AGB  <- sum(data$AGB_CO * data$NPLANT) * 20
   append(AGB, data)
  }
  return(data$AGB)
}

##' ##' .. content for \description{} (no empty lines) ..
##'
##' 
##' @title 
##' @return vector of output variable for all runs within ensemble
read.ensemble.output <- function(filenames,  years = seq(start.year, end.year)){
  ens.filenames <- grep("ENS", grep(years, filenames))
  agb.temp <- tapply
  output.vector <- tapply(ens.filenames, read.output)
}

read.ensemble.runn.yeari(runn, yeari, read.output = read.output.ed){
read.output(runn, yeari

##' .. content for \description{} (no empty lines) ..
##'
##' 
##' @title 
##' @return dataframe with one col per quantile analysed and one row per trait  
##' @author David
read.sa.output       <- function(filenames){

}
