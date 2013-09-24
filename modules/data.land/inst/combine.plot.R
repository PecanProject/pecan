##' function combines multiple plot files into a list
##' @title Combine Plots
##' @param folder
##' @return plot.data, list
##' @author Joshua Mantooth 
combine.plot <- function(folder){
  filelist <- dir(folder,pattern =  "*.csv",full.names=TRUE)
  plot.data <- list()
  for (file in filelist){
     plot.data[[file]] <- read.plot(file) # read each file from folder and list 
  }
  plot.data<<-plot.data
  return(plot.data)
}
