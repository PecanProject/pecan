##' @name match.timestep 
##' @title Match time step
##' @param date.fine numeric
##' @param data.fine data.frame
##' @param date.coarse numeric
##' @export match.timestep
##' 
##' @author Istem Fer
match.timestep <- function(date.coarse, date.fine, data.fine){
  
  # convert data.frame to data.table using date as the reference 
  fine.dt <- setDT(data.frame(data = data.fine, date = date.fine), key = "date")
  coarse.dt <- setDT(data.frame(date = date.coarse), key = "date")
  
  # extract from fine-step data matching the nearest time-stamp to the coarse-step
  match.dt <- fine.dt[coarse.dt, roll = 'nearest']
  
  # convert from data.table back to data.frame
  match.df <- setDF(match.dt)
  
  # data will be in the first column
  return(match.df[,1])
}
