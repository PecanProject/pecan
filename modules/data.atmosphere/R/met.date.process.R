##' @title met.data.process
##' @export
##'
##' @param dates, list of date information from db and run
##' @author Tony Gardella, Elizabeth Cowdery

dates2process <- function(dates){
  
  if(run_start < db_start & run_end < db_end){
    # run dates are for years before db run dates
    dates$new_start = run_start
    dates$new_end = db_start
    dates$update_new = run_start
    dates$update_end = db_end
  }else if(run_start > db_start & run_end > db_end){
    #runs dates are for years after db dates
    dates$new_start = db_end
    dates$new_end = run_end
    dates$update_new = db_start
    dates$update_end = run_end
  }else if(run_start < db_start & run_end > db_end){
    #run dates envelope existing db dates
    dates$new_start = run_start
    dates$new_end = run_end
    dates$update_new = run_start
    dates$update_end = run_end
  }else if(run_start > db_start & run_end < db_end){
    #run dates are between db dates(subset case)
    dates$new_start = run_start
    dates$new_end = run_end
    dates$update_new = run_start
    dates$update_end = run_end
  }
}
  
  