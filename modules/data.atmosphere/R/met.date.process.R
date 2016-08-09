##' @title met.process.set.stage
##' @export
##' 
##' @param input.info.list ,list containing DB record ids, start-dates and end_dates
##' @param start_date , Start date as given by run
##' @param end_date , End date as given bu run
##' @param con, database connection
##' @author Tony Gardella, Elizabeth Cowdery

met.process.set.stage <- function(input.info.list,start_date,end_date,con){
  stage <- NULL
  dates <-NULL
  ## Check if Raw exists
  if(!is.null(input.info.list$raw[[1]])){
    ## If YES then check if Raw dates match run dates
    if(input.info.list$raw[[2]] == start_date & input.info.list$raw[[3]] == end_date){
      stage$download.raw <- FALSE
      
      ## Since Raw dates match up, check if met2cf exists and dates match up
      if(!is.null(input.info.list$met2cf[[1]])){
        if(input.info.list$met2cf[[2]] == start_date & input.info.list$met2cf[[3]] == end_date){
          stage$met2cf <- FALSE
          
          ## Since CF dates match up, check if gapfilled file exists and dates match up
          if(!is.null(input.info.list$gfill[[1]])){
            if(input.info.list$gfill[[2]] == start_date & input.info.list$gfill[[3]] == end_date){
              stage$standardize <- FALSE   
              
              ## Since gapfill dates match up, check if model formatted file exists and dates match up
              if(!is.null(input.info.list$met.model.input.id[[1]])){
                if(input.info.list$met2model[[2]]== start_date & input.info.list$met2model[[3]] == end_date){
                  stage$met2model <- FALSE
                  
                }else if (start_date < input.info.list$met2model[[2]] & end_date <= input.info.list$met2model[[3]]){
                  stage$download.raw <- stage$met2met.model <-  stage$standardize <- stage$met2model <-TRUE
                  stage$update <- TRUE
                  #Run dates include years before db input record dates
                  ## Dates to be put into input record
                  dates$update$start_date <- start_date
                  dates$update$end_date   <- input.info.list$met2model[[3]]
                  ## Dates to be downloaded 
                  ###(Note:Download data up to current start_date record)
                  dates$download$start_date <- start_date
                  dates$download$end_date <- input.info.list$met2model[[2]]
                }else if (end_date > input.info.list$met2model[[3]] & start_date >= input.info.list$met2model[[2]]){
                  stage$download.raw <- stage$met2met.model <-  stage$standardize <- stage$met2model <-TRUE
                  stage$update <- TRUE
                  #Run dates are for years after db input record dates
                  ##Date to be put into input record
                  dates$update$start_date <- input.info.list$met2model[[2]]
                  dates$update$end_date <- end_date
                  ## Dates to be downloaded
                  dates$download$start_date <- input.info.list$met2model[[3]]
                  dates$download$end_date <- end_date
                }else if (start_date < input.info.list$met2model[[2]] & end_date > input.info.list$met2model[[3]]){
                  stage$download.raw <- stage$met2met.model <-  stage$standardize <- stage$met2model <-TRUE
                  stage$update <- TRUE
                  #Run dates envelope existing db dates
                  ## Dates to be put into input records
                  dates$update$start_date <- start_date
                  dates$update$end_date <- end_date
                  ## Dates to be downloaded
                  dates$download$start_date[1] <- start_date
                  dates$download$end_date[1] <-input.info.list$met2model[[2]]
                  dates$download$start_date[2] <- input.info.list$met2model[[3]]
                  dates$download$end_date[2] <- end_date
                }
              }
              
              
            }else if (start_date < input.info.list$gfill[[2]] & end_date <= input.info.list$gfill[[3]]){
              stage$download.raw <- stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
              stage$update <- TRUE
              #Run dates include years before db input record dates
              ## Dates to be put into input record
              dates$update$start_date <- start_date
              dates$update$end_date   <- input.info.list$gfill[[3]]
              ## Dates to be downloaded 
              ###(Note:Download data up to current start_date record)
              dates$download$start_date <- start_date
              dates$download$end_date <- input.info.list$gfill[[2]]
            }else if(end_date > input.info.list$gfill[[3]] & start_date >= input.info.list$gfill[[2]]){
              stage$download.raw <- stage$met2gfill <-  stage$standardize <- stage$met2model <-TRUE
              stage$update <- TRUE
              #Run dates are for years after db input record dates
              ##Date to be put into input record
              dates$update$start_date <- input.info.list$gfill[[2]]
              dates$update$end_date <- end_date
              ## Dates to be downloaded
              dates$download$start_date <- input.info.list$gfill[[3]]
              dates$download$end_date <- end_date
            }else if(start_date < input.info.list$gfill[[2]] & end_date > input.info.list$gfill[[3]]){
              stage$download.raw <- stage$met2gfill <-  stage$standardize <- stage$met2model <-TRUE
              stage$update <- TRUE
              #Run dates envelope existing db dates
              ## Dates to be put into input records
              dates$update$start_date <- start_date
              dates$update$end_date <- end_date
              ## Dates to be downloaded
              dates$download$start_date[1] <- start_date
              dates$download$end_date[1] <-input.info.list$gfill[[2]]
              dates$download$start_date[2] <- input.info.list$gfill[[3]]
              dates$download$end_date[2] <- end_date
            }
          }
          
          
          
        }else if (start_date < input.info.list$met2cf[[2]] & end_date <= input.info.list$met2cf[[3]]){
          stage$download.raw <- stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
          stage$update <- TRUE
          #Run dates include years before db input record dates
          ## Dates to be put into input record
          dates$update$start_date <- start_date
          dates$update$end_date   <- input.info.list$met2cf[[3]]
          ## Dates to be downloaded 
          ###(Note:Download data up to current start_date record)
          dates$download$start_date <- start_date
          dates$download$end_date <- input.info.list$met2cf[[2]]
        }else if(end_date > input.info.list$met2cf[[3]] & start_date >= input.info.list$met2cf[[2]]){
          stage$download.raw <- stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
          stage$update <- TRUE
          #Run dates are for years after db input record dates
          ##Date to be put into input record
          dates$update$start_date <- input.info.list$met2cf[[2]]
          dates$update$end_date <- end_date
          ## Dates to be downloaded
          dates$download$start_date <- input.info.list$met2cf[[3]]
          dates$download$end_date <- end_date
        }else if (start_date < input.info.list$met2cf[[2]] & end_date > input.info.list$met2cf[[3]]){
          stage$download.raw <- stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
          stage$update <- TRUE
          #Run dates envelope existing db dates
          ## Dates to be put into input records
          dates$update$start_date <- start_date
          dates$update$end_date <- end_date
          ## Dates to be downloaded
          dates$download$start_date[1] <- start_date
          dates$download$end_date[1] <-input.info.list$met2cf[[2]]
          dates$download$start_date[2] <- input.info.list$met2cf[[3]]
          dates$download$end_date[2] <- end_date
        }    
      }
      
    }else if(start_date < input.info.list$raw[[2]] & end_date <= input.info.list$raw[[3]]){
      stage$download.raw <- stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates include years before db input record dates
      ## Dates to be put into input record
      dates$update$start_date <- start_date
      dates$update$end_date   <- input.info.list$raw[[3]]
      ## Dates to be downloaded 
      ###(Note:Download data up to current start_date record)
      dates$download$start_date <- start_date
      dates$download$end_date <- input.info.list$raw[[2]]
    }else if (end_date > input.info.list$raw[[3]] & start_date >= input.info.list$raw[[2]]){
      stage$download.raw <- stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates are for years after db input record dates
      ##Date to be put into input record
      dates$update$start_date <- input.info.list$raw[[2]]
      dates$update$end_date <- end_date
      ## Dates to be downloaded
      dates$download$start_date <- input.info.list$raw[[3]]
      dates$download$end_date <- end_date
    }else if (start_date < input.info.list$raw[[2]] & end_date > input.info.list$raw[[3]]){
      stage$download.raw <- stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates envelope existing db dates
      ## Dates to be put into input records
      dates$update$start_date <- start_date
      dates$update$end_date <- end_date
      ## Dates to be downloaded
      dates$download$start_date[1] <- start_date
      dates$download$end_date[1] <-input.info.list$raw[[2]]
      dates$download$start_date[2] <- input.info.list$raw[[3]]
      dates$download$end_date[2] <- end_date
    }    
    
  }else{
    
    ## No input record of an sort for site and source. Run all stages as normal.
    
    stage$download.raw <- stage$met2cf <- stage$standardize <- stage$met2model <- TRUE
    stage$new <- TRUE
    stage$update <- FALSE
    
    dates$download.start_date <- start_date
    dates$download.end_date <- end_date
    dates$update.start_date <- start_date
    datesupdate.end_date <- start_date
    
  } 
  
} ### END OF FUNCTION