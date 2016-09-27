##' @title met.process.stage
##' 
##' Function to set up stage for PEcAn met processing and set "update" or "new" routine.
##' It compares run start/end dates to those that exist in the input records of the BETY 
##' database, returning a list object that sets up met processing stages and gives 
##' dates that need to be updated in the database and those that need to be downloaded.
##' 
##' @param info.list object passed from met.files.tree function containing existing file information
##' @param start_date object containing start date of current model run attempts
##' @param met.source object containing end date of the current model run attempt
##' @export
##' @author Tony Gardella

met.process.set.stage <- function(info.list,start_date,end_date,con){
  result<- NULL
  stage <- NULL
  dates <- NULL
  skip <- FALSE
  
  if(length(info.list$raw.info)> 0){
    #Raw met Product exists. Check dates
    if(info.list$raw.info$start_date == start_date & info.list$raw.info$end_date == end_date){
      #Dates line-up. raw stage unecessary
      stage$download.raw <- FALSE
      stage$update <- FALSE
      stage$new <- FALSE
    }else if(start_date < info.list$raw.info$raw.start_date & end_date <= info.list$raw.info$raw.end_date){
      stage$download.raw <- stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates include years before db input record dates
      ## Dates to be put into input record
      dates$update$start_date <- start_date
      dates$update$end_date   <- info.list$raw.info$raw.end_date
      ## Dates to be downloaded 
      ###(Note:Download data up to current start_date record)
      dates$download$start_date <- start_date
      dates$download$end_date <- info.list$raw.info$raw.start_date
    }else if (end_date > info.list$raw.info$raw.end_date & start_date >= info.list$raw.info$raw.start_date){
      stage$download.raw <- stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates are for years after db input record dates
      ##Date to be put into input record
      dates$update$start_date <- info.list$raw.info$raw.start_date
      dates$update$end_date <- end_date
      ## Dates to be downloaded
      dates$download$start_date <- info.list$raw.info$raw.end_date
      dates$download$end_date <- end_date
    }else if (start_date < info.list$raw.info$raw.start_date & end_date > info.list$raw.info$raw.end_date){
      stage$download.raw <- stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates envelope existing db dates
      ## Dates to be put into input records
      dates$update$start_date <- start_date
      dates$update$end_date <- end_date
      ## Dates to be downloaded
      dates$download$start_date[1] <- start_date
      dates$download$end_date[1] <-info.list$raw.info$raw.start_date
      dates$download$start_date[2] <- info.list$raw.info$raw.end_date
      dates$download$end_date[2] <- end_date
    }
    
    
  }else if (info.list$raw.info == 0){
    stage$download.raw <- stage$met2cf <- stage$standardize <- stage$met2model <- TRUE
    stage$new <- TRUE
    stage$update <- FALSE
    skip <- TRUE
  }
  #----------------------------- Move onto checking next CF Met Files-----------------------------------------------------#  
  if(length(info.list$cf.info) > 0 & skip == FALSE){
    #CF standardized met files exists. Check dates.
    if(info.list$cf.info$start_date == start_date & info.list$cf.info$end_date == end_date){
      #Dates line-up. CF stage unecessary
      stage$download.raw <- FALSE
      stage$update <- FALSE
      stage$new <- FALSE
    }else if(start_date < info.list$cf.info$raw.start_date & end_date <= info.list$cf.info$cf.end_date){
      stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates include years before db input record dates
      ## Dates to be put into input record
      dates$update$start_date <- start_date
      dates$update$end_date   <- info.list$cf.info$cf.end_date
      ## Dates to be downloaded 
      ###(Note:Download data up to current start_date record)
      dates$download$start_date <- start_date
      dates$download$end_date <- info.list$cf.info$cf.start_date
    }else if (end_date > info.list$cf.info$cf.end_date & start_date >= info.list$cf.info$cf.start_date){
      stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates are for years after db input record dates
      ##Date to be put into input record
      dates$update$start_date <- info.list$cf.info$cf.start_date
      dates$update$end_date <- end_date
      ## Dates to be downloaded
      dates$download$start_date <- info.list$cf.info$cf.end_date
      dates$download$end_date <- end_date
    }else if (start_date < info.list$cf.info$raw.start_date & end_date > info.list$cf.info$cf.end_date){
      stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates envelope existing db dates
      ## Dates to be put into input records
      dates$update$start_date <- start_date
      dates$update$end_date <- end_date
      ## Dates to be downloaded
      dates$download$start_date[1] <- start_date
      dates$download$end_date[1] <-info.list$cf.info$cf.start_date
      dates$download$start_date[2] <- info.list$cf.info$cf.end_date
      dates$download$end_date[2] <- end_date
    }
  }else if(info.list$cf.info == 0 & skip == FALSE){
    stage$standardize <- stage$met2model <- TRUE
    stage$new <- TRUE
    stage$update <- FALSE
    skip <- TRUE
  }
  #----------------------------- Move onto gap filled section ----------------------------------------------------------# 
  if(length(info.list$gf.info) < 0 & skip == FALSE){
    #Gapfilled met files files exists. Check dates. 
    if(info.list$gf.info$start_date == start_date & info.list$gf.info$end_date == end_date){
      #Dates line-up. gf stage unecessary
      stage$standardize <- FALSE
      stage$update <- FALSE
      stage$new <- FALSE
    }else if(start_date < info.list$gf.info$raw.start_date & end_date <= info.list$gf.info$raw.end_date){
      stage$standardize <- stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates include years before db input record dates
      ## Dates to be put into input record
      dates$update$start_date <- start_date
      dates$update$end_date   <- info.list$gf.info$gf.end_date
      ## Dates to be downloaded 
      ###(Note:Download data up to current start_date record)
      dates$download$start_date <- start_date
      dates$download$end_date <- info.list$gf.info$gf.start_date
    }else if (end_date > info.list$gf.info$gf.end_date & start_date >= info.list$gf.info$gf.start_date){
      stage$standardize <- stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates are for years after db input record dates
      ##Date to be put into input record
      dates$update$start_date <- info.list$gf.info$gf.start_date
      dates$update$end_date <- end_date
      ## Dates to be downloaded
      dates$download$start_date <- info.list$gf.info$gf.end_date
      dates$download$end_date <- end_date
    }else if (start_date < info.list$gf.info$gf.start_date & end_date > info.list$gf.info$gf.end_date){
      stage$download.raw <- stage$met2cf <-  stage$standardize <- stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates envelope existing db dates
      ## Dates to be put into input records
      dates$update$start_date <- start_date
      dates$update$end_date <- end_date
      ## Dates to be downloaded
      dates$download$start_date[1] <- start_date
      dates$download$end_date[1] <-info.list$gf.info$gf.start_date
      dates$download$start_date[2] <- info.list$gf.info$gf.end_date
      dates$download$end_date[2] <- end_date
    }
  }else if (info.list$gf.info == 0 & skip == FALSE){
    stage$standardize <- stage$met2model <- TRUE
    stage$new <- TRUE
    stage$update <- FALSE
    skip <- TRUE
  }
  #----------------------------Move onto model stage --------------------------------------------------------------------#
  if(length(info.list$model.info) < 0 & skip == FALSE){
    #Model Specific files exist. Check dates.
    #CF standardized met files exists. Check dates.
    if(info.list$cf.info$start_date == start_date & info.list$cf.info$end_date == end_date){
      #Dates line-up. CF stage unecessary
      stage$download.raw <- FALSE
      stage$update <- FALSE
      stage$new <- FALSE
    }else if(start_date < info.list$model.info$model.start_date & end_date <= info.list$model.info$model.end_date){
      stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates include years before db input record dates
      ## Dates to be put into input record
      dates$update$start_date <- start_date
      dates$update$end_date   <- info.list$model.info$model.end_date
      ## Dates to be downloaded 
      ###(Note:Download data up to current start_date record)
      dates$download$start_date <- start_date
      dates$download$end_date <- info.list$model.info$model.start_date
    }else if (end_date > info.list$model.info$model.end_date & start_date >= info.list$model.info$model.start_date){
      stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates are for years after db input record dates
      ##Date to be put into input record
      dates$update$start_date <- info.list$model.info$model.start_date
      dates$update$end_date <- end_date
      ## Dates to be downloaded
      dates$download$start_date <- info.list$model.info$model.end_date
      dates$download$end_date <- end_date
    }else if (start_date < info.list$model.info$model.start_date & end_date > info.list$model.info$model.end_date){
      stage$met2model <-TRUE
      stage$update <- TRUE
      #Run dates envelope existing db dates
      ## Dates to be put into input records
      dates$update$start_date <- start_date
      dates$update$end_date <- end_date
      ## Dates to be downloaded
      dates$download$start_date[1] <- start_date
      dates$download$end_date[1] <-info.list$model.info$model.start_date
      dates$download$start_date[2] <- info.list$model.info$model.end_date
      dates$download$end_date[2] <- end_date
    }
  }else if (info.list$model.info == 0 & skip == FALSE){
    stage$met2model <- TRUE
    stage$new <- TRUE
    stage$update <- FALSE
  }
  
  #If any stage has not been set, because of skip == TURE, set stage to TRUE.
  if(is.null(stage$download.raw)){stage$download.raw <- TRUE}
  if(is.null(stage$met2cf)){stage$met2cf <- TRUE}
  if(is.null(stage$standardize)){stage$standardize <- TRUE}
  if(is.null(stage$met2model)){stage$met2model <- TRUE}
  
  stage <- stage
  stage$dates <- dates 
  return(stage)
  
} ### END OF FUNCTION