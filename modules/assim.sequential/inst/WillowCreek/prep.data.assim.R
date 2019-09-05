##'@title prep.data.assim
##'@section purpose:
##'This function finds flux uncertainty and finds mean and cov
##'for a call to PEcAn.assim.sequential::sda.enkf() 
##'
##'@param settings the PEcAn settings object (a collection of nested lists)
##'@param numvals number of simulated data points for each time point
##'@param vars variable (NEE or LE) that you want to do data assimilation on 
##'@param data.len how many hours for the output (default is 48 hours)
##'@return None
##'@export
##'@author Luke Dramko and K. Zarada and Hamze Dokoohaki
prep.data.assim <- function(start_date, end_date, numvals, vars, data.len = 48) {
  
  data.len = data.len *2 #turn hour time steps into half hour
  
  Date.vec <-NULL

  gapfilled.vars <- vars %>%
      purrr::map_dfc(function(var) {
     
        field_data <- gapfill_WCr(start_date, end_date, var)
  
        PEcAn.logger::logger.info(paste(var, " is done"))
        #I'm sending the date out to use it later on 
        return(field_data)
    })


  #Reading the columns we need
  cols <- grep(paste0("_*_f$"), colnames(gapfilled.vars), value = TRUE)
  gapfilled.vars <- gapfilled.vars %>% dplyr::select(Date=date, Flag,cols)

 #Creating NEE and LE filled output 
  gapfilled.vars.out <- gapfilled.vars %>% dplyr::select(-Flag) %>% 
                            tail(data.len)

 #Pecan Flux Uncertainty 
  processed.flux <- 3:(3+length(vars)-1) %>%
    purrr::map(function(col.num) {

      field_data <- gapfilled.vars[,c(1,2,col.num)]
      
      uncertainty_vals <- list()
      # Creates a proxy row for rbinding
      sums <- list()
      # One vector holds the mean for each variable.
      obs.mean <- NULL
      # for each of Gap filling uncertainty bands 
      # The first will be always Date, second Flag and the third is the flux
      AMF.params <- PEcAn.uncertainty::flux.uncertainty(field_data[,3], QC = field_data$Flag)
      
      # Create proxy row for rbinding
      random_mat = NULL
      new_col = rep(0, dim(field_data)[1])
        
      # Create a new column
      # i: the particular variable being worked with; j: the column number; k: the row number
      for (j in 1:numvals) {
        # number of random numbers
        obs <- field_data[, 3][!is.na(field_data[, 3])]
        pos <- obs >= 0
        
        res <- obs
        res[pos]  <- rexp(length(obs[pos]),
                          1 / (AMF.params$intercept[[1]] + (AMF.params$slopeP[[1]] * obs[pos])))
        
        res[!pos]  <- rexp(length(obs[!pos]),
                           1 / (AMF.params$intercept[[1]] + (AMF.params$slopeN[[1]] * obs[!pos])))
        
        random_multiplier <- sample(c(-1, 1), length(res), replace = TRUE)
        simulated <- obs + (random_multiplier * res)

        random_mat = cbind(random_mat, simulated)
        } # end j
        
        obs.mean <- c(obs.mean, mean(field_data[, 3], na.rm = TRUE))
        # this keeps the mean of each day for the whole time series and all variables
        sums = c(sums, list(random_mat))
   
        data.frame(Date=field_data$Date,sums)
    }) # end of map

 #I'm sending mixing up simulations of vars to aggregate them first and then estimate their var/cov
  outlist<-processed.flux %>%
       map2_dfc(vars, function(x, xnames) {
         names(x)[2:numvals] <- paste0(names(x)[2:numvals], xnames)
 
         x %>%
           tail(data.len) %>%
           mutate(Interval = lubridate::round_date(Date, "6 hour")) %>%
           dplyr::select(-Date)
       }) %>%
         split(.$Interval) %>%
           map(function(row) {
         
            #fidning the interval cols / taking them out 
             colsDates <- grep(paste0("Interval"), colnames(row), value = FALSE)
             Date1 <- row[, colsDates[1]]
             row <- row[, -c(colsDates)]
             # finding the order of columns in dataframe
              var.order <- split(1:ncol(row),
                                 ceiling(seq_along(1:ncol(row))/(ncol(row)/length(vars))))
              
              #combine all the numbers for this time interval
              alldata <- var.order %>% 
               map_dfc(~row[,.x] %>% unlist %>% as.numeric) %>%
               setNames(vars) 
              # mean and the cov between all the state variables is estimated here 
              return(list(
                Date = Date1 %>% unique(),
                covs = cov(alldata),
                means = apply(alldata, 2, mean)
              ))
           })
  
  outlist <- list(obs=outlist, rawobs=gapfilled.vars.out )
  
  return(outlist)

} # prep.data.assim

