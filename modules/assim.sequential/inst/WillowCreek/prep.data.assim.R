##'@title prep.data.assim
##'@section purpose:
##'This function finds flux uncertainty and finds mean and cov
##'for a call to PEcAn.assim.sequential::sda.enkf() 
##'
##'@param settings the PEcAn settings object (a collection of nested lists)
##'@param numvals number of simulated data points for each time point
##'@param vars variable (NEE or LE) that you want to do data assimilation on 
##'@return None
##'@export
##'@author Luke Dramko and K. Zarada and Hamze Dokoohaki
prep.data.assim <- function(start_date, end_date, numvals, vars) {
  
  Date.vec <-NULL
  
  gapfilled.vars <- vars %>%
    purrr::map(function(var) {
      
      field_data <- gapfill_WCr(start_date, end_date, var)
      cols <- grep(paste0(var, "_*_f$"), colnames(field_data), value = TRUE)
      field_data <- field_data %>% dplyr::select(cols, Flag)
      PEcAn.logger::logger.info(paste(var, " is done"))
      #I'm sending the date out to use it later on 
      Date.vec <<- field_data$date
      return(field_data)
  })
  
  
  processed.flux <- gapfilled.vars %>%
    purrr::map(function(field_data) {
      uncertainty_vals <- list()
      # Creates a proxy row for rbinding
      sums <- list()
      # One vector holds the mean for each variable.
      obs.mean <- NULL
      # for each of Gap filling uncertainty bands
      for (i in 1:(dim(field_data)[2] - 1)) {
        AMF.params <- PEcAn.uncertainty::flux.uncertainty(field_data[, i], QC = field_data$Flag)
        
        # Create proxy row for rbinding
        random_mat = NULL
        new_col = rep(0, dim(field_data)[1])
        
        # Create a new column
        # i: the particular variable being worked with; j: the column number; k: the row number
        for (j in 1:numvals) {
          # number of random numbers
          obs <- field_data[, i][!is.na(field_data[, i])]
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
        
        obs.mean <- c(obs.mean, mean(field_data[, i], na.rm = TRUE))
        # this keeps the mean of each day for the whole time series and all variables
        sums = c(sums, list(random_mat))
      } # end i
      sums
    }) # end of map
 browser()
  
 processed.flux %>% 
   purrr::map(~.x%>% 
                as.data.frame %>%
                mutate(Date=Date.vec)
              )
 
 
} # prep.data.assim

prep.data.assim(start_date = "2017-01-01", end_date = "2018-10-30",numvals = 10, var = c("NEE","LE"))

