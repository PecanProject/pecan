##'@title prep.data.assim
##'@section purpose:
##'This function sets up a call to PEcAn.assim.sequential::sda.enkf()
##'
##'@param settings the PEcAn settings object
##'@return None
##'@export
##'@author Luke Dramko
prep.data.assim <- function(settings) {
  fcn <- "download.US-WCr"
  
  # Obtain real data from the site
  field_data <- PEcAn.data.atmosphere::download.US_WCr(settings$run$start_date, settings$run$end_date, timestep = 0.5)
  
  uncertainty_vals <- list()
  
  # Creates a proxy row for rbinding
  sums <- matrix(rep(0, length(field_data[[1]])), nrow = length(field_data[[1]]), ncol = 1)
  
  # Will be one vector per variable.  Each variable is its own list entry.
  obs.mean <- list()
  
  for (i in 1:length(field_data)) {
    uncertainty_vals[[i]] <- PEcAn.uncertainty::flux.uncertainty(field_data[[i]], QC = rep(0, length(field_data[[i]])))
    numvals <- 10;
    
    # Create proxy row for rbinding
    random_mat <- matrix(rep(0, numvals), nrow = 1, ncol = numvals)
    for(j in 1:length(field_data[[i]])) {
      random_range = seq(min(uncertainty_vals[[i]]$err, na.rm=TRUE), max(uncertainty_vals[[i]]$err, na.rm=TRUE), by=abs(mean(uncertainty_vals[[i]]$mag))/100)
      row <- sample(random_range, numvals, replace=TRUE)
      random_mat <- rbind(random_mat, row)
    }
    
    #Strip away proxy row
    random_mat <- random_mat[-1,]
    
    obs.mean[[i]] <- cumsum(field_data[[i]])
    
    applied <- apply(random_mat, 1, sum)
    print(sums)
    print(nrow(sums))
    print("------------------------")
    print(applied)
    print(length(applied))
    
    sums = cbind(sums, apply(random_mat, 1, sum))  ### cumsum?
  } ### cumsum produces a matrix instead of a vector.
  
  # strip away proxy row
  sums = sums[-1,]
  
  ### Note that this will mean obs.cov will have length 1, while obs.mean will have a length equal to the number of variables
  obs.cov <- list(cov(sums))
  
  ### suspend function call until function works to this point
  # PEcAn.assim.sequential::sda.enkf(settings, obs.cov = obs.cov, obs.mean = obs.mean)
} # prep.data.assim

settings = list(run = list(start_date = '2018-07-19', end_date = '2018-07-30'))
prep.data.assim(settings)
