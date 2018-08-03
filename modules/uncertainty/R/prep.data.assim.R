##'@title prep.data.assim
##'@section purpose:
##'This function sets up a call to PEcAn.assim.sequential::sda.enkf()
##'
##'@param settings the PEcAn settings object (a collection of nested lists)
##'@return None
##'@export
##'@author Luke Dramko
prep.data.assim <- function(settings) {
  # Obtain real data from the site
  timestep <- 1 ### Every 12 hours... it's this way for testing # Every half hour = 0.5
  
  field_data <- PEcAn.data.atmosphere::download.US_WCr(settings$run$start_date, settings$run$end_date, timestep = timestep)
  
  uncertainty_vals <- list()
  
  # Creates a proxy row for rbinding
  sums <- matrix(rep(0, length(field_data[[1]])), nrow = length(field_data[[1]]), ncol = 1)
  
  # Will be one vector per variable.  Each variable is its own list entry.
  obs.mean <- NULL
  
  ### print(field_data)
  
  for (i in 1:length(field_data)) {
    AMF.params <- PEcAn.uncertainty::flux.uncertainty(field_data[[i]], QC = rep(0, length(field_data[[i]])))
    numvals <- 10;
    
    # Create proxy row for rbinding
    #random_mat <- matrix(field_data[[i]], nrow = length(field_data[[i]]), ncol = 1)
    random_mat = NULL
    new_col = rep(0, length(field_data[[i]]))
    
    # Create a new column
    # i: the particular variable being worked with
    # j: the column number
    # k: the row number
    for (j in 1:numvals) { # number of random numbers
      obs <- field_data[[i]][!is.na(field_data[[i]])]
      plot(obs)
      pos <- obs >= 0
      
      res <- obs
      res[pos]  <- rexp(length(obs[pos]),
                        1 / (AMF.params$intercept[[1]] + (AMF.params$slopeP[[1]] * obs[pos])))
      res[!pos]  <- rexp(length(obs[!pos]),
                         1 / (AMF.params$intercept[[1]] + (AMF.params$slopeN[[1]] * obs[!pos])))
      
      random_multiplier <- sample(c(-1,1), length(res), replace = TRUE)
      simulated <- obs+(random_multiplier*res)
      
      ### print("simulated")
      ### print(simulated)
      
      sim_idx = 1; # Because NA's are excluded, real time points don't match up exactly.
      for (k in 1:length(field_data[[i]])) { # k for each real time point
        if (!is.na(field_data[[i]][k])) {
          new_col[k] <- simulated[sim_idx]
          sim_idx = sim_idx + 1
        }
      } # end k
      
      print("Checking lengths")
      print(" new_col")
      print(length(new_col))
      print("random_mat")
      print(nrow(random_mat))
      random_mat <- cbind(random_mat, new_col)
      
    } # end j
    
    
    #for(j in 1:length(field_data[[i]])) {
    #  err <- mean(uncertainty_vals[[i]]$err, na.rm=TRUE)
    #  real <- field_data[[i]][j]
    #  
    #  # Occasionally, there is missing data, which causes issues.
    #  # Samples real data from the same time of day.
    #  if (is.na(real)) {
    #    same <- seq(j %% (24 / timestep), length(field_data[[i]]), by = (24 / timestep))  # Select from same time of day
    #    same <- field_data[[i]][same]
    #    same <- same[!is.na(same)]
    #    real = sample(same, 1)
    #  }
    #  
    #  random_range = seq(real - err, real + err, by = abs(real / 100))
    #  row <- sample(random_range, numvals, replace=TRUE) # Sample from possible random values
    #  random_mat <- rbind(random_mat, row)
    #}
    
    #Strip away proxy row
    ### random_mat <- random_mat[-1,]
    
    obs.mean <- c(obs.mean, mean(field_data[[i]], na.rm = TRUE))
    
    applied <- apply(random_mat, 1, mean, na.rm=TRUE)
    
    print("Covarience Check:")
    print(length(applied))
    print(nrow(sums))
    
    sums = cbind(sums, applied)
  } # end i
  
  # strip away proxy row
  sums = sums[,-1]
  
  print("sums")
  
  ### Note that this will mean obs.cov will have length 1, while obs.mean will have a length equal to the number of variables
  obs.cov <- list(cov(sums))
  
  names(obs.mean) <- names(field_data)
  obs.mean <- list(obs.mean)
  names(obs.mean) <- settings$run$end_date
  
  print("mean")
  print(obs.mean)
  
  print("covarience")
  print(obs.cov)
  
  warnings()
  ### suspend function call until function works to this point
  # PEcAn.assim.sequential::sda.enkf(settings, obs.cov = obs.cov, obs.mean = obs.mean)
} # prep.data.assim

settings <- list(run = list(start_date='2018-07-14', end_date='2018-07-30'))
prep.data.assim(settings)
