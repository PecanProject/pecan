##'@title prep.data.assim
##'@section purpose:
##'This function sets up a call to PEcAn.assim.sequential::sda.enkf()
##'
##'@param settings the PEcAn settings object (a collection of nested lists)
##'@param numvals number of simulated data points for each time point
##'@return None
##'@export
##'@author Luke Dramko
prep.data.assim <- function(settings, numvals) {
  # Obtain real data from the site
  timestep <- 0.5 # Every half hour = 0.5
  
  field_data <- PEcAn.data.atmosphere::download.US_WCr(settings$run$start.date, settings$run$end.date, timestep = timestep)
  
  uncertainty_vals <- list()
  
  # Creates a proxy row for rbinding
  sums <- NULL
  
  # One vector holds the mean for each variable.
  obs.mean <- NULL
  
  for (i in 1:length(field_data)) {
    AMF.params <- PEcAn.uncertainty::flux.uncertainty(field_data[[i]], QC = rep(0, length(field_data[[i]])))
    
    # Create proxy row for rbinding
    random_mat = NULL
    new_col = rep(0, length(field_data[[i]]))
    
    # Create a new column
    # i: the particular variable being worked with
    # j: the column number
    # k: the row number
    for (j in 1:numvals) { # number of random numbers
      obs <- field_data[[i]][!is.na(field_data[[i]])]
      pos <- obs >= 0
      
      res <- obs
      res[pos]  <- rexp(length(obs[pos]),
                        1 / (AMF.params$intercept[[1]] + (AMF.params$slopeP[[1]] * obs[pos])))
      res[!pos]  <- rexp(length(obs[!pos]),
                         1 / (AMF.params$intercept[[1]] + (AMF.params$slopeN[[1]] * obs[!pos])))
      
      random_multiplier <- sample(c(-1,1), length(res), replace = TRUE)
      simulated <- obs+(random_multiplier*res)
      
      sim_idx = 1; # Because NA's are excluded, real time points don't match up exactly.
      for (k in 1:length(field_data[[i]])) { # k for each real time point
        if (!is.na(field_data[[i]][k])) {
          new_col[k] <- simulated[sim_idx]
          sim_idx = sim_idx + 1
        }
      } # end k
      
      random_mat = cbind(random_mat, new_col)
    } # end j
    
    obs.mean <- c(obs.mean, mean(field_data[[i]], na.rm = TRUE))
    
    applied <- apply(random_mat, 1, mean, na.rm=TRUE)
    sums = cbind(sums, applied)
  } # end i
  
  # Remove NA's
  sums = sums[complete.cases(sums), ]
  
  obs.cov <- list(cov(sums))
  names(obs.cov) <- settings$run$end_date
  
  names(obs.mean) <- names(field_data)
  obs.mean <- list(obs.mean)
  names(obs.mean) <- settings$run$end_date
  
  PEcAn.logger::logger.info("Calcualted obs.mean")
  print(obs.mean)
  PEcAn.logger::logger.info("Calcualted obs.cov")
  print(obs.cov)
  
  PEcAn.assim.sequential::sda.enkf(settings, obs.cov = obs.cov, obs.mean = obs.mean)
} # prep.data.assim
