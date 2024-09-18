#' Calculate ensemble weights for each site at time t.
#'
#' @param FORECAST FORECAST object built within the sda.enkf_MultiSite function. 
#' @param ANALYSIS ANALYSIS object built within the Analysis_sda_multisite function. 
#' @param t exact number of t inside the sda.enkf_MultiSite function.
#' @param ens number of ensemble members.
#'
#' @return list of weights associated with each ensemble member of each site.
#' @export
#'
#' @author Dongchen Zhang and Hamze Dokoohaki
sda_weights_site <- function(FORECAST, ANALYSIS, t, ens){
  #This function is the refactored version 
  #of the original code "Weights_Site.R" written by Hamzed.
  #read site ids from forecast results.
  site.ids <- attr(FORECAST[[1]],'Site') %>% unique() 
  Year.applid.weight <- names(FORECAST)[t]
  
  #calculate weights for each ensemble member of each site at time point t.
  Weights.new <- site.ids %>%
    future_map_dfr(function(one.site){
      #match site id
      site.ind <- which(attr(FORECAST[[1]],'Site') %in% one.site)
      #match date
      ind <- which(names(FORECAST) %in% Year.applid.weight)
      
      #if we only have single variable.
      if(length(site.ind) == 1){
        #calculate analysis mean value
        mu.a <- mean(ANALYSIS[[t]][,site.ind])
        #calculate analysis variance
        Pa <- stats::sd(ANALYSIS[[t]][,site.ind])
        #calculate weights
        w <- stats::dnorm(FORECAST.r[,site.ind], mu.a, Pa, TRUE)
      }else{
        #calculate analysis mean value
        mu.a <- apply(ANALYSIS[[t]][,site.ind],2 ,mean)
        #calculate analysis covariance matrix
        Pa <- stats::cov(ANALYSIS[[t]][,site.ind])
        #calculate weights
        w <- mvtnorm::dmvnorm(x = FORECAST[[t]][,site.ind], mean = mu.a, sigma = Pa, log = TRUE)
      }
      #return outputs
      data.frame(
        ens = 1:ens,
        raw_weight=w,
        Site= one.site,
        Relative_weight=abs(w)/sum(abs(w)),
        Year=lubridate::year(Year.applid.weight)
      )
    }, .progress = TRUE)
  Weights.new
}