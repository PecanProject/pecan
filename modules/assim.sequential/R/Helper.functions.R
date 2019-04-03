

#' outlier.detector.boxplot
#'
#' @param X A list of dataframes
#' @description This function performs a simple outlier replacement on all the colmuns of dataframes inside a list
#' @return
#' @export
#'
outlier.detector.boxplot<-function(X) {
  X<-X %>% 
    map(function(X.tmp){
      #X.tmp is all the state variables for each element of the list (site)
      X.tmp %>%
        map_dfc(function(col.tmp){
          #naive way of finding the outlier - 10 * IQR
          OutVals <- boxplot(col.tmp, plot=FALSE)$out
          # if I make this NA then it would stay NA for ever.
          #bc adjustment uses X to and comes up with new analysis
          col.tmp[which((col.tmp %in% OutVals))] <- median(col.tmp, na.rm=T)
          col.tmp
        })
      
    })
  
  return(X)
}


# finding outlier based on last step ensembles.
# if (t>1){
#   site.ids %>%
#     walk(function(site){# walking thorugh the sites
#       # columns of X for this site
#       col.inds <- which(attr(FORECAST[[t - 1]], 'Site') %in% site)
#       # previous step X
#       old.X <- (FORECAST[[t - 1]])[, col.inds]
#       #Foreach column
#       X[, col.inds] <<- col.inds %>%
#         map_dfc(function(col) {
#           
#           tmp.v <- X[, col]
#           # if there was an ens which was higher or lower of 3*sd of mean of last step
#           HB <-
#             mean(FORECAST[[t - 1]][, col], na.rm = T) + (3 * sd(FORECAST[[t - 1]][, col] , na.rm =
#                                                                   T))
#           # not checking for lower because of disturbance
#           #LB <- mean(FORECAST[[t - 1]][, col], na.rm=T) - (2*sd(FORECAST[[t - 1]][, col] , na.rm=T))
#           outies <- which(tmp.v > HB)
#           # if there was outliers replace them with the median of previous step
#           if (length(outies) > 0)
#             tmp.v[outies] <- median(FORECAST[[t - 1]][, col], na.rm = T)
#           
#           tmp.v
#           
#         }) %>%
#         as.matrix()
#       
#     })
# }