

#' outlier.detector.boxplot
#'
#' @param X A list of dataframes
#' @description This function performs a simple outlier replacement on all the colmuns of dataframes inside a list
#' @return
#' @export
#'
outlier.detector.boxplot<-function(X) {
  X <- X  %>% 
    map(function(X.tmp){
      #X.tmp is all the state variables for each element of the list (site)
      X.tmp %>%
        map_dfc(function(col.tmp){
          #naive way of finding the outlier - 10 * IQR
          OutVals <- boxplot(col.tmp, plot = FALSE)$out
          # if I make this NA then it would stay NA for ever.
          #bc adjustment uses X to and comes up with new analysis
          col.tmp[which((col.tmp %in% OutVals))] <- median(col.tmp, na.rm = TRUE)
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


#' SDA.arguments
#'
#' @param trace Logical if code should print out the progress of SDA .
#' @param ForewardForecast Logical if the foreward forecast estimates needs to be read and visulized in time series plots.
#' @param interactivePlot Logical if time series plots need to be generated.
#' @param TimeseriesPlot Logical if time series plots need to be generated.
#' @param BiasPlot Logical if bias plots need to be generated
#' @param plot.title Character defining the title of times series plots
#' @param facet.plots Logical if the timeseries plots should be faceted based on state variables
#' @param debug Logical if the code should stop at some milestones and open an interactive debugging environment
#' @param pause Logical if code needs to be puased and wait for further instruction after the analysis step
#' @param Profiling Logical if code should keep track of how much time each step took
#'
#' @return list of all arguments needed to setup the SDA function
#' @export
#'
SDA.arguments <-
  function(trace = TRUE,
           ForewardForecast = FALSE,
           interactivePlot = FALSE,
           TimeseriesPlot = FALSE,
           BiasPlot = FALSE,
           plot.title = NULL,
           facet.plots = FALSE,
           debug = FALSE,
           pause = FALSE,
           Profiling = FALSE) {
    
    return(
      list(
        trace = trace,
        FF = ForewardForecast,
        interactivePlot = interactivePlot,
        TimeseriesPlot = TimeseriesPlot,
        BiasPlot = BiasPlot,
        plot.title = plot.title,
        facet.plots = facet.plots,
        debug = debug,
        pause = pause,
        Profiling = Profiling
      )
    )
  }



