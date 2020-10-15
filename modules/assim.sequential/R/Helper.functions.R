

#' outlier.detector.boxplot
#'
#' @param X A list of dataframes
#' @description This function performs a simple outlier replacement on all the columns of dataframes inside a list
#' @return A list the same dimension as X, with each column of each dataframe
#'   modified by replacing outlier points with the column median
#' @export
#'
outlier.detector.boxplot<-function(X) {
  X <- X  %>% 
    map(function(X.tmp){
      #X.tmp is all the state variables for each element of the list (site)
      X.tmp %>%
        map_dfc(function(col.tmp){
          #naive way of finding the outlier - 3 * IQR
          OutVals <- boxplot(col.tmp, plot = FALSE)$out
          # if I make this NA then it would stay NA for ever.
          #bc adjustment uses X to and comes up with new analysis
          col.tmp[which((col.tmp %in% OutVals))] <- median(col.tmp, na.rm = TRUE)
          col.tmp
        })
      
    })
  
  return(X)
}


#' SDA_control
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
#' @param OutlierDetection Logical if TRUE then a simple method will be used to replace simulations of outside 3IQR with the median of ensembles.
#'
#' @return list of all arguments needed to setup the SDA function
#' @export
#'
SDA_control <-
  function(trace = TRUE,
           ForewardForecast = FALSE,
           interactivePlot = FALSE,
           TimeseriesPlot = FALSE,
           BiasPlot = FALSE,
           plot.title = NULL,
           facet.plots = FALSE,
           debug = FALSE,
           pause = FALSE,
           Profiling = FALSE,
           OutlierDetection=FALSE) {
    
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




#' rescaling_stateVars
#'
#' @param settings pecan xml settings where state variables have the scaling_factor tag
#' @param X Any Matrix with column names as variable names
#' @description This function uses a set of scaling factors defined in the pecan XML to scale a given matrix
#' @return
#' @export
#'
rescaling_stateVars <- function(settings, X, multiply=TRUE) {
  
  FUN <- ifelse(multiply, .Primitive('*'), .Primitive('/'))
    
  
  # Finding the scaling factors
  scaling.factors <-
    settings$state.data.assimilation$state.variables %>%
    purrr::map('scaling_factor') %>%
    setNames(settings$state.data.assimilation$state.variables %>%
               purrr::map('variable.name')) %>%
    purrr::discard(is.null)
  
  if (length(scaling.factors) == 0)  return(X)
  
  
  Y <- seq_len(ncol(X)) %>%
    purrr::map_dfc(function(.x) {
      
      if(colnames(X)[.x] %in% names(scaling.factors))  {
        # This function either multiplies or divides
        FUN( X[, .x], scaling.factors[[colnames(X)[.x]]] %>% as.numeric())
      }else{
        X[, .x]
      }
    }) %>%
    as.matrix() %>%
    `colnames<-`(colnames(X))
  
  try({
    # I'm trying to give the new transform variable the attributes of the old one
    # X for example has `site` attribute
    
    attr.X <- names(attributes(X)) %>%
      purrr::discard( ~ .x %in% c("dim", "dimnames"))
    
    if (length(attr.X) > 0) {
      attr(Y, attr.X) <- attr(X, attr.X)
    }
    
  }, silent = TRUE)
  
  return(Y)
}

