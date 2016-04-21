##' @name align.data
##' @title align.data
##' @export
##' @param model
##' @param obvs
##' @author Betsy Cowdery


align.data <- function(model, obvs, vars_used, start_year, end_year){

  t <- which(vars_used$pecan_name == "time")
  
  
  # We know that the model output time is days since the beginning of the year.
  # Make a column of years to supplement the column of days of the year.
  
  years <- start_year:end_year
  Diff <- diff(model$time)
  n <- model$time[c(which(Diff < 0), length(model$time))]
  y <- c()
  for(i in 1:length(n)){
    y <- c(y, rep(years[i], n[i]))
  }
  model$year <- y
  
  
  ##############################
  
  obvs_time <- obvs[,vars_used$orig_name[t]]
  # Should we make a lookup table that converts between udunits and
  m_obvs <- min(diff(strptime(as.character(sort(unique(obvs_time))), format = "%Y")))
  units(m_obvs) <- "secs"
  m_obvs <- as.numeric(m_obvs)
  mode_obvs <- m_obvs[which.max(tabulate(match(unique(m_obvs), m_obvs)))]

  m_model <- diff(strptime(paste(model$time, model$year), format = "%j %Y"))
  # DALEC isn't outputting the right number of days whyyyyyyyy
  units(m_model) <- "secs"
  m_model <- as.numeric(m_model)
  mode_model <- m_model[which.max(tabulate(match(unique(m_model), m_model)))]


  # Compare timestep sizes,
  # choose the smaller of the two
  # then choose the appropriate conversion function

  min(mode_model,mode_obvs)
  
  ###################################################

  # Then big theoretical leap to get me here O.o
  
  out_model <- as.data.frame(matrix(NA, ncol = length(vars_used$pecan_name), nrow = length(unique(model$year))))
  colnames(out_model) <- paste(vars_used$pecan_name, "model", sep = "_")
  out_model$years <- sort(unique(model$year))
  for(i in 1:nrow(vars_used)){
    v <- vars_used$pecan_name[i]
    out_model[,paste(v,"model",sep="_")] <- aggregate(model[,v], by=list(model$year), FUN=mean, na.rm=TRUE)[,2]
  }
  
  colnames(obvs)[which(names(obvs)=="YEAR")] <- "time"
  out_obvs <- as.data.frame(matrix(NA, ncol = length(vars_used$pecan_name), nrow = length(unique(obvs$time))))
  colnames(out_obvs) <- paste(vars_used$pecan_name, "obvs", sep = "_")
  out_obvs$years <- sort(unique(obvs$time))
  for(i in 1:nrow(vars_used)){
    v <- vars_used$pecan_name[i]
    print(v)
    print(paste(v,"obvs",sep="_"))
    out_obvs[,paste(v,"obvs",sep="_")] <- aggregate(obvs[,v], by=list(obvs$time), FUN=mean, na.rm=TRUE)[,2]
  }
  
  
  out <- merge(out_model, out_obvs, by = "years", all = TRUE)
  colnames(out)[which(names(out)=="years")] <- "time" 
   out$time_model  <- NULL
   out$time_obvs  <- NULL
  return(out)
}