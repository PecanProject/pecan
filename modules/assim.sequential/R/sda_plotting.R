# @author Ann Raiho
# @description This function generates a series of colors. This is mainly used in AssimSequential package.
generate_colors_sda <-function(){
  pink        <- col2rgb("deeppink")
  alphapink   <- rgb(pink[1], pink[2], pink[3], 180, max = 255)
  green       <- col2rgb("green")
  alphagreen  <- rgb(green[1], green[2], green[3], 75, max = 255)
  blue        <- col2rgb("blue")
  alphablue   <- rgb(blue[1], blue[2], blue[3], 75, max = 255)
  purple      <- col2rgb("purple")
  alphapurple <- rgb(purple[1], purple[2], purple[3], 75, max = 255)
  brown       <- col2rgb("brown")
  alphabrown  <- rgb(brown[1], brown[2], brown[3], 30, max = 255)

  return(list(
    pink = alphapink,
    green = alphagreen,
    blue = alphablue,
    purple = alphapurple,
    brown = alphabrown))
}


##' Internal functions for plotting SDA outputs. Interactive, post analysis time-series and bias plots in base plotting system and ggplot
##' @param settings  pecan standard settings list.  
##' @param t current time - int number giving the position of the current time in obs.time. 
##' @param obs.times vector of dates of measurements
##' @param obs.mean list of vectors of the means of observed data named by the measured date.
##' @param obs.cov list of cov matrices of the observed data named by the measured date.
##' @param obs list containing the mean and cov object
##' @param X  dataframe of state variables for each ensemble
##' @param FORECAST dataframe of state variables for each ensemble
##' @param ANALYSIS  vector of mean of state variable after analysis
##' @param plot.title character giving the title for post visualization ggplots
##' @param Add_Map Bool variable decide if we want to export the GIS map of Ecoregion.
##' @export

interactive.plotting.sda<-function(settings, t, obs.times, obs.mean, obs.cov, obs, X, FORECAST, ANALYSIS){

  if (!requireNamespace("plyr", quietly = TRUE)) {
    PEcAn.logger::logger.error(
      "Can't find package 'plyr',",
      "needed by `PEcAnAssimSequential::interactive.plotting.sda()`.",
      "Please install it and try again.")
  }
  if (!requireNamespace("PEcAn.visualization", quietly = TRUE)) {
    PEcAn.logger::logger.error(
      "Can't find package 'PEcAn.visualization',",
      "needed by `PEcAnAssimSequential::interactive.plotting.sda()`.",
      "Please install it and try again.")
  }
  #Defining some colors
  sda_colors <- generate_colors_sda()
  t1 <- 1
  var.names <- var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  names.y <- unique(unlist(lapply(obs.mean[t1:t], function(x) { names(x) })))
  
  Ybar <- t(sapply(obs.mean[t1:t], function(x) {
    tmp <- rep(NA, length(names.y))
    names(tmp) <- names.y
    mch <- match(names(x), names.y)
    tmp[mch] <- x[mch]
    tmp
  }))
  
  if(any(obs)){
    Y.order <- stats::na.omit(pmatch(colnames(X), colnames(Ybar)))
    Ybar <- Ybar[,Y.order]
    Ybar[is.na(Ybar)] <- 0
    YCI <- t(as.matrix(sapply(obs.cov[t1:t], function(x) {
      if (length(x)<2) {
        rep(NA, length(names.y))
      }
      sqrt(diag(x))
    })))
    
    YCI <- YCI[,Y.order]
    YCI[is.na(YCI)] <- 0
    
  }else{
    YCI <- matrix(NA,nrow=length(t1:t), ncol=max(length(names.y),1))
  }
  
  graphics::par(mfrow = c(2, 1))
  colmax<-2
  for (i in 1:ncol(FORECAST[[t]])) { #
    
    Xbar <- plyr::laply(FORECAST[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    Xci  <- plyr::laply(FORECAST[t1:t], function(x) { stats::quantile(x[, i], c(0.025, 0.975), na.rm = TRUE) })
    
    Xa <- plyr::laply(ANALYSIS[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    XaCI <- plyr::laply(ANALYSIS[t1:t], function(x) { stats::quantile(x[, i], c(0.025, 0.975), na.rm = TRUE) })
    
    ylab.names <- unlist(sapply(settings$state.data.assimilation$state.variable, 
                                function(x) { x })[2, ], use.names = FALSE)
    
    # observation / data
    if (i <= ncol(Ybar) & any(obs)) {
      #browser()
      plot(as.Date(obs.times[t1:t]), 
           Xbar, 
           ylim = range(c(XaCI, Xci, Ybar[,i]), na.rm = TRUE), 
           type = "n", 
           xlab = "Year", 
           ylab = ylab.names[grep(colnames(X)[i], var.names)], 
           main = colnames(X)[i])
      PEcAn.visualization::ciEnvelope(as.Date(obs.times[t1:t]),
                 as.numeric(Ybar[, i]) - as.numeric(YCI[, i]) * 1.96, 
                 as.numeric(Ybar[, i]) + as.numeric(YCI[, i]) * 1.96, 
                 col = sda_colors$green)
      graphics::lines(as.Date(obs.times[t1:t]), 
            as.numeric(Ybar[, i]), 
            type = "l", 
            col = "darkgreen", 
            lwd = 2)
    }else{
      plot(as.Date(obs.times[t1:t]), 
           Xbar, 
           ylim = range(c(XaCI, Xci), na.rm = TRUE), 
           type = "n", 
           xlab = "Year", 
           ylab = ylab.names[grep(colnames(X)[i], var.names)], 
           main = colnames(X)[i])
    }
    
    # forecast
    PEcAn.visualization::ciEnvelope(as.Date(obs.times[t1:t]), Xci[, 1], Xci[, 2], col = sda_colors$blue)  #col='lightblue')
    graphics::lines(as.Date(obs.times[t1:t]), Xbar, col = "darkblue", type = "l", lwd = 2)
    
    # analysis
    PEcAn.visualization::ciEnvelope(as.Date(obs.times[t1:t]), XaCI[, 1], XaCI[, 2], col = sda_colors$pink)
    graphics::lines(as.Date(obs.times[t1:t]), Xa, col = "black", lty = 2, lwd = 2)
    #legend('topright', c('Forecast','Data','Analysis'), col=c(sda_colors$blue, sda_colors$green, sda_colors$pink), lty=1, lwd=5)
  }
}

##' @rdname interactive.plotting.sda
##' @export

postana.timeser.plotting.sda<-function(settings, t, obs.times, obs.mean, obs.cov, obs, X, FORECAST, ANALYSIS){

  if (!requireNamespace("plyr", quietly = TRUE)) {
    PEcAn.logger::logger.error(
      "Can't find package 'plyr',",
      "needed by `PEcAnAssimSequential::postana.timeser.plotting.sda()`.",
      "Please install it and try again.")
  }
  if (!requireNamespace("PEcAn.visualization", quietly = TRUE)) {
    PEcAn.logger::logger.error(
      "Can't find package 'PEcAn.visualization',",
      "needed by `PEcAnAssimSequential::postana.timeser.plotting.sda()`.",
      "Please install it and try again.")
  }

  #Defining some colors
  sda_colors <- generate_colors_sda()
  t1 <- 1
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  #----
  grDevices::pdf(file.path(settings$outdir,"SDA", "sda.enkf.time-series.pdf"))
  names.y <- unique(unlist(lapply(obs.mean[t1:t], function(x) { names(x) })))
  Ybar <- t(sapply(obs.mean[t1:t], function(x) {
    tmp <- rep(NA, length(names.y))
    names(tmp) <- names.y
    mch <- match(names(x), names.y)
    tmp[mch] <- x[mch]
    tmp
  }))
  #Y.order <- na.omit(pmatch(colnames(FORECAST[[t]]), colnames(Ybar)))
  Y.order <- sapply(colnames(FORECAST[[t]]),agrep,x=colnames(Ybar),max=2,USE.NAMES = F)%>%unlist
  Ybar <- Ybar[,Y.order]
  YCI <- t(as.matrix(sapply(obs.cov[t1:t], function(x) {
    if (is.na(x)) {
      rep(NA, length(names.y))
    } else {
    sqrt(diag(x))
    }
  })))
  
  Ybar[is.na(Ybar)]<-0
  YCI[is.na(YCI)]<-0
  
  YCI <- YCI[,c(Y.order)]
  
  
  
  Xsum <- plyr::laply(FORECAST, function(x) { mean(rowSums(x[,1:length(names.y)], na.rm = TRUE)) })[t1:t]
  Xasum <- plyr::laply(ANALYSIS, function(x) { mean(rowSums(x[,1:length(names.y)], na.rm = TRUE)) })[t1:t]

  #------For each state variable 
  for (i in seq_len(ncol(X))) {
    Xbar <- plyr::laply(FORECAST[t1:t], function(x) {
      mean(x[, i], na.rm = TRUE) }) #/rowSums(x[,1:9],na.rm = T)
    Xci <- plyr::laply(FORECAST[t1:t], function(x) { 
      stats::quantile(x[, i], c(0.025, 0.975),na.rm = T) })
    
    Xci[is.na(Xci)]<-0
    
    Xbar <- Xbar
    Xci <- Xci
    
    Xa <- plyr::laply(ANALYSIS[t1:t], function(x) { 
      mean(x[, i],na.rm = T) })
    
    XaCI <- plyr::laply(ANALYSIS[t1:t], function(x) { 
      stats::quantile(x[, i], c(0.025, 0.975),na.rm = T )})
    
    Xa <- Xa
    XaCI <- XaCI
    
    plot(as.Date(obs.times[t1:t]),
         Xbar, 
         ylim = range(c(XaCI, Xci,Ybar[, 1]), na.rm = TRUE),
         type = "n", 
         xlab = "Year", 
         #ylab = ylab.names[grep(colnames(X)[i], var.names)],
         main = colnames(X)[i])
    
    # observation / data
    if (i<=ncol(X)) { #
      PEcAn.visualization::ciEnvelope(as.Date(obs.times[t1:t]), 
                 as.numeric(Ybar[, i]) - as.numeric(YCI[, i]) * 1.96, 
                 as.numeric(Ybar[, i]) + as.numeric(YCI[, i]) * 1.96, 
                 col = sda_colors$green)
      graphics::lines(as.Date(obs.times[t1:t]), 
            as.numeric(Ybar[, i]), 
            type = "l", col = "darkgreen", lwd = 2)
    }
    
    # forecast
    PEcAn.visualization::ciEnvelope(as.Date(obs.times[t1:t]), Xci[, 1], Xci[, 2], col = sda_colors$blue)  #col='lightblue') #alphablue
    graphics::lines(as.Date(obs.times[t1:t]), Xbar, col = "darkblue", type = "l", lwd = 2) #"darkblue"
    
    # analysis
    PEcAn.visualization::ciEnvelope(as.Date(obs.times[t1:t]), XaCI[, 1], XaCI[, 2], col = sda_colors$pink) #alphapink
    graphics::lines(as.Date(obs.times[t1:t]), Xa, col = "black", lty = 2, lwd = 2) #"black"
    
    graphics::legend(
      'topright',
      c('Forecast', 'Data', 'Analysis'),
      col=c(sda_colors$blue, sda_colors$green, sda_colors$pink),
      lty=1,
      lwd=5)
    
  }
  
  grDevices::dev.off()
  
}

##' @rdname interactive.plotting.sda
##' @export

postana.bias.plotting.sda<-function(settings, t, obs.times, obs.mean, obs.cov, obs, X, FORECAST, ANALYSIS){

  if (!requireNamespace("plyr", quietly = TRUE)) {
    PEcAn.logger::logger.error(
      "Can't find package 'plyr',",
      "needed by `PEcAnAssimSequential::postana.bias.plotting.sda()`.",
      "Please install it and try again.")
  }
  if (!requireNamespace("PEcAn.visualization", quietly = TRUE)) {
    PEcAn.logger::logger.error(
      "Can't find package 'PEcan.visualization',",
      "needed by `PEcAnAssimSequential::postana.bias.plotting.sda()`.",
      "Please install it and try again.")
  }

  #Defining some colors
  sda_colors <- generate_colors_sda()
  t1 <- 1
  ylab.names <- unlist(sapply(settings$state.data.assimilation$state.variable, 
                              function(x) { x })[2, ], use.names = FALSE)
  names.y <- unique(unlist(lapply(obs.mean[t1:t], function(x) { names(x) })))
  Ybar <- t(sapply(obs.mean[t1:t], function(x) {
    tmp <- rep(NA, length(names.y))
    names(tmp) <- names.y
    mch <- match(names(x), names.y)
    tmp[mch] <- x[mch]
    tmp
  }))
  #----
  grDevices::pdf(file.path(settings$outdir,"SDA", "bias.diagnostic.pdf"))
  for (i in seq_along(obs.mean[[1]])) {
    Xbar <- plyr::laply(FORECAST[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    Xci <- plyr::laply(FORECAST[t1:t], function(x) { stats::quantile(x[, i], c(0.025, 0.975)) })
    
    Xa <- plyr::laply(ANALYSIS[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    XaCI <- plyr::laply(ANALYSIS[t1:t], function(x) { stats::quantile(x[, i], c(0.025, 0.975)) })
    
    if(length(which(is.na(Ybar[,i])))>=length(t1:t)) next()
    reg <- stats::lm(Xbar[t1:t] - unlist(Ybar[, i]) ~ c(t1:t))
    plot(t1:t, 
         Xbar - unlist(Ybar[, i]),
         pch = 16, cex = 1, 
         ylim = c(min(Xci[, 1] - unlist(Ybar[, i])), max(Xci[,2] - unlist(Ybar[, i]))), 
         xlab = "Time", 
         ylab = "Error", 
         main = paste(colnames(X)[i], " Error = Forecast - Data"))
    PEcAn.visualization::ciEnvelope(rev(t1:t), 
               rev(Xci[, 1] - unlist(Ybar[, i])), 
               rev(Xci[, 2] - unlist(Ybar[, i])),
               col = sda_colors$brown)
    graphics::abline(h = 0, lty = 2, lwd = 2)
    graphics::abline(reg)
    graphics::mtext(paste(
      "slope =", signif(summary(reg)$coefficients[2], digits = 3),
      "intercept =", signif(summary(reg)$coefficients[1], digits = 3)))
    # d<-density(c(Xbar[t1:t] - unlist(Ybar[t1:t,i]))) lines(d$y+1,d$x)
    
    # forecast minus analysis = update
    reg1 <- stats::lm(Xbar - Xa ~ c(t1:t))
    plot(t1:t, 
         Xbar - Xa, 
         pch = 16, cex = 1, 
         ylim = c(min(Xbar - XaCI[, 2]), max(Xbar - XaCI[, 1])), 
         xlab = "Time", ylab = "Update", 
         main = paste(colnames(X)[i], 
                      "Update = Forecast - Analysis"))
    PEcAn.visualization::ciEnvelope(rev(t1:t), 
               rev(Xbar - XaCI[, 1]), 
               rev(Xbar - XaCI[, 2]), 
               col = sda_colors$purple)
    graphics::abline(h = 0, lty = 2, lwd = 2)
    graphics::abline(reg1)
    graphics::mtext(paste(
      "slope =", signif(summary(reg1)$coefficients[2], digits = 3),
      "intercept =", signif(summary(reg1)$coefficients[1],
                            digits = 3)))
    # d<-density(c(Xbar[t1:t] - Xa[t1:t])) lines(d$y+1,d$x)
  }
  grDevices::dev.off()
  
}

##' @rdname interactive.plotting.sda
##' @export
postana.bias.plotting.sda.corr<-function(t, obs.times, X, aqq, bqq){
  
  t1<- 1
  #Defining some colors
  sda_colors <- generate_colors_sda()

  #---
  grDevices::pdf('SDA/process.var.plots.pdf')
  
  cor.mat <- stats::cov2cor(aqq[t,,] / bqq[t])
  colnames(cor.mat) <- colnames(X)
  rownames(cor.mat) <- colnames(X)
  graphics::par(mfrow = c(1, 1), mai = c(1, 1, 4, 1))
  corrplot::corrplot(cor.mat, type = "upper", tl.srt = 45,order='FPC')
  
  graphics::par(mfrow=c(1,1))   
  plot(as.Date(obs.times[t1:t]), bqq[t1:t],
       pch = 16, cex = 1,
       ylab = "Degrees of Freedom", xlab = "Time")
  
  grDevices::dev.off()
}

##' @rdname interactive.plotting.sda
##' @export

post.analysis.ggplot <- function(settings, t, obs.times, obs.mean, obs.cov, obs, X, FORECAST, ANALYSIS, plot.title=NULL){

  t1 <- 1
  #Defining some colors
  ready.OBS<-NULL
  sda_colors <- generate_colors_sda()
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  #----
  #Analysis & Forcast cleaning and STAT
  All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
  
  ready.FA <-
    c("FORECAST", "ANALYSIS") %>% purrr::map_df(function(listFA) {
      All.my.data[[listFA]] %>% purrr::map_df(function(state.vars) {
        means <- apply(state.vars, 2, mean, na.rm = T)
        CI <- apply(state.vars, 2, stats::quantile, c(0.025, 0.975),
                    na.rm = T)
        rbind(means, CI) %>% t %>% as.data.frame() %>% dplyr::mutate(Variables = paste(colnames(state.vars))) %>%
          tidyr::replace_na(list(0))
      }) %>% dplyr::mutate(
        Type = listFA,
        Date = rep(
          lubridate::ymd_hms(obs.times[t1:t], truncated = 3, tz = "EST"),
          each = colnames((All.my.data[[listFA]])[[1]]) %>%
            length()
        )
      )
    })
  
  
  
  #Observed data
  #first merging mean and conv based on the day
  
  tryCatch({
      ready.OBS<- names(obs.mean)%>%
        purrr::map(~c(obs.mean[.x],obs.cov[.x],.x)%>%
                     stats::setNames(c('means','covs','Date')))%>%
        stats::setNames(names(obs.mean))%>%
        purrr::map_df(function(one.day.data){
          #CI
          
          purrr::map2_df(sqrt(diag(one.day.data$covs)), one.day.data$means,
                         function(sd, mean){
                           data.frame(mean-(sd*1.96), mean+(sd*1.96))
                           
                         })%>%
            dplyr::mutate(Variables=names(one.day.data$means))%>%
            `colnames<-`(c('2.5%', '97.5%', 'Variables'))%>%
            dplyr::mutate(means=one.day.data$means%>%unlist,
                   Type="Data",
                   Date=one.day.data$Date%>%as.POSIXct(tz="EST"))
          
          
        })
    },
    error = function(e) {
      ready.OBS<-NULL
    }
  )

  ready.to.plot <- ready.OBS %>%
    dplyr::bind_rows(ready.FA)
  
  #Adding the units to the variables
  ready.to.plot$Variable %>% unique() %>% 
    purrr::walk(function(varin){
      #find the unit
      unitp <- which(lapply(settings$state.data.assimilation$state.variable, "[", 'variable.name') %>% unlist %in% varin)
      if (length(unitp)>0) {
        unit <- settings$state.data.assimilation$state.variable[[unitp]]$unit
        
        #replace it in the dataframe
        ready.to.plot$Variable[ready.to.plot$Variable==varin] <<- paste(varin,"(",unit,")")
      }
      
    })
  
  

  p <- ready.to.plot %>%
    ggplot2::ggplot(ggplot2::aes(x = Date)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$`2.5%`, ymax = .data$`97.5%`, fill = .data$Type),
      color = "black") +
    ggplot2::geom_line(ggplot2::aes(y = .data$means, color = .data$Type), lwd = 1.02, linetype = 2) +
    ggplot2::geom_point(ggplot2::aes(y = .data$means, color = .data$Type), size = 3, alpha = 0.75) +
    ggplot2::scale_fill_manual(values = c(sda_colors$pink, sda_colors$green, sda_colors$blue), name = "") +
    ggplot2::scale_color_manual(values = c(sda_colors$pink, sda_colors$green, sda_colors$blue), name = "") +
    ggplot2::theme_bw(base_size = 17) +
    ggplot2::facet_wrap(~.data$Variables, scales = "free", ncol = 2) +
    ggplot2::theme(legend.position = "top", strip.background = ggplot2::element_blank())
  if (!is.null(plot.title)) {
    p <- p + ggplot2::labs(title = plot.title)
  }


  
  
  grDevices::pdf("SDA/SDA.pdf", width = 14, height = 10, onefile = TRUE)
  print(p)
  grDevices::dev.off()
  
  #saving plot data
  save(p, ready.to.plot, file = file.path(settings$outdir,"SDA", "timeseries.plot.data.Rdata"))
  
  
}

##' @rdname interactive.plotting.sda
##' @export
post.analysis.ggplot.violin <- function(settings, t, obs.times, obs.mean, obs.cov, obs, X, FORECAST, ANALYSIS, plot.title=NULL){

  t1 <- 1 
  #Defining some colors
  sda_colors <- generate_colors_sda()
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")

#rearranging the forcast and analysis data  

  All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
  
  ready.FA <- c('FORECAST','ANALYSIS')%>%
    purrr::map_df(function(listFA){
      All.my.data[[listFA]]%>%
        purrr::map_df(function(state.vars){
          state.vars%>%as.data.frame()
        })%>%dplyr::mutate(Type=listFA,
                    Date=rep(obs.times[t1:t], each=((All.my.data[[listFA]])[[1]]) %>% nrow())
        )
      
    })%>%
    tidyr::gather(key = "Variables", value = "Value", -c("Type", "Date"))
  #Observed data
  #first merging mean and conv based on the day
  obs.df <- names(obs.mean)%>%
    purrr::map(~c(obs.mean[.x], obs.cov[.x], .x)%>%
                 stats::setNames(c('means','covs','Date')))%>%
    stats::setNames(names(obs.mean))%>%
    purrr::map_df(function(one.day.data){
      #CI
      purrr::map2_df(sqrt(one.day.data$covs %>% purrr::map( ~ diag(.x)) %>% unlist), one.day.data$means,
                     function(sd,mean){
                       data.frame(mean-(sd*1.96), mean+(sd*1.96))
                       
                     })%>%
        dplyr::mutate(Variables=names(one.day.data$means)) %>%
        `colnames<-`(c('2.5%', '97.5%', 'Variables')) %>%
        dplyr::mutate(means=one.day.data$means %>% unlist,
               Type="Data",
               Date=one.day.data$Date%>%as.POSIXct(tz="UTC"))
      
      
    })#%>%
  #filter(Variables %in% var.names)
  
  #Adding the units to the variables
  ready.FA$Variable %>% unique() %>% 
    purrr::walk(function(varin){
      #find the unit
      unitp <- which(lapply(settings$state.data.assimilation$state.variable, "[", 'variable.name') %>% unlist %in% varin)
      if (length(unitp)>0) {
        unit <- settings$state.data.assimilation$state.variable[[unitp]]$unit
        
        #replace it in the dataframe
        ready.FA$Variable[ready.FA$Variable==varin] <<- paste(varin,"(",unit,")")
      }
      
    })


  p <- ready.FA %>%
    ggplot2::ggplot(ggplot2::aes(.data$Date, .data$Value)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = .data$Date, y = .data$means, ymin = .data$`2.5%`, ymax = .data$`97.5%`, fill = .data$Type),
      data = obs.df,
      color = "black") +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$means, color = .data$Type),
      data = obs.df,
      lwd = 1.02,
      linetype = 2) +
    ggplot2::geom_violin(
      ggplot2::aes(x = .data$Date, fill = .data$Type, group = interaction(.data$Date, .data$Type)),
      position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::geom_jitter(
      ggplot2::aes(color = .data$Type),
      position = ggplot2::position_jitterdodge(dodge.width = 0.9)) +
    ggplot2::scale_fill_manual(values = c(sda_colors$pink, sda_colors$green, sda_colors$blue)) +
    ggplot2::scale_color_manual(values = c(sda_colors$pink, sda_colors$green, sda_colors$blue)) +
    ggplot2::facet_wrap(~.data$Variables, scales = "free", ncol = 2) +
    ggplot2::theme_bw(base_size = 17) +
    ggplot2::theme(legend.position = "top", strip.background = ggplot2::element_blank())
  if (!is.null(plot.title)) {
    p <- p + ggplot2::labs(title = plot.title)
  }

  
  grDevices::pdf("SDA/SDA.Violin.pdf", width = 14, height = 10, onefile = TRUE)
   print(p)
   grDevices::dev.off()
  
  #saving plot data
  save(p, ready.FA, obs.df, file = file.path(settings$outdir,"SDA", "timeseries.violin.plot.data.Rdata"))
  
}

##' @rdname interactive.plotting.sda
##' @export
post.analysis.multisite.ggplot <- function(settings, t, obs.times, obs.mean, obs.cov, FORECAST, ANALYSIS, plot.title=NULL, facetg=FALSE, readsFF=NULL, Add_Map=FALSE){

  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    PEcAn.logger::logger.error(
      "Package `ggrepel` not found, but needed by",
      "PEcAnAssimSequential::post.analysis.multisite.ggplot.",
      "Please install it and try again.")
  }
  
  # fix obs.mean/obs.cov for multivariable plotting issues when there is NA data. When more than 1 data set is assimilated, but there are missing data
  # for some sites/years/etc. the plotting will fail and crash the SDA because the numbers of columns are not consistent across all sublists within obs.mean
  # or obs.cov.
  observed_vars =  vector()
  for (date in names(obs.mean))
  {
    for (site in names(obs.mean[[date]]))
    {
      vars = names(obs.mean[[date]][[site]])
      observed_vars = c(observed_vars, vars)
    }
  }
  observed_vars = unique(observed_vars)
  
  #new diag function: fixed the bug when length==1 then it will return 0x0 matrix
  diag_fix <- function(vector){
    if (length(vector)>1){
      return(diag(vector))
    }else if (length(vector)==1){
      return(vector)
    }
  }
  #bug fixing: detailed commends
  for (name in names(obs.mean)){
    for (site in names(obs.mean[[1]])){
      obs_mean <- obs.mean[[name]][[site]]
      obs_cov <- obs.cov[[name]][[site]]
      if(length(names(obs_mean))<length(observed_vars)){
        missing <- which(!(observed_vars %in% names(obs_mean)))
        not_missing <- which((observed_vars %in% names(obs_mean)))
        
        new_obs_mean <- rep(NA, length(observed_vars))
        new_obs_mean[not_missing] <- obs_mean
        names(new_obs_mean) <- observed_vars
        
        new_obs_cov <- diag(rep(NA, length(new_obs_mean)))
        diag(new_obs_cov)[not_missing] <- diag_fix(obs_cov)
        
        obs.mean[[name]][[site]] <- new_obs_mean
        obs.cov[[name]][[site]] <- new_obs_cov
        next
      }
      obs.mean[[name]][[site]] <- obs_mean
      obs.cov[[name]][[site]] <- obs_cov
    }
  }

  #Defining some colors
  t1 <- 1
  sda_colors <- generate_colors_sda()
  varnames <- settings$state.data.assimilation$state.variable
  #just a check
  if (is.null(varnames)) varnames <- settings[[1]]$state.data.assimilation$state.variable
  
  ylab.names <- unlist(sapply(varnames, 
                              function(x) { x })[2, ], use.names = FALSE)
  
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  site.ids <- attr(FORECAST[[1]], 'Site')
  site.names <- settings %>% purrr::map(~.x[['run']] ) %>% purrr::map('site') %>% purrr::map('name') %>% unlist() %>% as.character()
  
  #------------------------------------------------Data prepration
  #Analysis & Forcast cleaning and STAT
  All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
  
  ready.FA <- c('FORECAST','ANALYSIS')%>%
    purrr::map_df(function(listFA){
      All.my.data[[listFA]]%>%
        purrr::map_df(function(state.vars){
         
          #finding the mean and Ci for all the state variables
          site.ids %>% unique() %>%
            purrr::map_df(function(site){
              (state.vars)[,which(site.ids  %in% site)] %>% 
                as.data.frame %>% 
                dplyr::mutate(Site=site)
            }) %>%
            tidyr::gather(key = "Variable", value = "Value", -c("Site")) %>%
            dplyr::group_by(.data$Site,.data$Variable) %>%
            dplyr::summarise(
              Means = mean(.data$Value, na.rm = TRUE),
              Lower = stats::quantile(.data$Value,0.025, na.rm = TRUE),
              Upper = stats::quantile(.data$Value, 0.975,  na.rm = TRUE))
        }) %>% dplyr::mutate(Type = paste0("SDA_", listFA),
                    Date = rep(as.Date(names(FORECAST)), each = colnames((All.my.data[[listFA]])[[1]]) %>% length() / length(unique(site.ids))) %>% as.POSIXct()
        )
    
    })
      

  obs.var.names <- (obs.mean[[1]])[[1]] %>% names()
  #Observed data
  #first merging mean and conv based on the day
  ready.to.plot <- names(obs.mean)%>%
    purrr::map(~c(obs.mean[.x],obs.cov[.x],.x)%>%
                 stats::setNames(c('means','covs','Date')))%>%
    stats::setNames(names(obs.mean))%>%
    purrr::map_df(function(one.day.data){
      one.day.data$means %>% 
        purrr::map_dfr(~.x) %>% 
        dplyr::mutate(Site = names(one.day.data$means)) %>%
        tidyr::gather(key = "Variable", value = "Means", -c("Site")) %>%
        dplyr::right_join(one.day.data$covs %>% 
                     purrr::map_dfr(~ t(sqrt(as.numeric(diag_fix(.x)))) %>% 
                               data.frame %>% `colnames<-`(c(obs.var.names))) %>%
                     dplyr::mutate(Site = names(one.day.data$covs)) %>%
                     tidyr::gather(key = "Variable", value = "Sd", -c("Site")),
                   by = c('Site', 'Variable')) %>%
        dplyr::mutate(
          Upper = .data$Means + (.data$Sd*1.96),
          Lower = .data$Means - (.data$Sd*1.96)) %>%
        # dropped the "_" from "SDA_Data"
        dplyr::mutate(Type="Data",
               Date=one.day.data$Date %>% as.POSIXct())
        # mutate(Type="SDA_Data",
        #        Date=one.day.data$Date %>% as.POSIXct())
      
      
    })%>% 
    dplyr::select(-.data$Sd) %>%
    dplyr::bind_rows(ready.FA)
  
  #--- Adding the forward forecast
  if (!is.null(readsFF)){
    
    readsFF.df<-readsFF %>%
      purrr::map_df(function(siteX){
    
        siteX %>% purrr::map_df(function(DateX){
          DateX %>% 
            purrr::map_df(~.x %>% t ) %>%
            tidyr::gather(key = "Variable", value = "Value", -c("Date", "Site")) %>%
            dplyr::group_by(.data$Variable,.data$Date, .data$Site) %>%
            dplyr::summarise(
               Means = mean(.data$Value, na.rm = TRUE),
               Lower = stats::quantile(.data$Value, 0.025, na.rm = TRUE),
               Upper = stats::quantile(.data$Value, 0.975, na.rm = TRUE)) %>%
             dplyr::mutate(Type="ForwardForecast")
        })
      })
    
    ready.to.plot <- ready.to.plot %>%
      dplyr::bind_rows(readsFF.df)
    
  }
  
  ready.to.plot$Variable[ready.to.plot$Variable=="LeafC"] <-"leaf_carbon_content"
  

  #Adding the units to the variables
  ready.to.plot$Variable %>% unique() %>% 
    purrr::walk(function(varin){
      #find the unit
      unitp <- which(lapply(settings$state.data.assimilation$state.variable, "[", 'variable.name') %>% unlist %in% varin)
      if (length(unitp)>0) {
        unit <- settings$state.data.assimilation$state.variable[[unitp]]$unit
        
        #replace it in the dataframe
        ready.to.plot$Variable[ready.to.plot$Variable==varin] <<- paste(varin,"(",unit,")")
      }
      
    })
  
  #------------------------------------------- Time series plots
  if (facetg) {
    filew <- 14
    fileh <- 10
    #for each site  and for each variable
    all.plots <- ready.to.plot$Site%>%unique() %>%
      purrr::map(function(site){
            #plotting
            p <- ready.to.plot %>%
              dplyr::filter(.data$Site == site) %>%
              ggplot2::ggplot(ggplot2::aes(x = Date)) +
              ggplot2::geom_ribbon(
                ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper, fill = .data$Type),
                color = "black") +
              ggplot2::geom_line(ggplot2::aes(y = .data$Means, color = .data$Type), lwd = 1.02, linetype = 2) +
              ggplot2::geom_point(ggplot2::aes(y = .data$Means, color = .data$Type), size = 3, alpha = 0.75) +
              ggplot2::scale_fill_manual(
                values = c(sda_colors$brown, sda_colors$pink, sda_colors$green, sda_colors$blue),
                name = "") +
              ggplot2::scale_color_manual(
                values = c(sda_colors$brown, sda_colors$pink, sda_colors$green, sda_colors$blue),
                name = "") +
              ggplot2::theme_bw(base_size = 17) +
              ggplot2::labs(y = "", subtitle=paste0("Site id: ",site)) +
              ggplot2::theme(legend.position = "top", strip.background = ggplot2::element_blank())
            if (!is.null(plot.title)) {
              p <- p + ggplot2::labs(title=plot.title)
            }
            p <- p + ggplot2::facet_wrap(~.data$Variable, ncol = 2, scales = "free_y")

            list(p)
      })

  }else{
    filew <- 10
    fileh <- 8
    #for each site  and for each variable
    all.plots<-ready.to.plot$Site%>%unique() %>%
      purrr::map(function(site){
        ready.to.plot$Variable%>%unique()%>%
          purrr::map(function(vari){
            varin<-vari
            unit<-""
            if (substr(vari,1,8)=="AGB.pft.") varin <- "AGB.pft"
            #finding the unit
            unitp <- which(lapply(settings$state.data.assimilation$state.variable, "[", 'variable.name') %>% unlist %in% varin)
            if (length(unitp)>0) unit <- settings$state.data.assimilation$state.variable[[unitp]]$unit
            #plotting
            p<- ready.to.plot %>%
              dplyr::filter(.data$Variable == vari, .data$Sitev== site) %>%
              ggplot2::ggplot(ggplot2::aes(x = Date)) +
              ggplot2::geom_ribbon(
                ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper, fill = .data$Type),
                color = "black") +
              ggplot2::geom_line(ggplot2::aes(y = .data$Means, color = .data$Type), lwd = 1.02, linetype = 2) +
              ggplot2::geom_point(ggplot2::aes(y = .data$Means, color = .data$Type), size = 3, alpha = 0.75) +
              ggplot2::scale_fill_manual(
                values = c(sda_colors$brown, sda_colors$pink, sda_colors$green, sda_colors$blue),
                name = "") +
              ggplot2::scale_color_manual(
                values = c(sda_colors$brown, sda_colors$pink, sda_colors$green, sda_colors$blue),
                name = "") +
              ggplot2::theme_bw(base_size = 17) +
              ggplot2::labs(y = paste(vari,'(',unit,')'), subtitle = paste0("Site id: ",site)) +
              ggplot2::theme(legend.position = "top", strip.background = ggplot2::element_blank())
            if (!is.null(plot.title)) {
              p <- p + ggplot2::labs(title=plot.title)
            }

            p
          })
      })
  }

  if(Add_Map){
    #------------------------------------------------ map
    site.locs <- settings %>% 
      purrr::map(~.x[['run']] ) %>% 
      purrr::map('site') %>% 
      purrr::map_dfr(~c(.x[['lon']],.x[['lat']]) %>%
                as.numeric)%>% 
      t %>%
      as.data.frame()%>%
      `colnames<-`(c("Lon","Lat")) %>%
      dplyr::mutate(Site=.data$site.ids %>% unique(),
             Name=.data$site.names)
    
    suppressMessages({
      aoi_boundary_HARV <- sf::st_read(system.file("extdata", "eco-regionl2.json", package = "PEcAnAssimSequential"))
    })
    
    #transform site locs into new projection - UTM 2163
    site.locs.sp<-site.locs
    sp::coordinates(site.locs.sp) <- c("Lon", "Lat")
    sp::proj4string(site.locs.sp) <- sp::CRS("+proj=longlat +datum=WGS84")  ## for example
    res <- sp::spTransform(site.locs.sp, sp::CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
    site.locs[,c(1,2)] <-res@coords
    
    #finding site with data
    sites.w.data <-
      obs.mean %>% purrr::map(names) %>% unlist() %>% as.character() %>% unique()
    #adding the column to site
    site.locs <- site.locs %>%
      dplyr::mutate(Data = .data$Site %in% sites.w.data)
    
    #plotting
    map.plot<- ggplot2::ggplot() + 
      ggplot2::geom_sf(
        ggplot2::aes(fill = .data$NA_L1CODE),
        data = aoi_boundary_HARV,
        alpha=0.35,
        lwd=0,
        color="black") +
      ggplot2::geom_point(data = site.locs, size = 2) +
      ggrepel::geom_label_repel(
        data = site.locs,
        ggplot2::aes(
          x = .data$Lon,
          y = .data$Lat,
          label = paste0(.data$Site, "\n", .data$Name),
          color = .data$Data,
        ),
        vjust = 1.2,
        fontface = "bold",
        size = 3.5
      ) + 
      #coord_sf(datum = sf::st_crs(2163),default = F)+
      ggplot2::scale_fill_manual(values = c("#a6cee3",
                                   "#1f78b4","#b2df8a",
                                   "#33a02c","#fb9a99",
                                   "#e31a1c","#fdbf6f",
                                   "#ff7f00","#cab2d6",
                                   "#6a3d9a","#ffff99",
                                   "#b15928","#fccde5",
                                   "#d9d9d9","#66c2a5",
                                   "#ffd92f","#8dd3c7",
                                   "#80b1d3","#d9d9d9",
                                   "#fdbf6f"),name="Eco-Region")+
      ggplot2::scale_color_manual(values= c("#e31a1c","#33a02c"))+
      ggplot2::theme_minimal()+
      ggplot2::theme(axis.text = ggplot2::element_blank())
    
    #----- Reordering the plots
    all.plots.print <-list(map.plot)
    for (i in seq_along(all.plots)) all.plots.print <-c(all.plots.print,all.plots[[i]])
    
    grDevices::pdf(paste0(settings$outdir,"/SDA.pdf"),width = filew, height = fileh)
    all.plots.print %>% purrr::map(~print(.x))
    grDevices::dev.off()
  }else{
    grDevices::pdf(paste0(settings$outdir,"/SDA.pdf"),width = filew, height = fileh)
    all.plots %>% purrr::map(~print(.x))
    grDevices::dev.off()
  }
  
  
  #saving plot data
  save(all.plots, ready.to.plot, file = file.path(settings$outdir, "timeseries.plot.data.Rdata"))
  
  
}



##' @rdname interactive.plotting.sda
##' @param ANALYSIS Analysis object from the sda.output.Rdata.
##' @param FORECAST Forecast object from the sda.output.Rdata.
##' @param obs.mean obs.mean
##' @param obs.cov obs.cov
##' @param outdir physical path where the pdf will be stored.
##' @param pft.path Physical path of pft.csv file to allow by = pft option.
##' @param by arrange figures by var, pft, or site.
##' @param types data types that shown in the figure.
##' @param CI range of confidence interval.
##' @param unit list of unit used for y axis label.
##' @param style color option.
##' @export
##' @author Dongchen Zhang
SDA_timeseries_plot <- function(ANALYSIS, FORECAST, obs.mean = NULL, obs.cov = NULL, outdir, pft.path = NULL, by = "site", types = c("FORECAST", "ANALYSIS", "OBS"), CI = c(0.025, 0.975), 
                                unit = list(AbvGrndWood = "Mg/ha", LAI = "m2/m2", SoilMoistFrac = "", TotSoilCarb = "kg/m2"),
                                style = list(general_color = c("FORECAST" = "blue", "ANALYSIS" = "red", "OBS" = "black"),
                                             fill_color = c("FORECAST" = "yellow", "ANALYSIS" = "green", "OBS" = "grey"),
                                             title_color = "red")){
  #Check package availability.
  if("try-error" %in% class(try(find.package("ggpubr"), silent = T))){
    PEcAn.logger::logger.info("Package ggpubr is not installed! Please install it and rerun the function!")
    return(0)
  }
  #TODO: make page, font, line, point sizes adjustable.
  time_points <- names(FORECAST)
  site_ids <- attributes(FORECAST[[1]])$Site
  var_names <- attributes(FORECAST[[1]])$dimnames[[2]]
  #new diag function: fixed the bug when length==1 then it will return 0x0 matrix
  diag_fix <- function(vector){
    if (length(vector)>1){
      return(diag(vector))
    }else if (length(vector)==1){
      return(vector)
    }
  }
  #read pft.csv file for the option by == pft.
  if(!is.null(pft.path)){
    pft <- utils::read.csv(pft.path)
  }
  #create database
  DB <- data.frame()
  for (id in sort(unique(site_ids))) {
    for (time_point in time_points) {
      for (var_name in sort(unique(var_names))) {
        for (type in types) {
          if(type == "OBS") {
            obs_mean <- obs.mean[[time_point]][[id]][[var_name]]
            if(length(obs_mean) == 0 | is.null(obs_mean)){
              next
            }else{
              obs_cov <- diag_fix(obs.cov[[time_point]][[id]])[which(var_name == names(obs.mean[[time_point]][[id]]))]
              MIN <- obs_mean - 1.96*sqrt(obs_cov)
              MAX <- obs_mean + 1.96*sqrt(obs_cov)
              MEAN <- obs_mean
            }
          } else {
            temp_Dat <- get(type)[[time_point]]
            site_ind <- which(id == site_ids)
            var_ind <- which(var_name == var_names)
            ind <- var_ind[which(var_ind %in% site_ind)]
            MEAN <- mean(temp_Dat[,ind])
            MIN <- stats::quantile(temp_Dat[,ind], CI[1])
            MAX <- stats::quantile(temp_Dat[,ind], CI[2])
          }
          if(MIN < 0) MIN <- 0
          DB <- rbind(DB, list(id = id, date = time_point, var_name = var_name, type = type, upper = MAX, lower = MIN, mean = MEAN))
        }
      }
    }
  }
  #if we plot by each site.
  if(by == "site") {
    PDF_w <- 10
    PDF_h <- 8
    p <- list()
    for (site.id in sort(unique(site_ids))) {
      site_p <- list()
      for (var.name in sort(unique(var_names))) {
        site_p <- rlist::list.append(site_p, dplyr::filter(DB, id == site.id & var_name == var.name) %>% 
                                       dplyr::select(-c(id, var_name)) %>%
                                       dplyr::mutate(date = lubridate::ymd(date)) %>%
                                       ggplot2::ggplot(ggplot2::aes(x=date)) +
                                       ggplot2::geom_ribbon(ggplot2::aes(x = .data$date, ymin = .data$lower, ymax = .data$upper, fill=.data$type), inherit.aes = FALSE, alpha = 0.5) +
                                       ggplot2::geom_line(ggplot2::aes(y=mean, color=type),lwd=0.5,linetype=2) +
                                       ggplot2::geom_point(ggplot2::aes(y=mean, color=type), size=1.5, alpha=0.75) +
                                       ggplot2::scale_fill_manual(values = style$fill_color) +
                                       ggplot2::scale_color_manual(values = style$general_color) +
                                       ggplot2::ylab(paste0(var.name, " (", unit[var.name], ")")))
      }
      p <- rlist::list.append(p, ggpubr::annotate_figure(ggpubr::ggarrange(plotlist = site_p, common.legend = TRUE), 
                                                         top = ggpubr::text_grob(site.id, color = style$title_color, face = "bold", size = 14)))
    }
    #if we plot by each state variable
  } else if (by == "var") {
    PDF_w <- 20
    PDF_h <- 16
    p <- list()
    for (var.name in sort(unique(var_names))) {
      var_p <- list()
      for (site.id in sort(unique(site_ids))) {
        var_p <- rlist::list.append(var_p, dplyr::filter(DB, id == site.id & var_name == var.name) %>% 
                                      dplyr::select(-c(id, var_name)) %>%
                                      dplyr::mutate(date = lubridate::ymd(date)) %>%
                                      ggplot2::ggplot(ggplot2::aes(x=date)) +
                                      ggplot2::geom_ribbon(ggplot2::aes(x = .data$date, ymin = .data$lower, ymax = .data$upper, fill=.data$type), inherit.aes = FALSE, alpha = 0.5) +
                                      ggplot2::geom_line(ggplot2::aes(y=mean, color=type),lwd=0.5,linetype=2) +
                                      ggplot2::geom_point(ggplot2::aes(y=mean, color=type), size=1.5, alpha=0.75) +
                                      ggplot2::scale_fill_manual(values = style$fill_color) +
                                      ggplot2::scale_color_manual(values = style$general_color) +
                                      ggplot2::ylab(paste0(var.name, " (", unit[var.name], ")")) +
                                      ggplot2::ggtitle(site.id))
      }
      p <- rlist::list.append(p, ggpubr::annotate_figure(ggpubr::ggarrange(plotlist = var_p, common.legend = TRUE), 
                                                         top = ggpubr::text_grob(var.name, color = style$title_color, face = "bold", size = 14)))
    }
    #if we plot by each (pft * state variable)
  } else if (by == "pft") {
    if (!exists("pft")) {
      PEcAn.logger::logger.info("Please provide the pdf path!")
      return(0)
    } else {
      PDF_w <- 20
      PDF_h <- 16
      p <- list()
      for (PFT in sort(unique(pft$pft))) {
        site_id_pft <- pft$site[which(pft$pft == PFT)]
        var_p <- list()
        for (var.name in sort(unique(var_names))) {
          site_p <- list()
          for (site.id in sort(site_id_pft)) {
            site_p <- rlist::list.append(site_p, dplyr::filter(DB, id == site.id & var_name == var.name) %>% 
                                           dplyr::select(-c(id, var_name)) %>%
                                           dplyr::mutate(date = lubridate::ymd(date)) %>%
                                           ggplot2::ggplot(ggplot2::aes(x=date)) +
                                           ggplot2::geom_ribbon(ggplot2::aes(x = .data$date, ymin = .data$lower, ymax = .data$upper, fill=.data$type), inherit.aes = FALSE, alpha = 0.5) +
                                           ggplot2::geom_line(ggplot2::aes(y=mean, color=type),lwd=0.5,linetype=2) +
                                           ggplot2::geom_point(ggplot2::aes(y=mean, color=type), size=1.5, alpha=0.75) +
                                           ggplot2::scale_fill_manual(values = style$fill_color) +
                                           ggplot2::scale_color_manual(values = style$general_color) +
                                           ggplot2::ylab(paste0(var.name, " (", unit[var.name], ")")) +
                                           ggplot2::ggtitle(site.id))
          }
          var_p <- rlist::list.append(var_p, ggpubr::annotate_figure(ggpubr::ggarrange(plotlist = site_p, common.legend = TRUE), 
                                                                     top = ggpubr::text_grob(paste(PFT, var.name), color = style$title_color, face = "bold", size = 14)))
        }
        p <- rlist::list.append(p, var_p)
      }
    }
  }
  #print pdf
  grDevices::pdf(file.path(outdir, paste0("SDA_", by, ".pdf")),width = PDF_w, height = PDF_h)
  print(p)
  grDevices::dev.off()
}