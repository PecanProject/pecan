#' @title generate_colors_sda
#' @name  generate_colors_sda
#' @author Ann Raiho 
#' @description This function generates a series of colors in its parents enviroment. This is mainly used in assim.sequential package.
#' @export
generate_colors_sda <-function(){
  pink       <<- col2rgb("deeppink")
  alphapink  <<- rgb(pink[1], pink[2], pink[3], 180, max = 255)
  green      <<- col2rgb("green")
  alphagreen <<- rgb(green[1], green[2], green[3], 75, max = 255)
  blue       <<- col2rgb("blue")
  alphablue  <<- rgb(blue[1], blue[2], blue[3], 75, max = 255)
  purple       <<- col2rgb("purple")
  alphapurple <<- rgb(purple[1], purple[2], purple[3], 75, max = 255)
  brown       <<- col2rgb("brown")
  alphabrown <<- rgb(brown[1], brown[2], brown[3], 30, max = 255)
}


##' Internal functions for plotting SDA outputs. Interactive, post analysis time-series and bias plots in base plotting system and ggplot
##' @param settings  pecan standard settings list.  
##' @param t current time - int number giving the position of the current time in obs.time. 
##' @param obs.time vector of dates of measurements
##' @param obs.mean list of vectors of the means of observed data named by the measured date.
##' @param obs.cov list of cov matrices of the observed data named by the measured date.
##' @param obs list containing the mean and cov object
##' @param X  dataframe of state variables for each ensemble
##' @param FORECAST dataframe of state variables for each ensemble
##' @param ANALYSIS  vector of mean of state variable after analysis
##' @param plot.title character giving the title for post visualization ggplots
##' @export

interactive.plotting.sda<-function(settings, t, obs.times, obs.mean, obs.cov, obs, X, FORECAST, ANALYSIS){

  #Defining some colors
  generate_colors_sda()
  t1         <- 1
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
    Y.order <- na.omit(pmatch(colnames(X), colnames(Ybar)))
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
  
  par(mfrow = c(2, 1))
  colmax<-2
  for (i in 1:ncol(FORECAST[[t]])) { #
    
    Xbar <- plyr::laply(FORECAST[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    Xci  <- plyr::laply(FORECAST[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975), na.rm = TRUE) })
    
    Xa <- plyr::laply(ANALYSIS[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    XaCI <- plyr::laply(ANALYSIS[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975), na.rm = TRUE) })
    
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
      ciEnvelope(as.Date(obs.times[t1:t]),
                 as.numeric(Ybar[, i]) - as.numeric(YCI[, i]) * 1.96, 
                 as.numeric(Ybar[, i]) + as.numeric(YCI[, i]) * 1.96, 
                 col = alphagreen)
      lines(as.Date(obs.times[t1:t]), 
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
    ciEnvelope(as.Date(obs.times[t1:t]), Xci[, 1], Xci[, 2], col = alphablue)  #col='lightblue')
    lines(as.Date(obs.times[t1:t]), Xbar, col = "darkblue", type = "l", lwd = 2)
    
    # analysis
    ciEnvelope(as.Date(obs.times[t1:t]), XaCI[, 1], XaCI[, 2], col = alphapink)
    lines(as.Date(obs.times[t1:t]), Xa, col = "black", lty = 2, lwd = 2)
    #legend('topright',c('Forecast','Data','Analysis'),col=c(alphablue,alphagreen,alphapink),lty=1,lwd=5)
  }
}

##' @rdname interactive.plotting.sda
##' @export

postana.timeser.plotting.sda<-function(settings, t, obs.times, obs.mean, obs.cov, obs, X, FORECAST, ANALYSIS){

  #Defining some colors
  generate_colors_sda()
  t1         <- 1
  ylab.names <- unlist(sapply(settings$state.data.assimilation$state.variable, 
                              function(x) { x })[2, ], use.names = FALSE)
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  #----
  pdf(file.path(settings$outdir,"SDA", "sda.enkf.time-series.pdf"))
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
    if (is.null(x)) {
      rep(NA, length(names.y))
    }
    sqrt(diag(x))
  })))
  
  Ybar[is.na(Ybar)]<-0
  YCI[is.na(YCI)]<-0
  
  YCI <- YCI[,Y.order]
  
  
  
  Xsum <- plyr::laply(FORECAST, function(x) { mean(rowSums(x[,1:length(names.y)], na.rm = TRUE)) })[t1:t]
  Xasum <- plyr::laply(ANALYSIS, function(x) { mean(rowSums(x[,1:length(names.y)], na.rm = TRUE)) })[t1:t]

  #------For each state variable 
  for (i in seq_len(ncol(X))) {
    Xbar <- plyr::laply(FORECAST[t1:t], function(x) {
      mean(x[, i], na.rm = TRUE) }) #/rowSums(x[,1:9],na.rm = T)
    Xci <- plyr::laply(FORECAST[t1:t], function(x) { 
      quantile(x[, i], c(0.025, 0.975),na.rm = T) })
    
    Xci[is.na(Xci)]<-0
    
    Xbar <- Xbar
    Xci <- Xci
    
    Xa <- plyr::laply(ANALYSIS[t1:t], function(x) { 
      mean(x[, i],na.rm = T) })
    
    XaCI <- plyr::laply(ANALYSIS[t1:t], function(x) { 
      quantile(x[, i], c(0.025, 0.975),na.rm = T )})
    
    Xa <- Xa
    XaCI <- XaCI
    
    plot(as.Date(obs.times[t1:t]),
         Xbar, 
         ylim = range(c(XaCI, Xci,Ybar[, 1]), na.rm = TRUE),
         type = "n", 
         xlab = "Year", 
         ylab = ylab.names[grep(colnames(X)[i], var.names)],
         main = colnames(X)[i])
    
    # observation / data
    if (i<=ncol(X)) { #
      ciEnvelope(as.Date(obs.times[t1:t]), 
                 as.numeric(Ybar[, i]) - as.numeric(YCI[, i]) * 1.96, 
                 as.numeric(Ybar[, i]) + as.numeric(YCI[, i]) * 1.96, 
                 col = alphagreen)
      lines(as.Date(obs.times[t1:t]), 
            as.numeric(Ybar[, i]), 
            type = "l", col = "darkgreen", lwd = 2)
    }
    
    # forecast
    ciEnvelope(as.Date(obs.times[t1:t]), Xci[, 1], Xci[, 2], col = alphablue)  #col='lightblue') #alphablue
    lines(as.Date(obs.times[t1:t]), Xbar, col = "darkblue", type = "l", lwd = 2) #"darkblue"
    
    # analysis
    ciEnvelope(as.Date(obs.times[t1:t]), XaCI[, 1], XaCI[, 2], col = alphapink) #alphapink
    lines(as.Date(obs.times[t1:t]), Xa, col = "black", lty = 2, lwd = 2) #"black"
    
    legend('topright',c('Forecast','Data','Analysis'),col=c(alphablue,alphagreen,alphapink),lty=1,lwd=5)
    
  }
  
  dev.off()
  
}

##' @rdname interactive.plotting.sda
##' @export

postana.bias.plotting.sda<-function(settings, t, obs.times, obs.mean, obs.cov, obs, X, FORECAST, ANALYSIS){

  #Defining some colors
  generate_colors_sda()
  t1         <- 1
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
  pdf(file.path(settings$outdir,"SDA", "bias.diagnostic.pdf"))
  for (i in seq_along(obs.mean[[1]])) {
    Xbar <- plyr::laply(FORECAST[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    Xci <- plyr::laply(FORECAST[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975)) })
    
    Xa <- plyr::laply(ANALYSIS[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    XaCI <- plyr::laply(ANALYSIS[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975)) })
    
    if(length(which(is.na(Ybar[,i])))>=length(t1:t)) next()
    reg <- lm(Xbar[t1:t] - unlist(Ybar[, i]) ~ c(t1:t))
    plot(t1:t, 
         Xbar - unlist(Ybar[, i]),
         pch = 16, cex = 1, 
         ylim = c(min(Xci[, 1] - unlist(Ybar[, i])), max(Xci[,2] - unlist(Ybar[, i]))), 
         xlab = "Time", 
         ylab = "Error", 
         main = paste(colnames(X)[i], " Error = Forecast - Data"))
    ciEnvelope(rev(t1:t), 
               rev(Xci[, 1] - unlist(Ybar[, i])), 
               rev(Xci[, 2] - unlist(Ybar[, i])),
               col = alphabrown)
    abline(h = 0, lty = 2, lwd = 2)
    abline(reg)
    mtext(paste("slope =", signif(summary(reg)$coefficients[2], digits = 3), 
                "intercept =", signif(summary(reg)$coefficients[1], digits = 3)))
    # d<-density(c(Xbar[t1:t] - unlist(Ybar[t1:t,i]))) lines(d$y+1,d$x)
    
    # forecast minus analysis = update
    reg1 <- lm(Xbar - Xa ~ c(t1:t))
    plot(t1:t, 
         Xbar - Xa, 
         pch = 16, cex = 1, 
         ylim = c(min(Xbar - XaCI[, 2]), max(Xbar - XaCI[, 1])), 
         xlab = "Time", ylab = "Update", 
         main = paste(colnames(X)[i], 
                      "Update = Forecast - Analysis"))
    ciEnvelope(rev(t1:t), 
               rev(Xbar - XaCI[, 1]), 
               rev(Xbar - XaCI[, 2]), 
               col = alphapurple)
    abline(h = 0, lty = 2, lwd = 2)
    abline(reg1)
    mtext(paste("slope =", signif(summary(reg1)$coefficients[2], digits = 3),
                "intercept =", signif(summary(reg1)$coefficients[1], 
                                      digits = 3)))
    # d<-density(c(Xbar[t1:t] - Xa[t1:t])) lines(d$y+1,d$x)
  }
  dev.off()
  
}

##' @rdname interactive.plotting.sda
##' @export
postana.bias.plotting.sda.corr<-function(t, obs.times, X, aqq, bqq){
  
  t1<- 1
  #Defining some colors
  generate_colors_sda()

  #---
  library(corrplot)
  pdf('SDA/process.var.plots.pdf')
  
  cor.mat <- cov2cor(aqq[t,,] / bqq[t])
  colnames(cor.mat) <- colnames(X)
  rownames(cor.mat) <- colnames(X)
  par(mfrow = c(1, 1), mai = c(1, 1, 4, 1))
  corrplot(cor.mat, type = "upper", tl.srt = 45,order='FPC')
  
  par(mfrow=c(1,1))   
  plot(as.Date(obs.times[t1:t]), bqq[t1:t],
       pch = 16, cex = 1,
       ylab = "Degrees of Freedom", xlab = "Time")
  
  dev.off()
}

##' @rdname interactive.plotting.sda
##' @export

post.analysis.ggplot <- function(settings, t, obs.times, obs.mean, obs.cov, obs, X, FORECAST, ANALYSIS, plot.title=NULL){

  t1         <- 1
  #Defining some colors
  generate_colors_sda()
  ylab.names <- unlist(sapply(settings$state.data.assimilation$state.variable, 
                              function(x) { x })[2, ], use.names = FALSE)
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  #----
  #Analysis & Forcast cleaning and STAT
  All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
  
  ready.FA <- c('FORECAST','ANALYSIS')%>%
    purrr::map_df(function(listFA){
      All.my.data[[listFA]]%>%
        purrr::map_df(function(state.vars){
          #finding the mean and Ci for all the state variables
          means <- apply(state.vars,2,mean,na.rm=T)
          CI <- apply(state.vars,2,quantile,c(0.025, 0.975),na.rm = T)
          #putting them into a nice clean df
          rbind(means,CI) %>% t %>%
            as.data.frame()%>%
            mutate(Variables=paste(colnames(state.vars)))%>%
            tidyr::replace_na(list(0))
          
          
        })%>%mutate(Type=listFA,
                    Date=rep(obs.times[t1:t], each=colnames((All.my.data[[listFA]])[[1]]) %>% length())
        )
      
    })
  
  
  #Observed data
  #first merging mean and conv based on the day
  ready.to.plot <- names(obs.mean)%>%
    purrr::map(~c(obs.mean[.x],obs.cov[.x],.x)%>%
                 setNames(c('means','covs','Date')))%>%
    setNames(names(obs.mean))%>%
    purrr::map_df(function(one.day.data){
      #CI
      
      purrr::map2_df(sqrt(diag(one.day.data$covs)), one.day.data$means,
                     function(sd, mean){
                       data.frame(mean-(sd*1.96), mean+(sd*1.96))
                       
                     })%>%
        mutate(Variables=names(one.day.data$means))%>%
        `colnames<-`(c('2.5%','97.5%','Variables'))%>%
        mutate(means=one.day.data$means%>%unlist,
               Type="Data",
               Date=one.day.data$Date%>%as.POSIXct())
      
      
    })%>%
    #filter(Variables %in% var.names)%>%
    bind_rows(ready.FA)
  
  ready.to.plot$Variables%>%unique()%>%
    purrr::map(function(vari){
      
      varin<-vari
      unit<-""
      if (substr(vari,1,8)=="AGB.pft.") varin <- "AGB.pft"
      #finding the unit
      unitp <- which(lapply(settings$state.data.assimilation$state.variable, "[", 'variable.name') %>% unlist %in% varin)
      if (length(unitp)>0) unit <- settings$state.data.assimilation$state.variable[[unitp]]$unit
      #plotting
      ready.to.plot%>%
        filter(Variables==vari)%>%
        ggplot(aes(x=Date))+
        geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`,fill=Type),color="black")+
        geom_line(aes(y=means, color=Type),lwd=1.02,linetype=2)+
        geom_point(aes(y=means, color=Type),size=3,alpha=0.75)+
        scale_fill_manual(values = c(alphapink,alphagreen,alphablue),name="")+
        scale_color_manual(values = c(alphapink,alphagreen,alphablue),name="")+
        theme_bw(base_size = 17)+
        labs(y=paste(vari,'(',unit,')'))+
        theme(legend.position = "top",
              strip.background = element_blank())->p
      if (!is.null(plot.title)) p <- p + labs(title=plot.title)
      p
    })->all.plots
  
  
  pdf("SDA/SDA.pdf",width = 10,height = 8)
  all.plots %>% purrr::map(~print(.x))
  dev.off()
  
  #saving plot data
  save(all.plots, ready.to.plot, file = file.path(settings$outdir,"SDA", "timeseries.plot.data.Rdata"))
  
  
}

##' @rdname interactive.plotting.sda
##' @export

post.analysis.ggplot.violin <- function(settings, t, obs.times, obs.mean, obs.cov, obs, X, FORECAST, ANALYSIS, plot.title=NULL){

  
  #Defining some colors
  t1         <- 1
  generate_colors_sda()
  ylab.names <- unlist(sapply(settings$state.data.assimilation$state.variable, 
                              function(x) { x })[2, ], use.names = FALSE)
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")

#rearranging the forcast and analysis data  

  All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
  
  ready.FA <- c('FORECAST','ANALYSIS')%>%
    purrr::map_df(function(listFA){
      All.my.data[[listFA]]%>%
        purrr::map_df(function(state.vars){
          state.vars%>%as.data.frame()
        })%>%mutate(Type=listFA,
                    Date=rep(obs.times[t1:t], each=((All.my.data[[listFA]])[[1]]) %>% nrow())
        )
      
    })%>%
    tidyr::gather(Variables, Value, -c(Type,Date)) 
  #Observed data
  #first merging mean and conv based on the day
  obs.df <- names(obs.mean)%>%
    purrr::map(~c(obs.mean[.x], obs.cov[.x], .x)%>%
                 setNames(c('means','covs','Date')))%>%
    setNames(names(obs.mean))%>%
    purrr::map_df(function(one.day.data){
      #CI
      purrr::map2_df(sqrt(diag(one.day.data$covs)), one.day.data$means,
                     function(sd,mean){
                       data.frame(mean-(sd*1.96), mean+(sd*1.96))
                       
                     })%>%
        mutate(Variables=names(one.day.data$means))%>%
        `colnames<-`(c('2.5%','97.5%','Variables'))%>%
        mutate(means=one.day.data$means%>%unlist,
               Type="Data",
               Date=one.day.data$Date%>%as.POSIXct())
      
      
    })#%>%
  #filter(Variables %in% var.names)
  
  
  
  ready.FA$Variables%>%unique()%>%
    purrr::map(function(vari){
      varin<-vari
      unit<-""
      if (substr(vari,1,8)=="AGB.pft.") varin <- "AGB.pft"
      #finding the unit
      unitp <- which(lapply(settings$state.data.assimilation$state.variable, "[", 'variable.name') %>% unlist %in% varin)
      if (length(unitp)>0) unit <- settings$state.data.assimilation$state.variable[[unitp]]$unit
      #plotting
      ready.FA%>%
        filter(Variables==vari)%>%
        ggplot(aes(Date,Value))+
        geom_ribbon(aes(x=Date,y=means,ymin=`2.5%`,ymax=`97.5%`,fill=Type), data=obs.df %>% filter(Variables==vari), color="black")+
        geom_line(aes(y=means, color=Type),data=obs.df%>% filter(Variables==vari),lwd=1.02,linetype=2)+
        geom_violin(aes(x=Date,fill=Type,group=interaction(Date,Type)), position = position_dodge(width=0.9))+
        geom_jitter(aes(color=Type), position=position_jitterdodge(dodge.width=0.9))+
        scale_fill_manual(values = c(alphapink,alphagreen,alphablue))+
        scale_color_manual(values = c(alphapink,alphagreen,alphablue))+
        theme_bw(base_size = 17)+
        labs(y=paste(vari,'(',unit,')'))+
        theme(legend.position = "top",
              strip.background = element_blank())->p
      if (!is.null(plot.title)) p <- p + labs(title=plot.title)
      p
    })->all.plots
  
  pdf("SDA/SDA.Violin.pdf", width = 10, height = 8, onefile = TRUE)
  all.plots %>% purrr::map(~print(.x))
  dev.off()
  
  #saving plot data
  save(all.plots, ready.FA, obs.df, file = file.path(settings$outdir,"SDA", "timeseries.violin.plot.data.Rdata"))
  
}

##' @rdname interactive.plotting.sda
##' @export
post.analysis.multisite.ggplot <- function(settings, t, obs.times, obs.mean, obs.cov, FORECAST, ANALYSIS, plot.title=NULL, facetg=F, readsFF=NULL){

  if (!('ggrepel' %in% installed.packages()[,1])) devtools::install_github("slowkow/ggrepel")

  #Defining some colors
  t1         <- 1
  generate_colors_sda()
  varnames <- settings$state.data.assimilation$state.variable
  #just a check
  if (is.null(varnames)) varnames <- settings[[1]]$state.data.assimilation$state.variable
  
  ylab.names <- unlist(sapply(varnames, 
                              function(x) { x })[2, ], use.names = FALSE)
  
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  site.ids <- attr(FORECAST[[1]], 'Site')
  site.names <- settings %>% map(~.x[['run']] ) %>% map('site') %>% map('name') %>% unlist() %>% as.character()
  
  #------------------------------------------------Data prepration
  #Analysis & Forcast cleaning and STAT
  All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
  
  ready.FA <- c('FORECAST','ANALYSIS')%>%
    purrr::map_df(function(listFA){
      All.my.data[[listFA]]%>%
        purrr::map_df(function(state.vars){
         
          #finding the mean and Ci for all the state variables
          site.ids %>% unique() %>%
            map_df(function(site){
              (state.vars)[,which(site.ids  %in% site)] %>% 
                as.data.frame %>% 
                mutate(Site=site)
            }) %>%
            tidyr::gather(Variable, Value, -c(Site)) %>%
            group_by(Site,Variable) %>%
            summarise(
              Means=mean(Value, na.rm=T),
              Lower=quantile(Value,0.025, na.rm=T),
              Upper=quantile(Value,0.975, na.rm=T))
          
        })%>%mutate(Type=paste0("SDA_",listFA),
                    Date=rep(obs.times[t1:t], each=colnames((All.my.data[[listFA]])[[1]]) %>% length() / length(unique(site.ids)))%>% as.POSIXct()
        )
    
    })
      

  obs.var.names <- (obs.mean[[1]])[[1]] %>% colnames()
  #Observed data
  #first merging mean and conv based on the day
  ready.to.plot <- names(obs.mean)%>%
    purrr::map(~c(obs.mean[.x],obs.cov[.x],.x)%>%
                 setNames(c('means','covs','Date')))%>%
    setNames(names(obs.mean))%>%
    purrr::map_df(function(one.day.data){
      one.day.data$means %>% 
        map_dfr(~.x) %>% 
        mutate(Site=names(one.day.data$means)) %>% 
        tidyr::gather(Variable,Means,-c(Site)) %>%
        right_join(one.day.data$covs %>% 
                     map_dfr(~ t(sqrt(diag(.x))) %>% 
                               data.frame %>% `colnames<-`(c(obs.var.names))) %>%
                     mutate(Site=names(one.day.data$covs)) %>% 
                     tidyr::gather(Variable,Sd,-c(Site)),
                   by=c('Site','Variable')) %>%
        mutate(Upper=Means+(Sd*1.96),
               Lower=Means-(Sd*1.96))%>%
        mutate(Type="SDA_Data",
               Date=one.day.data$Date %>% as.POSIXct())
      
      
    })%>% 
    dplyr::select(-Sd) %>%
    bind_rows(ready.FA)
  
  #--- Adding the forward forecast
  if (!is.null(readsFF)){
    
    readsFF.df<-readsFF %>%
      map_df(function(siteX){
    
        siteX %>% map_df(function(DateX){
          DateX %>% 
            map_df(~.x %>% t ) %>%
            tidyr::gather(Variable, Value,-c(Date, Site)) %>%
            group_by(Variable,Date, Site) %>%
             summarise(
               Means=mean(Value, na.rm=T),
               Lower=quantile(Value,0.025, na.rm=T),
               Upper=quantile(Value,0.975, na.rm=T)) %>% 
             mutate(Type="ForwardForecast")
        })
      })
    
    ready.to.plot <- ready.to.plot %>%
      bind_rows(readsFF.df)
    
  }
  
  ready.to.plot$Variable[ready.to.plot$Variable=="LeafC"] <-"leaf_carbon_content"
  

  #Adding the units to the variables
  ready.to.plot$Variable %>% unique() %>% 
    walk(function(varin){
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
    all.plots<-ready.to.plot$Site%>%unique() %>%
      purrr::map(function(site){
            #plotting
            ready.to.plot%>%
              filter(Site==site)%>%
              ggplot(aes(x=Date))+
              geom_ribbon(aes(ymin=Lower,ymax=Upper,fill=Type),color="black")+
              geom_line(aes(y=Means, color=Type),lwd=1.02,linetype=2)+
              geom_point(aes(y=Means, color=Type),size=3,alpha=0.75)+
              scale_fill_manual(values = c(alphabrown,alphapink,alphagreen,alphablue),name="")+
              scale_color_manual(values = c(alphabrown,alphapink,alphagreen,alphablue),name="")+
              theme_bw(base_size = 17)+
              labs(y="", subtitle=paste0("Site id: ",site))+
              theme(legend.position = "top",
                    strip.background = element_blank())->p
            if (!is.null(plot.title)) p <- p + labs(title=plot.title)
            p <- p + facet_wrap(~Variable, ncol=2, scales = "free_y")
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
            ready.to.plot%>%
              filter(Variable==vari, Site==site)%>%
              ggplot(aes(x=Date))+
              geom_ribbon(aes(ymin=Lower,ymax=Upper,fill=Type),color="black")+
              geom_line(aes(y=Means, color=Type),lwd=1.02,linetype=2)+
              geom_point(aes(y=Means, color=Type),size=3,alpha=0.75)+
              scale_fill_manual(values = c(alphabrown,alphapink,alphagreen,alphablue),name="")+
              scale_color_manual(values = c(alphabrown,alphapink,alphagreen,alphablue),name="")+
              theme_bw(base_size = 17)+
              labs(y=paste(vari,'(',unit,')'), subtitle=paste0("Site id: ",site))+
              theme(legend.position = "top",
                    strip.background = element_blank())->p
            if (!is.null(plot.title)) p <- p + labs(title=plot.title)
            p
          })
      })
  }

  
  #------------------------------------------------ map
  site.locs <- settings %>% map(~.x[['run']] ) %>% map('site') %>% map_dfr(~c(.x[['lon']],.x[['lat']]) %>%as.numeric)%>% 
    t %>%
    as.data.frame()%>%
    `colnames<-`(c("Lon","Lat")) %>%
    mutate(Site=site.ids %>% unique(),
           Name=site.names)
  

  suppressMessages({
      aoi_boundary_HARV <- sf::st_read(system.file("extdata", "eco-regionl2.json", package = "PEcAn.assim.sequential"))
  })
  
  #transform site locs into new projection - UTM 2163
  site.locs.sp<-site.locs
  coordinates(site.locs.sp) <- c("Lon", "Lat")
  proj4string(site.locs.sp) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(site.locs.sp, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  site.locs[,c(1,2)] <-res@coords
  
  
  #finding site with data
  sites.w.data <-
    obs.mean %>% purrr::map(names) %>% unlist() %>% as.character() %>% unique()
  #adding the column to site
  site.locs <- site.locs %>%
    mutate(Data = Site %in% sites.w.data)

  #plotting
  map.plot<- ggplot() + 
    geom_sf(aes(fill=NA_L1CODE),data = aoi_boundary_HARV, alpha=0.35,lwd=0,color="black")+
    geom_point(data = site.locs,
               aes(x = Lon, y = Lat),
               size = 2) +
    ggrepel::geom_label_repel(
      data = site.locs,
      aes(
        x = Lon,
        y = Lat,
        label = paste0(Site, "\n", Name),
        color = Data,
      ),
      vjust = 1.2,
      fontface = "bold",
      size = 3.5
    ) + 
    #coord_sf(datum = sf::st_crs(2163),default = F)+
    scale_fill_manual(values = c("#a6cee3",
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
    scale_color_manual(values= c("#e31a1c","#33a02c"))+
    theme_minimal()+
    theme(axis.text = element_blank())

  #----- Reordering the plots
  all.plots.print <-list(map.plot)
  for (i in seq_along(all.plots)) all.plots.print <-c(all.plots.print,all.plots[[i]])
  
  pdf("SDA/SDA.pdf",width = filew, height = fileh)
  all.plots.print %>% purrr::map(~print(.x))
  dev.off()
  
  #saving plot data
  save(all.plots, ready.to.plot, file = file.path(settings$outdir,"SDA", "timeseries.plot.data.Rdata"))
  
  
}
