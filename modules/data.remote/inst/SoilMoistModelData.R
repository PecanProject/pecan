library(tidyverse)
library(plotly)
library(patchwork)
library(babynames)
library(viridis)
library(purrr)
library(lubridate)
library(tidyr)
library(dplyr)
library(ncdf4)
library(stringr)
library(hydroGOF)
library(Metrics)
library(verification)

sipnet.out <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/SDA/out/ENS-00001-1000004927/sipnet.2012-07-16.out"
sipnet.out1 <- "/projectnb/dietzelab/hamzed/SDA/ProductionRun/50Sites/SDA_50Sites_1000008768/out/ENS-00001-769/sipnet.out"
text <- readLines(sipnet.out)[-1]
csv <- read.csv(textConnection(text), sep = "")


plot(csv)

site_pft <- read.csv("/projectnb/dietzelab/hamzed/SDA/ProductionRun/50Sites/SDA_50Sites_1000008768/out/ENS-00001-769/sipnet.out")

plot(csv$day,csv$LAI)
plot(csv$day,csv$gpp)

clim.path <- "/projectnb/dietzelab/dongchen/Multi-site/download_500_sites/SDA/run/ENS-00001-646/sipnet.clim"
text.clim <- readLines(clim.path)
csv.clim <- read.csv(textConnection(text.clim), sep="", header = F)
plot(csv.clim$V3, csv.clim$V10)

sipnet.out1 <- "/projectnb/dietzelab/hamzed/SDA/ProductionRun/200Sites/SDA_200Sites_1000008769/out/ENS-00001-622/sipnet.2004-07-16.out"

text1 <- readLines(sipnet.out1)[-1]
csv1 <- read.csv(textConnection(text1), sep = "")
#csv1_1986 <- csv1[csv1$year==1986,]
plot(csv1$day, csv1$nee)

#csv_2001 <- csv[csv$year==2001,]
plot(csv$day, csv$LAI)


param.path <- "/projectnb/dietzelab/dongchen/Multi-site/download_500_sites/SDA/run/ENS-00001-646/sipnet_2001_2002.param"
text.param <- readLines(param.path)
csv.param <- read.csv(textConnection(text.param), sep="", header = F)







#############################################################################################################################

sipnet.out <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA_testrun/out/ENS-00025-1000004921/sipnet.2015-07-16.out"
text <- readLines(sipnet.out)[-1]
csv <- read.csv(textConnection(text), sep = "")
plot(csv$day, csv$soilWater)




#check on nc file
## Harvard Forest also = 1000004921
#################### Extracts Soil Moisture by Year ####################
setwd('/projectnb2/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA_testrun/out')

extractSoilMoist <- function(betySiteId, yearStart, yearEnd, ENS){

  for (x in 0:(yearEnd - yearStart)) {
    nc.file <- paste0("/projectnb2/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA_testrun/out/ENS-",
                      ENS, "-", betySiteId, "/", yearStart + x,".nc")
    nc <- nc_open(nc.file)
    if (x == 0) {
      soilMoist <- ncvar_get(nc,'SoilMoist')
      time <- PEcAn.utils::cf2datetime(nc[["dim"]][["time"]][["vals"]],nc$dim$time$units)
    } else {
      soilMoist <- append(soilMoist, ncvar_get(nc,'SoilMoist'))
      time <- append(time, PEcAn.utils::cf2datetime(nc[["dim"]][["time"]][["vals"]],nc$dim$time$units))
    }
  }
  return(list(time, soilMoist))
}

#################### Collects Multiple Ensembles and Plots ####################
extractEnsambles <- function(betySiteId, start, end, ENSnum=25){
  for (x in 1:ENSnum) {
    newEns <- extractSoilMoist(betySiteId = betySiteId, yearStart = as.numeric(format(start, '%Y')), 
                               yearEnd = as.numeric(format(end, '%Y')), ENS = str_pad(x, 5, pad = '0'))
    
    if (x == 1){
      moisture_time <- newEns
    } else {
      moisture_time[[1]] <- append(moisture_time[[1]], newEns[[1]])
      moisture_time[[2]] <- append(moisture_time[[2]], newEns[[2]]) 
    }
  }
  
  # Eliminate extraneous date range values #
  vtr <- as.data.frame(moisture_time, col.names = c('time', 'moisture'))
  vtr <- vtr[vtr$time >= as.Date(start) & vtr$time <= as.Date(end), ]
  
  return(vtr)
}

runAnalysis <- function(betySiteId, start, end, compare){
  
  moistureFrame <- extractEnsambles(betySiteId, as.Date(start), as.Date(end))
  
  ## Grouping data by Date ##
  grouped_ENS <- moistureFrame %>% 
    group_by(time) %>% 
    mutate(max = max(moisture)) %>%
    mutate(min = min(moisture))
  
  ## Calculating Mean Per Ensemble ##
  mean_per_ENS <- summarize(grouped_ENS, mean = mean(moisture))
  
  ## Calculating Standard Deviation ##
  sd_per_ENS <- summarize(grouped_ENS, sd = sd(moisture))
  
  ## Calculating .025 and .975 Quantiles ##
  quantiles_lower <- summarize(grouped_ENS, q_lower = quantile(moisture, .025))
  quantiles_upper <- summarize(grouped_ENS, q_upper = quantile(moisture, .975))
  
  ## Hex Data Distribution with Mean-line Highlighted ##
  p1 <- ggplot(moistureFrame, aes(x = time, y = moisture)) + geom_hex() + theme_bw() + 
    guides(fill=guide_legend(title = "Frequency")) + 
    ggtitle('Hex Distribution of Model Data') +
    geom_line(data = mean_per_ENS, aes(x = time, y = mean, color = 'Mean of Model Data')) + 
    labs(colour = '')
  
  
  ## Min/Max Values with 95% Quantile Range ## Still has messed up color legend
  p2 <- ggplot(grouped_ENS, aes(x = time, y = moisture)) + theme_bw() + 
    ggtitle('Min/Max Values with 95% Quantile Range in Black') +
    geom_point(data = grouped_ENS, aes(x = time, y = max, color = 'red')) +
    geom_point(data = grouped_ENS, aes(x = time, y = min), color = 'green') +
    geom_line(data = mean_per_ENS, aes(x = time, y = mean, color = 'Mean')) +
    geom_line(data = quantiles_lower, aes(x = time, y = q_lower)) +
    geom_line(data = quantiles_upper, aes(x = time, y = q_upper))
  
  if (compare) {
    ## Analyses Requiring Actual SMAP Data ##
    ## Download SMAP data ##
    source('~/pecan/modules/data.remote/inst/download_SMAP.R')
    smap_data <- all_smap_data[[betySiteId]]
    
      
    ## Quick Plot of Actual SMAP Data ##
    plot(smap_data$Date, smap_data$ssm, main='SMAP ssm (Surface Soil Moisture) vs Time')
    p3 <- recordPlot()
    
    ## ggplot Version with Linear Regression Line ##
    p4 <- ggplot(smap_data, aes(x = Date, y = ssm)) + geom_point() +
      geom_smooth(data = smap_data, aes(x = Date, y = ssm), method='lm', formula= y~x) +
      ggtitle('ggplot Version with Linear Regression Line') +
      theme(plot.title = element_text(hjust = 0.5))
    
    ## Hex distribution of Model Data with Actual Values as well as Mean and Quantile Range (Way too much) ##
    p5 <- ggplot(moistureFrame, aes(x = time, y = moisture)) + geom_hex() + theme_bw() + 
      geom_line(data = mean_per_ENS, aes(x = time, y = mean, color = 'Mean of Model Data')) +
      geom_line(data = quantiles_lower, aes(x = time, y = q_lower, color = '95% Quantile Range')) + 
      geom_line(data = quantiles_upper, aes(x = time, y = q_upper, color = '95% Quantile Range')) +
      geom_line(data = smap_data, aes(x = as_datetime(Date), y = ssm, color = 'Actual SMAP Values')) +
      ggtitle('Way too Much...')
    
    ## Actual Data with Mean and 95% quantile range of Model Data ##
    p6 <- ggplot() + theme_bw() + 
      geom_line(data = mean_per_ENS, aes(x = time, y = mean, color = 'Mean of Model Data')) +
      geom_line(data = quantiles_lower, aes(x = time, y = q_lower, color = '95% Quantile Range')) + 
      geom_line(data = quantiles_upper, aes(x = time, y = q_upper, color = '95% Quantile Range')) +
      geom_line(data = smap_data, aes(x = as_datetime(Date), y = ssm, color = 'Actual SMAP Values')) + 
      ggtitle('Actual SMAP vs Mean and 95% Quantile Range of Model Data')
    
    ## Collapsing Temporal Scale to Match SMAP Output Data ##
    vec_time <- vector()
    vec_mean <- vector()
    vec_sd <- vector()
    running_mean <- 0
    running_sd <- 0
    
    for (row in 1:nrow(mean_per_ENS)) {
      cur_day <- as_date(mean_per_ENS$time[row]) - 1 ## -1 to match days. This really isn't that necessary
      running_mean <- running_mean + as.numeric(mean_per_ENS[row, 'mean'])
      running_sd <- running_sd + as.numeric(sd_per_ENS[row, 'sd'])
      
      if (row %% 24 == 0){ ## 1:24 ratio between actual and model sample duration
        vec_time <- append(vec_time, cur_day)
        vec_mean <- append(vec_mean, (running_mean / 24))
        vec_sd <- append(vec_sd, running_sd / 24)
        running_mean <- 0
        running_sd <- 0 
        cur_day <- (cur_day + 3)
      }
    }
    df <- data.frame(vec_time, vec_mean, vec_sd)
    
    ## Quick Plot of Actual SMAP Data vs Mean Model Data ##
    mod <- lm(smap_data$ssm ~ df$vec_mean)
    plot(df$vec_mean, smap_data$ssm, main= 'Actual SMAP vs Mean Model Data\nw/ Linear Regression Line') + 
      abline(mod)
    p7 <- recordPlot()
    
    ## Calculating Bias ##
    pbias <- pbias(df['vec_mean'], smap_data['ssm'], rm.NA=TRUE)
    
    ## Calculating RMSE ##
    RMSE <- rmse(smap_data$ssm, df$vec_mean)
    
    ## Calculating CRPS ##
    CRPS <- crps(smap_data$ssm, as.matrix(cbind(df$vec_mean, df$vec_sd)))$CRPS
    
    ## Calculating Correlation ##
    correlation <- cor(df$vec_mean, smap_data$ssm)
    
    ## Stat Summary of Linear Regression Model Including R^2 Value ##
    r.squared <- summary(mod)$r.squared
    
    ## Outputs ##
    cat('Percent Bias:', pbias, '%\nRMSE:', RMSE,'\nCRPS:', CRPS, '\nCorrelation:', correlation, '\nR-Squared:', r.squared)
    
    ## Quick Plot of Residuals vs. Time ##
    plot(df$vec_time, mod$residuals, main='Residuals vs. Time') +
      abline(h = 0, lty = 2)
    p8 <- recordPlot()
    
    vtr <- list(p1, p2, p3, p4, p5, p6, p7, p8, pbias, RMSE, CRPS, correlation, r.squared)
    
    names(vtr) <-  c('Plot1', 'Plot2', 'Plot3', 'Plot4', 'Plot5', 'Plot6', 'Plot7',
                     'Plot8', 'pbias','RMSE', 'CRPS', 'correlation', 'r.squared')
    
  } else {
    vtr <- list(p1, p2)
    
    names(vtr) <- c('Plot1', 'Plot2')
  }
  return(vtr)
}

all_smap_data <- download_SMAP('2016-01-02', '2016-07-16')

## Test Zone ##
run1 <- runAnalysis('1000004945', '2016-01-02', '2016-07-16', TRUE)
run1$Plot5
run2 <- runAnalysis('1000004931', '2016-01-02', '2016-07-16', TRUE)
run2$Plot5

run3 <- runAnalysis('1000004890', '2016-01-02', '2016-07-16', TRUE)
run3$Plot5
run4 <- runAnalysis('1000004924', '2016-01-02', '2016-07-16', TRUE)
run4$Plot5

run5 <- runAnalysis('1000004918', '2016-01-02', '2016-07-16', TRUE)
run5$Plot5
run6 <- runAnalysis('1000004882', '2016-01-02', '2016-07-16', TRUE)
run6$Plot5

##^ Test Zone ^##

#############################################################################################################################
  
  
  
  
#check on the param files
param1.path <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/SDA/run/ENS-00001-1000004927/sipnet.param"
param2.path <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/SDA/run/ENS-00001-646/sipnet.param"
#sipnet 2402
param1 <- readLines(param1.path)
param1 <- read.csv(textConnection(param1), sep="", header = F)
param2 <- readLines(param2.path)
param2 <- read.csv(textConnection(param2), sep="", header = F)