# Script to use the posterior estimates from Stage2 (+ random intercepts + random betaX slope) to understand the effects of changing management & climate on regions of AZ pipo
# Author: Kelly Heilman
# 1. Extract posterior estimates from stage 2 model
# 2. For each plot & Tree, generate the covariate data we will use to generate scenarios:
#       a. calculate average climate
#       b. get the estimates of the current state (DBH)
#       c. get the estimates of current SDI
#       d. associate with tree and dbh's
# 3. For each plot and tree generate the predicted tree growth using the posterior parameter estimates
#       a. Start with the predicted tree growth for the next year given average conditions, current SDI
# 4. Plot out spatially the predicted tree growth (and the range of CI) (3 maps for visualization).
# 5. Do the same for +10% current SDI & -10% current SDI

#------------------------------------------------------------------------------------------
# 1. Extract posterior estimates from stage 2 or stage 1 model
#------------------------------------------------------------------------------------------
stage2 = FALSE

# assuming that we already ran the conditional_effects_mcmc_plots script, and that output.base.name is in our env
jags.comb.params <- readRDS(file=paste0("IGF",output.base.name,".rds"))
out <- as.matrix(jags.comb.params)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]


# get the alpha_PLOT intercept random effects:
alphas <- out[,grep(pattern = "alpha_PLOT",colnames(out))]

# get the betaX_PLOT slope random effects:
# note that I forgot to save the betaX_PLOTS on this run, so we dont have these estimates, but lets build the framework for generating the postior ests:
betaXplots <- out[,grep(pattern = "betaX_PLOT",colnames(out))]

# get the estimated x values for each tree/plot:
xval.ests<- readRDS(paste0(output.base.name,"_estimated_xvals.RDS"))
x.mat <- as.matrix(xval.ests)
x.ci      <- apply(x.mat , 2, quantile, c(0.025, 0.5, 0.975))
x.ci[, "x[1,24]"]
hist(x.mat[,"x[1,24]"])

# make sure we have the average climate conditions for each plot/tree:
# We have plot MAP and MAT standarized in cov.data:


# Add the plot index to cov.data
plot.table <- data.frame(PLOT = unique(cov.data$PLOT), 
                         plotid = 1:length(unique(cov.data$PLOT)))

cov.data <- left_join(plot.table, cov.data, by  = "PLOT")
cov.data$id <- 1:length(cov.data$PLOT)

# KH note: need to standardize SDIseq first sdece I ran the model with standardized covariates
SDIseq.real <- seq(0,400,by=10)
SDIseq <- (SDIseq.real-mean(temp2$SDI))/sd(temp2$SDI)

# we want to fill a matrix with rows as mcmc samples and columns as the individual
# this loop is slow, need to use apply function:


decpred <- matrix(NA, ns, length(cov.data$PLOT))
#decpred <- matrix(NA, ns, 3)
for(m in 1:length(cov.data$PLOT)){#length(cov.data$PLOT)){ # loop for each individual
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid,"]")
  
  xname <- paste0("x[", cov.data[m,]$id,",24]")
  # get the last x--just use the mean for now for that individual and scale by 30:
  x <- mean(x.mat[,xname])-30
  
  for(k in 1:ns){# loop for each mcmc iterations
    #j <- i[k]
    j <- k
    decpred[k,m]  <- alphas[j, alphaplotid] + B0[j] + betas[j,"betaSDI"]*cov.data[m,]$SDI + betas[j,"betaSDI_wintP.wateryr"]*cov.data[m,]$SDI*cov.data[m,]$MAP + 
      betas[j,"betaX"]*x + betas[j,"betaX2"]*(x)*x +
      betas[j,"betaX_SDI"]*x*cov.data[m,]$SDI  + betas[j,"betaX_wintP.wateryr"]*x*cov.data[m,]$MAP +
      betas[j,"betawintP.wateryr"]*cov.data[m,]$MAP + 
      # temperature
      betas[j,"betaX_tmax.fallspr"]*x*cov.data[m,]$MAT +
      betas[j,"betatmax.fallspr"]*cov.data[m,]$MAT + betas[j,"betaSDI_tmax.fallspr"]*cov.data[m,]$SDI*cov.data[m,]$MAT +
      betas[j,"betatmax.fallspr_wintP.wateryr"]*cov.data[m,]$MAT*cov.data[m,]$MAP # +
    
  }
  print(m)
}

CIdecpred <- apply(decpred,2,quantile,c(0.025,0.5,0.975))



decpred <- matrix(NA, ns, length(cov.data$PLOT))
#decpred <- matrix(NA, ns, 3)
#for(m in 1:length(cov.data$PLOT)){#length(cov.data$PLOT)){ # loop for each individual

predict.dec <- function(m, SDI.scale = 1){
  alphaplotid <- paste0("alpha_PLOT[", cov.data[m,]$plotid,"]")
  
  xname <- paste0("x[", cov.data[m,]$id,",24]")
  # get the last x--just use the mean for now for that individual and scale by 30:
  x <- mean(x.mat[,xname])-30
  decpred <- matrix(NA, ns, 1)
  SDI <- cov.data[m,]$SDI*SDI.scale # option to scale all SDI values by a given #
  
  # for(k in 1:ns){# loop for each mcmc iterations
  #   #j <- i[k]
  #   j <- k
  #   
  
  
  basic.mod <- function(j){
    
    decpred  <- alphas[j, alphaplotid] + B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*cov.data[m,]$MAP + 
      betas[j,"betaX"]*x + betas[j,"betaX2"]*(x)*x +
      betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_wintP.wateryr"]*x*cov.data[m,]$MAP +
      betas[j,"betawintP.wateryr"]*cov.data[m,]$MAP + 
      # temperature
      betas[j,"betaX_tmax.fallspr"]*x*cov.data[m,]$MAT +
      betas[j,"betatmax.fallspr"]*cov.data[m,]$MAT + betas[j,"betaSDI_tmax.fallspr"]*SDI*cov.data[m,]$MAT +
      betas[j,"betatmax.fallspr_wintP.wateryr"]*cov.data[m,]$MAT*cov.data[m,]$MAP # +
    decpred
  }
  
  mcmc.samps<- lapply(1:500, FUN = basic.mod)
  mcmcpred <- do.call(rbind,mcmc.samps)
  colnames(mcmcpred) <- m
  
  mcmcpred
}
# print(m)
#}

# Note if plotting stage2, then sample form 1:2500
#  if plotting stage2, then sample from 1:515
mlist <- list()
if(stage2==TRUE){
  mlist <- sample(100, x = 1:2500, replace = FALSE)
}else{
  mlist <- sample(100, x = 1:515, replace = FALSE) 
}


# the ids of individuals to scale over:
# create a list of all the individual predictions of SDI
# note that this still takes awhile
system.time(all.1000 <- lapply(mlist, predict.dec, SDI.scale = 1))

cis <- lapply(all.1000, quantile, c(0.025, 0.5, 0.975))

# decrease SDI in all plots by 15%
system.time(all.100.decSDI <- lapply(mlist, predict.dec, SDI.scale = 0.85))
low.sdi.cis <- lapply(all.100.decSDI, quantile, c(0.025, 0.5, 0.975))
system.time(all.100.decSDI <- lapply(mlist, predict.dec, SDI.scale = 1.15))
hi.sdi.cis <- lapply(all.100.decSDI, quantile, c(0.025, 0.5, 0.975))

low.sdi <- data.frame(do.call(rbind, low.sdi.cis))
hi.sdi <- data.frame(do.call(rbind, hi.sdi.cis))
nochange.sdi <- data.frame(do.call(rbind, cis))

# hacky but specify differentids
low.sdi$id <- mlist
hi.sdi$id <- mlist
nochange.sdi$id <- mlist

colnames(low.sdi) <- c("lowsdi.ci.lo", "lowsdi.ci.50", "lowsdi.ci.hi", "id")
colnames(hi.sdi) <- c("hisdi.ci.lo", "hisdi.ci.50", "hisdi.ci.hi", "id")
colnames(nochange.sdi) <- c("midsdi.ci.lo", "midsdi.ci.50", "midsdi.ci.hi", "id")



# merge with cov.data and lat long values:

cov.data.subset <- cov.data[mlist,]

cov.data.subset <- merge(cov.data.subset, low.sdi, by = "id")
cov.data.subset <- merge(cov.data.subset, hi.sdi, by = "id")
cov.data.subset <- merge(cov.data.subset, nochange.sdi, by = "id")

cov.data.subset
# below only works for stage 2 data
# get lat & long for each onee:
if(stage2 == TRUE){
  ll <- Tree2Tree.decored.plots[,c("LAT", "LON", "PLOT", "COUNTYCD")]
  ll$PLOT.cov.data <- paste0(ll$COUNTYCD, ll$PLOT)
  ll <- ll[!duplicated(ll),]
  
  ll.cov <- merge(cov.data.subset, ll[,c("LAT", "LON", "PLOT.cov.data")], by.x = "PLOT", by.y = "PLOT.cov.data", all.x =TRUE)
}else{
  ll.cov <- cov.data.subset
}
# calculate change in growth decrement relative to current SDI:

ll.cov$decSDI.diff <- ll.cov$lowsdi.ci.50 - ll.cov$midsdi.ci.50
ll.cov$decSDI.diff.lo <- ll.cov$lowsdi.ci.lo - ll.cov$midsdi.ci.lo
ll.cov$decSDI.diff.hi <- ll.cov$lowsdi.ci.hi - ll.cov$midsdi.ci.hi


ll.cov$decSDI.diff <- ll.cov$hisdi.ci.50 - ll.cov$midsdi.ci.50
ll.cov$decSDI.diff.lo <- ll.cov$hisdi.ci.lo - ll.cov$midsdi.ci.lo
ll.cov$decSDI.diff.hi <- ll.cov$hisdi.ci.hi - ll.cov$midsdi.ci.hi


ggplot()+
  geom_point(data = ll.cov, aes(LON, midsdi.ci.50), size = 0.75)+
  geom_errorbar(data = ll.cov, aes(x = LON, ymin = midsdi.ci.lo, ymax = midsdi.ci.hi))+theme_bw()+theme(legend.position = "bottom")+ylab("Predicted growth decrement")

ggplot()+
  geom_point(data = ll.cov, aes(LAT, midsdi.ci.50), size = 0.75)+
  geom_errorbar(data = ll.cov, aes(x = LAT, ymin = midsdi.ci.lo, ymax = midsdi.ci.hi))+theme_bw()+theme(legend.position = "bottom")+ylab("Predicted growth decrement")


ggplot()+
  geom_point(data = ll.cov, aes(LON, decSDI.diff), size = 0.75)+
  geom_errorbar(data = ll.cov, aes(x = LON, ymin = decSDI.diff.lo, ymax =decSDI.diff.hi))+theme_bw()+theme(legend.position = "bottom")+ylab("Predicted growth decrement")


ggplot()+
  geom_point(data = ll.cov, aes(LAT, decSDI.diff), size = 0.75)+
  geom_errorbar(data = ll.cov, aes(x = LAT, ymin = decSDI.diff.lo, ymax =decSDI.diff.hi))+theme_bw()+theme(legend.position = "bottom")+ylab("Predicted growth decrement")

library(maps)
library(sp)
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'arizona') )
coordinates(all_states)<-~long+lat
class(all_states)
mapdata <- data.frame(states)

ggplot()+ geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_point(data = ll.cov, aes(LON, LAT, color = midsdi.ci.50), size = 0.75)+scale_color_gradientn(colors = c("red", "yellow", "blue"))+theme_bw()+coord_equal()+theme(legend.position = "bottom")

ggplot()+ geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_point(data = ll.cov, aes(LON, LAT, color = midsdi.ci.lo), size = 0.75)+
  scale_color_gradientn(colors = c("red", "yellow", "blue"))+theme_bw()+coord_equal()+theme(legend.position = "bottom")

ggplot()+ geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_point(data = ll.cov, aes(LON, LAT, color = midsdi.ci.hi), size = 0.75)+
  scale_color_gradientn(colors = c("red", "yellow", "blue"))+theme_bw()+coord_equal()+theme(legend.position = "bottom")




ggplot()+ geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_point(data = ll.cov, aes(LON, LAT, color = lowsdi.ci.50), size = 0.75)+
  scale_color_gradientn(colors = c("red", "yellow", "blue"), limits = c(-0.2, 0.45))+theme_bw()+coord_equal()+theme(legend.position = "bottom", legend.title = element_blank())

ggplot()+ geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_point(data = ll.cov, aes(LON, LAT, color = lowsdi.ci.lo), size = 0.75)+
  scale_color_gradientn(colors = c("red", "yellow", "blue"), limits = c(-0.2, 0.45))+theme_bw()+coord_equal()+theme(legend.position = "bottom", legend.title = element_blank())

ggplot()+ geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_point(data = ll.cov, aes(LON, LAT, color = lowsdi.ci.lo), size = 0.75)+
  scale_color_gradientn(colors = c("red", "yellow", "blue"), limits = c(-0.2, 0.45))+theme_bw()+coord_equal()+theme(legend.position = "bottom", legend.title = element_blank())

# difference maps:
# for decreasing SDI
dec.meandiff <- ggplot()+ geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_point(data = ll.cov, aes(LON, LAT, color = decSDI.diff), size = 0.75)+
  scale_color_gradientn(colors = c("red", "white", "blue"), limits = c(-0.03, 0.03))+theme_bw()+coord_equal()+theme(legend.title = element_blank())+ggtitle("Effect of 15% SDI decrease (median)")

dec.hi.diff<- ggplot()+ geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_point(data = ll.cov, aes(LON, LAT, color = decSDI.diff.hi), size = 0.75)+
  scale_color_gradientn(colors = c("red", "white", "blue"), limits = c(-0.03, 0.03))+theme_bw()+coord_equal()+theme( legend.title = element_blank())+ggtitle("Effect of 15% SDI decrease (0.975%)")

dec.lo.diff<- ggplot()+ geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_point(data = ll.cov, aes(LON, LAT, color = decSDI.diff.hi), size = 0.75)+
  scale_color_gradientn(colors = c("red", "white", "blue"), limits = c(-0.03, 0.03))+theme_bw()+coord_equal()+theme(legend.title = element_blank())+ggtitle("Effect of 15% SDI decrease (0.025%)")

png(height = 3, width = 10, units = "in", res = 300, paste0(output.base.name,"_decreasing.SDI.15percent.change.decrement.png"))
cowplot::plot_grid(dec.lo.diff, dec.meandiff, dec.hi.diff, ncol = 3)
dev.off()


dec.meandiff <- ggplot()+ geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_point(data = ll.cov, aes(LON, LAT, color = decSDI.diff), size = 0.75)+
  scale_color_gradientn(colors = c("red", "white", "blue"), limits = c(-0.03, 0.03))+theme_bw()+coord_equal()+theme(legend.title = element_blank())+ggtitle("Effect of 15% SDI decrease (median)")

dec.hi.diff<- ggplot()+ geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_point(data = ll.cov, aes(LON, LAT, color = decSDI.diff.hi), size = 0.75)+
  scale_color_gradientn(colors = c("red", "white", "blue"), limits = c(-0.03, 0.03))+theme_bw()+coord_equal()+theme( legend.title = element_blank())+ggtitle("Effect of 15% SDI decrease (0.975%)")

dec.lo.diff<- ggplot()+ geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_point(data = ll.cov, aes(LON, LAT, color = decSDI.diff.hi), size = 0.75)+
  scale_color_gradientn(colors = c("red", "white", "blue"), limits = c(-0.03, 0.03))+theme_bw()+coord_equal()+theme(legend.title = element_blank())+ggtitle("Effect of 15% SDI decrease (0.025%)")

png(height = 3, width = 10, units = "in", res = 300, paste0(output.base.name,"decreasing.SDI.15percent.change.decrement.png"))
cowplot::plot_grid(dec.lo.diff, dec.meandiff, dec.hi.diff, ncol = 3)
dev.off()




ggplot(ll.cov, aes(ELEV, midsdi.ci.50))+geom_point()

ggplot(ll.cov, aes(LAT, midsdi.ci.50))+geom_point()

ggplot(ll.cov, aes(LON, midsdi.ci.50))+geom_point()

ggplot(ll.cov, aes(LAT, LON, color = (midsdi.ci.50-lowsdi.ci.50)))+geom_point()

head(low.sdi)
Tree2Tree.decored.plots[,c("LAT", "LON", "T1_PLOT", "")]
cov.data.subset




