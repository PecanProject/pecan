library(PEcAn.all)
library(nimble)

# SIPNET @ Harvard Forest, ens size 1000 
setwd("/fs/data2/output//PEcAn_1000008768")
load("/fs/data2/output/PEcAn_1000008768/sda.all.runs.Rdata")

#function for plotting matplot with ensemble number as label
mattext = function(data, data_names, colors, ylab, xlab, type='b', na.fix = FALSE){
  if(na.fix == TRUE){
    data[is.na(data)] <- 0
  }
  
  matplot(data, pch=NA, type=type, col=colors, ylab = ylab, xlab = xlab)
  for (i in 1:ncol(data)){
    text(x=1:nrow(data), y=data[,i], lab=data_names[i], col=colors[i])
  }
}

#calculate the likelihood of the ensemble members given mu.a and Pa
nens <- nrow(FORECAST[[1]])
nt <- length(FORECAST)

wt.mat <- matrix(NA,nrow=nens,ncol=nt)
for(t in seq_len(nt)){
  for(i in seq_len(nens)){
    wt.mat[i,t]<-dmnorm_chol(FORECAST[[t]][i,],enkf.params[[t]]$mu.a,solve(enkf.params[[t]]$Pa))
  }
}


#put into weights table  
wt.props <- t(prop.table(wt.mat,2))


pdf(file.path(settings$outdir,'ensemble.weights.time-series.pdf'))
par(mfrow=c(1,1))
mattext(data = wt.props,data_names = as.character(1:nens),colors=rainbow(nens),
        ylab = c('Ensemble Weight'), xlab = c('Time'))
dev.off()

library(Hmisc)
settings <- read.settings("pecan.SDA.xml")




years <- 1961:2010
# get ensemble member names used in SDA
ens.names <- basename(unlist(ens.inputs))
ens.names <- gsub(".1960.*", "", ens.names)


# load orig ensembles to associate names with values
load("/fs/data3/istfer/AGU17/met_ensembles/agg_ensembles.Rdata")

# match values 
ens.hist <- array(NA,dim=c(nens, nt))
for(me in seq_len(nt)){
  inds <- sapply(ens.names, function(x) which(rownames(pre.mat) == x))
  ens.hist[, me] <- pre.mat[inds, (me+1)]
}

wt.mean <- wt.var <- numeric(nt)



pre.df <- data.frame(t(pre.mat))
pre.df$year <- 1960:2010
pre.dfm <- reshape::melt(pre.df, id = c("year"))
pre.dfm$year <- as.factor(pre.dfm$year)
pre.dfm$variable <- as.factor(pre.dfm$variable)
pre.dfm$value <- as.numeric(pre.dfm$value)

fontsize = list(title = 18, axis = 14)
theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = fontsize$axis),
                                  panel.grid.minor = element_blank(),
                                  panel.border = element_blank(),
                                  axis.text.y = element_text(size = fontsize$axis)))

ggplot(pre.dfm, aes(x = value, y = year)) + 
  geom_density_ridges(fill = "dodgerblue1") + theme(legend.position="none")+ coord_flip()

pre.dfm$variable <- "prior"
pre.dfm$variable <- as.factor(pre.dfm$variable)


for(t in 1:(nt)){
  wt.mean[t] <- wtd.mean(x=ens.hist[,t], weights = wt.props[t,])
  
  if(all(is.nan(wt.props[t,]))){ 
    wt.props[t,] <- 1
  }
  
  tempw <- wt.props[t,] 
  tempw[tempw < 0 ] <- 0 
  wt.var[t] <- wtd.var(x=ens.hist[,t], weights = tempw)
  
  tmpbw <- density(ens.hist[,t])
  
  
  dens <- density(ens.hist[,t], weights = tempw, na.rm = TRUE)
  bu <- 1000*(dens$y * dens$bw)
  bu[bu<1] <- 1
  bo <- rep(dens$x, times = bu)
  
  bo.df <- data.frame(year= rep(years[t], length(bo)),
                      variable = rep("posterior", length(bo)),
                      value = bo)
  pre.dfm <- rbind(pre.dfm,bo.df)
}


pre.dfm$variable <- as.factor(pre.dfm$variable)

ggplot(pre.dfm, aes(x = value, y = year, fill = variable)) + 
  geom_density_ridges() + theme(legend.position="none")+ coord_flip()+ stat_density_ridges(bandwidth = 0.0735)+
  scale_fill_manual(values = c("dodgerblue1", "lightcoral"))

# do same for tmp

# tmp.dfm$variable <- "prior"
# tmp.dfm$variable <- as.factor(tmp.dfm$variable)
# 
# 
# tmp.df <- data.frame(t(tmp.mat))
# tmp.df$year <- 1960:2010
# tmp.dfm <- reshape::melt(tmp.df, id = c("year"))
# tmp.dfm$year <- as.factor(tmp.dfm$year)
# tmp.dfm$variable <- as.factor(tmp.dfm$variable)
# tmp.dfm$value <- as.numeric(tmp.dfm$value)
# 
# ggplot(tmp.dfm, aes(x = value, y = year)) + 
#   geom_density_ridges(fill = "dodgerblue1") + theme(legend.position="none")+ coord_flip() 


