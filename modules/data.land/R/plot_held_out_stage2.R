# script to compare held-out samples of DBH measurements with the forecasts from 2010-2018:


# read in the posterior estimates that contain forecasts from 2010-2018 that we will validate:

library(rjags)
#library(PEcAn.data.land)
jags.comb <- NULL
file.base.name <- "stage2.xscaled.forecast.1000."
output.base.name <- "stage2.xscaled.forecast.1000"
stage2 <- TRUE
workingdir <- "/home/rstudio/"
#workingdir <- "/Users/kah/Documents/docker_pecan/pecan"
climate <- "wintP.wateryr"
cov.data = cov.data


jags.comb <- NULL

for(i in 190:200){ # note this model stopped early b/c convergence
  load(paste0(workingdir,"/IGF_PIPO_AZ_mcmc/", file.base.name,i,".RData"))
  new.out <- jags.out 
  
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        jags.comb[[j]] <- new.out[[j]][,x.cols]
      }else{
        jags.comb[[j]] <- new.out[[j]]
      }
      
    }
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)


# read in the held out DBH measurement dataframe
df.validation <- readRDS("INV_FIA_DATA/data/post2010.core.validation.tree2tree.rds")
#colnames(df.validation)[10] <- "T2_FIADB_PLOT"
colnames(Tree2Tree.incored.plots)
colnames(jags.stuff.stage2$cov.data)
train.dbh <- Tree2Tree.incored.plots[, c("T1_PLOT", "T1_SUBP", "T1_TREE", "T1_TRE_CN", "COUNTYCD", "T1_DIA", "T2_DIA", "T1_MEASYR", "T2_MEASYR", "T2_FIADB_PLOT" )] 
colnames(train.dbh)[1:5] <- c("PLOT", "SUBP", "T1_TREE", "TREE", "COUNTYCD")
colnames(train.dbh)[10] <- "FIADB_PLOT"

# make a unique key with the PLOT, SUBPLOT, TREE, and COUNTY

# "PLOT" in cov.data is the same as FIADB_PLOT in df.validation
unique(cov.data$PLOT) %in% unique(df.validation$FIADB_PLOT)

# the "TREE" in cov.data matches "T1_TRE_CN in Tree2Tree dataset,but renamed as "TREE" in train.dbh
unique(cov.data$TREE) %in% unique(train.dbh$TREE)

# need to join this infomration with df.validation, so lets see:
unique(train.dbh$FIADB_PLOT) %in% unique(df.validation$FIADB_PLOT)
unique(train.dbh$T1_TREE) %in% unique(df.validation$TREE)
unique(train.dbh$SUBP) %in% unique(df.validation$SUBP)


validation <- merge( df.validation, train.dbh, by.x = c("FIADB_PLOT", "TREE", "SUBP", "COUNTYCD"), by.y = c("FIADB_PLOT", "T1_TREE", "SUBP", "COUNTYCD"))
# fix column names

# join validation with cov.data by tree and FIADB_PLOT

unique(validation$TREE.y) %in% unique(cov.data$TREE)
unique(validation$FIADB_PLOT) %in% unique(cov.data$PLOT)


cov.data$id <- 1:1000 # set an id to preserve tree order in the output
cov.data.joined <- merge(cov.data, validation, by.x = c( "TREE", "PLOT"), by.y = c( "TREE.y", "FIADB_PLOT"), all.x = TRUE, sort = F)


cov.data.joined<- cov.data.joined[!duplicated(cov.data.joined),]
length(cov.data.joined$PLOT)
cov.data.ordered <- cov.data.joined[order(cov.data.joined$id),] # order the df based on the original tree order

# T2_FIADB is the plot number recorded in the FIA database:

# reformate df.validation so we have a dataframe
# out.of.sample <- data.frame(T2_FIADB = df.validation$PLOT,
#            SUBP = df.validation$SUBP,
#            TREE = df.validation$TREE, 
#            COUNTYCD = df.validation$COUNTYCD,
#            time = df.validation$max.invyr, 
#            DBH.held.out = df.validation$max.DIA*2.54)

# join with the PIPO model output individuals


# make the predicte and observed plots

model.out <- jags.comb

### DBH par(mfrow=c(3,2))

pdf("IGF.stage2.xscaled.forecast.1000.held_out_dbh.pdf")
layout(matrix(1:8, 4, 2, byrow = TRUE))
out      <- as.matrix(model.out) ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
x.cols   <- which(substr(colnames(out), 1, 1) == "x") # grab the state variable columns

ci      <- apply(out[, x.cols], 2, quantile, c(0.025, 0.5, 0.975))
ci.names <- parse.MatrixNames(colnames(ci), numeric = TRUE)
total.index <- 1:1000
index.smp <- cov.data.ordered[!is.na(cov.data.ordered$max.invyr),]$id # get the row index of all trees with additional measurements
#smp <- sample.int(data$ni, min(8, data$ni)) # select a random sample of 8 trees to plot
smp <- index.smp

in.sample.obs <- out.sample.obs<- list()

for (i in smp) {
  sel <- which(ci.names$row == i)
  rng <- c(range(ci[, sel], na.rm = TRUE), range(data$z[i, ], na.rm = TRUE))
  
  plot(data$time, ci[2, sel], type = "n", 
       ylim = range(rng), ylab = "DBH (cm)", xlab="Year", main = i)
  ciEnvelope(data$time, ci[1, sel], ci[3, sel], col = "lightBlue")
  points(data$time, data$z[i, ], pch = "+", cex = 1.5)
  points(cov.data.ordered[i,]$max.invyr, cov.data.ordered[i,]$max.DIA*2.54, pch = "*", col = "red",cex = 1.5)
  # lines(data$time,z0[i,],lty=2)
  in.sample.obs[[i]] <- data.frame(z.data = data$z[i, ], 
                                   year = data$time, 
                                   min.ci = ci[1,sel],
                                   mean.ci = ci[2,sel],
                                   max.ci = ci[3,sel])
  ci.year <- ci[,sel]
  colnames(ci.year)<- 1995:2018
  
  ci.year[1,as.character(cov.data.ordered[i,]$max.invyr)]
  
  out.sample.obs[[i]] <- data.frame(z.data = cov.data.ordered[i,]$max.DIA*2.54, 
                                    year = cov.data.ordered[i,]$max.invyr, 
                                    
                                    min.ci = ci.year[1,as.character(cov.data.ordered[i,]$max.invyr)],
                                    mean.ci = ci.year[2,as.character(cov.data.ordered[i,]$max.invyr)],
                                    max.ci = ci.year[3,as.character(cov.data.ordered[i,]$max.invyr)])
  #plot(pred.obs$z.data, pred.obs$mean.ci)
  #abline(a= 1, b =0)
  
  ## growth
  # sel      <- which(ci.names$row == i)
  # inc.mcmc <- apply(out[, x.cols[sel]], 1, diff)
  # inc.ci   <- apply(inc.mcmc, 1, quantile, c(0.025, 0.5, 0.975))
  # # inc.names = parse.MatrixNames(colnames(ci),numeric=TRUE)
  # 
  # plot(data$time[-1], inc.ci[2, ], type = "n", 
  #      ylim = range(inc.ci, na.rm = TRUE), ylab = "Increment (mm)", xlab="Year")
  # ciEnvelope(data$time[-1], inc.ci[1, ], inc.ci[3, ], col = "lightBlue")
  # ciEnvelope(data$time[-1], inc.ci[1, ], inc.ci[3, ], col = "lightBlue")
  # points(data$time, data$y[i, ] , pch = "+", cex = 1.5, type = "b", lty = 2)
}

dev.off()

# plot up the out of sample predictions for DBH:
out.sample.dbh.df <- do.call(rbind,   out.sample.obs)
summary.stats <- summary(lm(z.data ~ mean.ci, data = out.sample.dbh.df))


saveRDS(out.sample.dbh.df, "pred.obs.out.of.sample.dbh.rds")


p.o.out.of.sample <- ggplot()+
  geom_errorbar(data = out.sample.dbh.df, aes(z.data, ymin = min.ci, ymax = max.ci), color = "grey")+
  geom_point(data = out.sample.dbh.df, aes(z.data, mean.ci), size = 0.75)+
  geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+
  ylab("Predicted DBH (cm)")+xlab("Measured DBH (held-out samples)")+
  theme_bw(base_size = 12)+theme(panel.grid = element_blank())+ylim(15, 80)+xlim(15, 80)+
  geom_text(data=data.frame(summary.stats$r.squared), aes( label = paste("R.sq: ", round(summary.stats$r.squared, digits = 3), sep="")),parse=T,x=20, y=75)



# plot up the within-sample predictions for DBH:
in.sample.dbh.df <- do.call(rbind,   in.sample.obs)
in.sample.summary.stats <- summary(lm(z.data ~ mean.ci, data = in.sample.dbh.df))


saveRDS(in.sample.dbh.df, "pred.obs.within.sample.dbh.rds")


p.o.within.sample <-ggplot()+
  geom_errorbar(data = in.sample.dbh.df, aes(z.data, ymin = min.ci, ymax = max.ci), color = "grey")+
  geom_point(data = in.sample.dbh.df, aes(z.data, mean.ci), size = 0.75)+
  geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+
  ylab("Predicted DBH (cm)")+xlab("Measured DBH (within-sample)")+
  theme_bw(base_size = 12)+theme(panel.grid = element_blank())+ylim(15, 80)+xlim(15, 80)+
  geom_text(data=data.frame(in.sample.summary.stats$r.squared), aes( label = paste("R.sq: ", round(in.sample.summary.stats$r.squared, digits = 3), sep="")),parse=T,x=20, y=75)

png(height = 5, width = 10, units = "in", res = 300, paste0(output.base.name, "_DBH_held_out_p.o.plots.png"))
grid.arrange(p.o.within.sample, p.o.out.of.sample, ncol = 2)
dev.off()


# Plot up the parameter estimates next to each tree + the alpha plot estimate:
# dataframes needed: 
# ests.out: df with beta mcmc estimates, taus, and alpha_plots
# the jags.comb 
# get ests.out 
ests.out <- readRDS(file=paste0("IGF",output.base.name,".rds"))
ests.out <- as.matrix(ests.out)
# get the betas
betas <- ests.out[,grep(pattern = "beta",colnames(ests.out))]
betas.m <- reshape2::melt(betas)
beta.summary <- betas.m %>% group_by(Var2) %>% summarise(median = quantile(value, 0.5, na.rm=TRUE), 
                                                        ci.low = quantile(value, 0.025, na.rm=TRUE),
                                                        ci.high = quantile(value, 0.975, na.rm=TRUE),)

  
# get the alphas
alphas <- ests.out[,grep(pattern = "alpha_PLOT",colnames(ests.out))]
alphas.m <- reshape2::melt(alphas)
alpha.summary<- alphas.m %>% group_by(Var2) %>% summarise(median = quantile(value, 0.5, na.rm=TRUE), 
                                                        ci.low = quantile(value, 0.025, na.rm=TRUE),
                                                        ci.high = quantile(value, 0.975, na.rm=TRUE),)

  
pdf("IGF.stage2.xscaled.forecast.1000.held_out_dbh_param_estimates.pdf")
layout(matrix(1:8, 4, 2, byrow = TRUE))
out      <- as.matrix(model.out) ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
x.cols   <- which(substr(colnames(out), 1, 1) == "x") # grab the state variable columns

ci      <- apply(out[, x.cols], 2, quantile, c(0.025, 0.5, 0.975))
ci.names <- parse.MatrixNames(colnames(ci), numeric = TRUE)
total.index <- 1:1000
index.smp <- cov.data.ordered[!is.na(cov.data.ordered$max.invyr),]$id # get the row index of all trees with additional measurements
#smp <- sample.int(data$ni, min(8, data$ni)) # select a random sample of 8 trees to plot
smp <- index.smp

# get the plot id for the tree:


effects.plots.list <- plots.list <- in.sample.obs <- out.sample.obs<- list()

for (i in smp) {
  sel <- which(ci.names$row == i)
  rng <- c(range(ci[, sel], na.rm = TRUE), range(data$z[i, ], na.rm = TRUE))
  
  plot(data$time, ci[2, sel], type = "n", 
       ylim = range(rng), ylab = "DBH (cm)", xlab="Year", main = i)
  ciEnvelope(data$time, ci[1, sel], ci[3, sel], col = "lightBlue")
  points(data$time, data$z[i, ], pch = "+", cex = 1.5)
  points(cov.data.ordered[i,]$max.invyr, cov.data.ordered[i,]$max.DIA*2.54, pch = "*", col = "red",cex = 1.5)
  
  predicted.dbh.df <- data.frame(time = data$time, 
                                 med = ci[2,sel],
                                 ci.lo = ci[1, sel], 
                                 ci.hi = ci[3, sel],
                                 insample.dbh =  data$z[i,],
                                 tree = i)
  dbh.ggplot <- ggplot()+geom_ribbon(data = predicted.dbh.df, aes(x = time, ymin = ci.lo, ymax = ci.hi), fill = "lightblue")+
    geom_point(data = predicted.dbh.df,aes(x = time, y = insample.dbh))+ylab("Year")+ggtitle(predicted.dbh.df$tree)+theme_bw(base_size = 12)+theme(panel.grid = element_blank(), axis.title.x = element_blank())
 
  plt.tbl <- data.frame(PLOT = levels(cov.data$PLOT), plotid = 1:253)
  alp.id <- plt.tbl[plt.tbl$PLOT %in% cov.data[i,]$PLOT,]$plotid # get the alpha id for this
  summary.table$Var2
  summary.table<- rbind(beta.summary, alpha.summary[alpha.summary$Var2 == paste0("alpha_PLOT[",alp.id,"]"),])

  effects.ggplot <- ggplot()+geom_errorbar(data = summary.table, aes(x = Var2, ymin = ci.low, ymax = ci.high), width = 0.1)+
    geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red")+
    geom_point(data = summary.table, aes(x = Var2, y = median))+ylab("Posterior Effects")+xlab("")+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
  

  plots.list[[i]] <- dbh.ggplot
  effects.plots.list[[i]] <- effects.ggplot
}
dev.off()


# now select a certain subset of effects plots lists and print out to png:
# we probably dont need to print all of them right now:
png(height = 20, width = 8.5, units = "in", res = 200, "test.effects_and_pred_dbh.png")
cowplot::plot_grid(plots.list[[1]], effects.plots.list[[1]], 
             plots.list[[2]], effects.plots.list[[2]],
             plots.list[[3]], effects.plots.list[[3]],
             plots.list[[4]], effects.plots.list[[4]],
             #plots.list[[5]], effects.plots.list[[5]],
             plots.list[[6]], effects.plots.list[[6]], ncol = 2, align = "hv")
dev.off()
#ggsave("arrange2x2.pdf", marrangeGrob(grobs = plots.list , nrow=500, ncol=2))



# Plot up the time series of all validation individuals + data used to constrain the model + data used to validate

# Plot predicted vs. observed (for the held out dataset) + calculate the Rsq

# uncertainty in the forecast(?)