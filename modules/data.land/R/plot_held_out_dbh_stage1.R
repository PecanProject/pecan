# script to compare held-out samples of DBH measurements with the forecasts from 2010-2018:
### first, some helper functions...

##' @name parse.MatrixNames
##' @title parse.MatrixNames
##' @author Michael Dietze
##' @param w mcmc object containing matrix outputs
##' @param pre prefix (variable name) for the matrix variable to be extracted
##' @param numeric boolean, whether to coerce class to numeric
##' @return matrix
##' @export
parse.MatrixNames <- function(w, pre = "x", numeric = FALSE) {
  w <- sub(pre, "", w)
  w <- sub("[", "", w, fixed = TRUE)
  w <- sub("]", "", w, fixed = TRUE)
  w <- matrix(unlist(strsplit(w, ",")), nrow = length(w), byrow = TRUE)
  if (numeric) {
    class(w) <- "numeric"
  }
  colnames(w) <- c("row", "col")
  return(as.data.frame(w))
} # parse.MatrixNames

#' plots a confidence interval around an x-y plot (e.g. a timeseries)
#' 
#' @param x Vector defining CI center
#' @param ylo Vector defining bottom of CI envelope
#' @param yhi Vector defining top of CI envelope
#' @export 
#' @author Michael Dietze, David LeBauer
ciEnvelope <- function(x, ylo, yhi, ...) {
  m   <- rbind(x, ylo, yhi)
  nas <- which(apply(is.na(m), 2, sum) > 0)
  if (length(nas) > 0) {
    ## break overall dataset into complete blocks
    sub.m <- list()
    for (i in seq_along(nas)) {
      if (i == 1) {
        if (nas[i] > 1) {
          sub.m[[i]] <- m[, 1:(nas[i] - 1)]
        }
      } else {
        if (nas[i] > (nas[i - 1] + 1)) {
          ## if NAs are not consecutive
          sub.m[[i]] <- m[, (nas[i - 1] + 1):(nas[i] - 1)]
        }
      }
    }
  } else {
    sub.m <- list(m = m)
  }
  for (i in seq_along(sub.m)) {
    x <- sub.m[[i]]["x", ]
    ylo <- sub.m[[i]]["ylo", ]
    yhi <- sub.m[[i]]["yhi", ]
    polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi), ylo[1])), border = NA, ...)
  }
} # ciEnvelope


# read in the posterior estimates that contain forecasts from 2010-2018 that we will validate:

library(rjags)
#library(PEcAn.data.land)
jags.comb <- NULL
file.base.name <- "X2_Xscaled_forecasted_2018_rand_slope."
output.base.name <- "X2_Xscaled_forecasted_2018_rand_slope"
stage2 <- TRUE
workingdir <- "/home/rstudio/"
#workingdir <- "/Users/kah/Documents/docker_pecan/pecan"
climate <- "wintP.wateryr"
cov.data = cov.data


jags.comb <- NULL

for(i in 318:326){ # note this model stopped early b/c convergence
  load(paste0(workingdir,"/IGF_PIPO_AZ_mcmc/cyverse_runs/", file.base.name,i,".RData"))
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
df.validation <- readRDS("INV_FIA_DATA/data/post2010.core.validation.rds")
df.validation <- readRDS("FIA_inc_data/post2010.core.validation.rds")

jags.data <- readRDS("jags.data.basic.rds")
cov.data <- jags.data$cov.data
data <- jags.data$data
data$time <- 1966:2018
data$y <- cbind(data$y, matrix(NA, nrow = nrow(data$y), ncol = 8))
colnames(data$y)<- 1966:2018
data$z <- cbind(data$z, matrix(NA, nrow = nrow(data$y), ncol = 8))
colnames(data$z)<- 1966:2018
data$nt <- length(1966:2018)
data$nt2 <- rep(length(1966:2018), 515)
data$endyr <- rep(length(1966:2018), 515)

yvals <- data$y
ndex.dups <- duplicated(yvals)
yvals.new <- yvals[!ndex.dups,]
#colnames(df.validation)[10] <- "T2_FIADB_PLOT"
# df.validation contains the PLOT, SUBP, TREE, COUNTYCD to match with the temp2 or cov.data dataframes
cov.data[cov.data$T2_FIADB %in% df.validation$PLOT,]

cored <- newtemp2[, c("PlotNo", "SubplotNo", "TreeNo", "CountyNo", "DBH", "T1_DIA", "T2_DIA", "T1_MEASYR", "T2_MEASYR", "T2_FIADB_PLOT" )] 
colnames(cored)[1:4] <- c("PlotNo", "SUBP", "TREE", "COUNTYCD")
colnames(cored)[10] <- "PLOT"
cored <- cored[!duplicated(cored),]

cored$id <- 1:length(cored$PlotNo) # set an id to preserve tree order in the output
cov.data.joined <- merge(cored, df.validation[, c("PLOT", "SUBP", "TREE", "COUNTYCD", "max.invyr", "max.DIA")], by = c( "SUBP", "TREE", "COUNTYCD", "PLOT"), all.x = TRUE, sort = F)
cored[1:3,]

cov.data.joined<- cov.data.joined[!duplicated(cov.data.joined),]
length(cov.data.joined$PLOT)
cov.data.ordered <- cov.data.joined[order(cov.data.joined$id),] # order the df based on the original tree order
#cov.data.ordered <- cov.data.joined
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

pdf(paste0("IGF.", output.base.name,".pdf"))

layout(matrix(1:8, 4, 2, byrow = TRUE))
out      <- as.matrix(model.out) ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
x.cols   <- which(substr(colnames(out), 1, 1) == "x") # grab the state variable columns

ci      <- apply(out[, x.cols], 2, quantile, c(0.025, 0.5, 0.975))
ci.names <- parse.MatrixNames(colnames(ci), numeric = TRUE)
total.index <- 1:544
index.smp <- cov.data.ordered[!is.na(cov.data.ordered$max.invyr),]$id # get the row index of all trees with additional measurements
#smp <- sample.int(data$ni, min(8, data$ni)) # select a random sample of 8 trees to plot
smp <- index.smp[1:181]

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
  colnames(ci.year)<- 1966:2018
  
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
  theme_bw(base_size = 12)+theme(panel.grid = element_blank())+ylim(0, 80)+xlim(0, 80)+
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
  theme_bw(base_size = 12)+theme(panel.grid = element_blank())+ylim(0, 80)+xlim(0, 80)+
  geom_text(data=data.frame(in.sample.summary.stats$r.squared), aes( label = paste("R.sq: ", round(in.sample.summary.stats$r.squared, digits = 3), sep="")),parse=T,x=20, y=75)

png(height = 5, width = 10, units = "in", res = 300, paste0(output.base.name, "_DBH_held_out_p.o.plots.png"))
grid.arrange(p.o.within.sample, p.o.out.of.sample, ncol = 2)
dev.off()



# get ests.out 
ests.out <- readRDS(file=paste0("IGF",output.base.name,".rds"))
ests.out <- as.matrix(ests.out)
# get the betas
betas <- ests.out[,grep(pattern = "beta",colnames(ests.out))]
betas <- betas[ ,c("betaX", "betaX2", "betaSDI", "betatmax.fallspr", "betawintP.wateryr",
                  "betaX_SDI", "betaX_tmax.fallspr", "betaX_wintP.wateryr", "betatmax.fallspr_wintP.wateryr", 
                  "betaSDI_wintP.wateryr", "betaSDI_tmax.fallspr")]
betas.m <- reshape2::melt(betas)
beta.summary <- betas.m %>% group_by(Var2) %>% summarise(median = quantile(value, 0.5, na.rm=TRUE), 
                                                         ci.low = quantile(value, 0.025, na.rm=TRUE),
                                                         ci.high = quantile(value, 0.975, na.rm=TRUE),)


betasX <- ests.out[,grep(pattern = "betaX_PLOT",colnames(ests.out))]
betasX.m <- reshape2::melt(betasX)
betaX.summary <- betasX.m %>% group_by(Var2) %>% summarise(median = quantile(value, 0.5, na.rm=TRUE), 
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
total.index <- 1:515
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
  
  plt.tbl <- data.frame(PLOT = levels(cov.data$PLOT), plotid = 1:290)
  alp.id <- plt.tbl[plt.tbl$PLOT %in% cov.data[i,]$PLOT,]$plotid # get the alpha id for this
  #summary.table$Var2
  summary.table <- rbind(betaX.summary[betaX.summary$Var2 == paste0("betaX_PLOT[",alp.id,"]"),],
                         beta.summary, 
                        alpha.summary[alpha.summary$Var2 == paste0("alpha_PLOT[",alp.id,"]"),])
  
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
                   #plots.list[[2]], effects.plots.list[[2]],
                   #plots.list[[3]], effects.plots.list[[3]],
                   #plots.list[[4]], effects.plots.list[[4]],
                   plots.list[[5]], effects.plots.list[[5]],
                   plots.list[[6]], effects.plots.list[[6]],
                   plots.list[[7]], effects.plots.list[[7]], ncol = 2, align = "hv")
dev.off()
#ggsave("arrange2x2.pdf", marrangeGrob(grobs = plots.list , nrow=500, ncol=2))



# Plot up the time series of all validation individuals + data used to constrain the model + data used to validate

# Plot predicted vs. observed (for the held out dataset) + calculate the Rsq

# uncertainty in the forecast(?)


# Plot up the time series of all validation individuals + data used to constrain the model + data used to validate

# Plot predicted vs. observed (for the held out dataset) + calculate the Rsq

# uncertainty in the forecast(?)