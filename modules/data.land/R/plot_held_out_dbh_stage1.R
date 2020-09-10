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
file.base.name <- "SDI_noSI.rand.X.nadapt.5000."
output.base.name <- "SDI_noSI.rand.X.nadapt.5000"
stage2 <- TRUE
workingdir <- "/home/rstudio/"
#workingdir <- "/Users/kah/Documents/docker_pecan/pecan"
climate <- "wintP.wateryr"
cov.data = cov.data


jags.comb <- NULL

for(i in 725:750){ # note this model stopped early b/c convergence
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
df.validation <- readRDS("data/post2010.core.validation.rds")
#df.validation <- readRDS("data/post2010.core.validation.rds")

jags.data <- readRDS("jags.data.new.rds")
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
#colnames(df.validation)[10] <- "T2_FIADB_PLOT"
# df.validation contains the PLOT, SUBP, TREE, COUNTYCD to match with the temp2 or cov.data dataframes
cov.data[cov.data$T2_FIADB %in% df.validation$PLOT,]

cored <- newtemp2[, c("PlotNo", "SubplotNo", "TreeNo", "CountyNo", "DBH", "T1_DIA", "T2_DIA", "T1_MEASYR", "T2_MEASYR", "T2_FIADB_PLOT" )] 
colnames(cored)[1:4] <- c("PlotNo", "SUBP", "TREE", "COUNTYCD")
colnames(cored)[10] <- "PLOT"

cored$id <- 1:515 # set an id to preserve tree order in the output
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
pdf(paste0(output.base.name,"_held_out_dbh.pdf"))
layout(matrix(1:8, 4, 2, byrow = TRUE))
out      <- as.matrix(model.out) ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
x.cols   <- which(substr(colnames(out), 1, 1) == "x") # grab the state variable columns

ci      <- apply(out[, x.cols], 2, quantile, c(0.025, 0.5, 0.975))
var.pred       <- apply(out[, x.cols], 2, var)
ci.names <- parse.MatrixNames(colnames(ci), numeric = TRUE)
total.index <- 1:515
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
                                   predvar = var.pred[sel], # calculate varince of the predictions
                                   min.ci = ci[1,sel],
                                   mean.ci = ci[2,sel],
                                   max.ci = ci[3,sel])
  ci.year <- ci[,sel]
  colnames(ci.year)<- 1966:2018
  
  ci.year[1,as.character(cov.data.ordered[i,]$max.invyr)]
  
  # var.pred
  var.year <- var.pred[sel]
  names(var.year)<- 1966:2018
  
  out.sample.obs[[i]] <- data.frame(z.data = cov.data.ordered[i,]$max.DIA*2.54, 
                                    year = cov.data.ordered[i,]$max.invyr, 
                                    predvar = var.year[as.character(cov.data.ordered[i,]$max.invyr)],
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


saveRDS(out.sample.dbh.df, paste0(output.base.name,"pred.obs.out.of.sample.dbh.rds"))


p.o.out.of.sample <- ggplot()+
  geom_errorbar(data = out.sample.dbh.df, aes(z.data, ymin = min.ci, ymax = max.ci), color = "grey")+
  geom_point(data = out.sample.dbh.df, aes(z.data, mean.ci), size = 0.75)+
  geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+
  ylab("Predicted DBH (cm)")+xlab("Measured DBH (held-out samples)")+
  theme_bw(base_size = 12)+theme(panel.grid = element_blank())+ylim(0, 80)+xlim(0, 80)#+
#geom_text(data=data.frame(summary.stats$r.squared), aes( label = paste("R.sq: ", round(summary.stats$r.squared, digits = 3), sep="")),parse=T,x=20, y=75)



# plot up the within-sample predictions for DBH:
in.sample.dbh.df <- do.call(rbind,   in.sample.obs)
in.sample.summary.stats <- summary(lm(z.data ~ mean.ci, data = in.sample.dbh.df))


saveRDS(in.sample.dbh.df, paste0(output.base.name,"pred.obs.within.sample.dbh.rds"))


p.o.within.sample <-ggplot()+
  geom_errorbar(data = in.sample.dbh.df, aes(z.data, ymin = min.ci, ymax = max.ci), color = "grey")+
  geom_point(data = in.sample.dbh.df, aes(z.data, mean.ci), size = 0.75)+
  geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+
  ylab("Predicted DBH (cm)")+xlab("Measured DBH (within-sample)")+
  theme_bw(base_size = 12)+theme(panel.grid = element_blank())+ylim(0, 80)+xlim(0, 80)#+
#geom_text(data=data.frame(in.sample.summary.stats$r.squared), aes( label = paste("R.sq: ", round(in.sample.summary.stats$r.squared, digits = 3), sep="")),parse=T,x=20, y=75)

png(height = 5, width = 10, units = "in", res = 300, paste0(output.base.name, "_DBH_held_out_p.o.plots.png"))
grid.arrange(p.o.within.sample, p.o.out.of.sample, ncol = 2)
dev.off()

#-------------------------------------------------------------------------------------
# calculating Error statistics for model valiation:
#-------------------------------------------------------------------------------------
# want to calculate:
# MSPE-mean squared predictive error
# RMSE-root mean squared predictive error
# MAPE-mean absolute predictive error
# V1-check for bias over time
# V2-accuracy of MSPE estimates
# V3-goodness of fit to compare across models
# PPL- calculating posterior predictive loss for model comparison:

#PPL = sum((Zobs - predZ)^2) - sum(var(predZ))

# out of sample calculations
out.of.sample.validation.metrics <- out.sample.dbh.df %>% summarise(MSPE = mean((z.data-mean.ci)^2), 
                                                                    RMSPE = sqrt(mean((z.data-mean.ci)^2)),
                                                                    MAPE = mean(abs(z.data-mean.ci)), 
                                                                    V1 = mean(z.data-mean.ci)/(sum(predvar)^(1/2))/n(), # estimate of bias in predictors over time (close to 0 = unbiased)
                                                                    V2 = (mean((z.data-mean.ci)^2)/(sum(predvar)/n())^(1/2)),  # estimate of accuracy of MSPEs (close to 1 = accurate)
                                                                    V3 = (mean((z.data-mean.ci)^2)^(1/2)), # goodness of fit estimate (small = better fit)
                                                                    PPL = sum((z.data - mean.ci)^2) - sum(predvar)) # posterior predictive loss

out.of.sample.validation.metrics$validation <- "out-of-sample"              


# in of sample calculations
in.sample.validation.metrics <- in.sample.dbh.df %>% summarise(MSPE = mean((z.data-mean.ci)^2, na.rm =TRUE), 
                                                               RMSPE = sqrt(mean((z.data-mean.ci)^2, na.rm =TRUE)),
                                                               MAPE = mean(abs(z.data-mean.ci), na.rm =TRUE), 
                                                               V1 = mean(z.data-mean.ci, na.rm =TRUE)/(sum(predvar)^(1/2))/n(), # estimate of bias in predictors over time (close to 0 = unbiased)
                                                               V2 = (mean((z.data-mean.ci)^2, na.rm =TRUE)/(sum(predvar)/n()^(1/2))),  # estimate of accuracy of MSPEs (close to 1 = accurate)
                                                               V3 = (mean((z.data-mean.ci)^2,na.rm =TRUE)^(1/2)),
                                                               PPL = sum((z.data - mean.ci)^2, na.rm = TRUE) - sum(predvar, na.rm = TRUE)) # posterior predictive loss# goodness of fit estimate (small = better fit)
in.sample.validation.metrics$validation <- "in-sample"
# concatenate together + add the model name + description:
valid.metrics <- rbind(in.sample.validation.metrics, out.of.sample.validation.metrics)
valid.metrics$model <- output.base.name
valid.metrics$description <- "Model with SDI, no SI, random BetaX effects, plot random slopes"

write.csv(valid.metrics, paste0(output.base.name, "model.validation.stats.csv"), row.names = FALSE)


#---------------------------------------------------------------------------
# Plot residuals
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# Validation for Increment (within sample only)
#--------------------------------------------------------------------------
pdf(paste0(output.base.name,"_insample_increment.pdf"))
layout(matrix(1:8, 4, 2, byrow = TRUE))
out      <- as.matrix(model.out) ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
x.cols   <- which(substr(colnames(out), 1, 1) == "x") # grab the state variable columns

ci      <- apply(out[, x.cols], 2, quantile, c(0.025, 0.5, 0.975))
var.pred       <- apply(out[, x.cols], 2, var)
ci.names <- parse.MatrixNames(colnames(ci), numeric = TRUE)
total.index <- 1:515
index.smp <- cov.data.ordered[!is.na(cov.data.ordered$max.invyr),]$id # get the row index of all trees with additional measurements
#smp <- sample.int(data$ni, min(8, data$ni)) # select a random sample of 8 trees to plot
smp <- total.index

in.sample.inc <-  list()

for (i in smp) {
  sel <- which(ci.names$row == i)
  rng <- c(range(ci[, sel], na.rm = TRUE), range(data$z[i, ], na.rm = TRUE))
  
  
  # increment growth
  sel      <- which(ci.names$row == i)
  inc.mcmc <- apply(out[, x.cols[sel]], 1, diff)
  inc.ci   <- apply(inc.mcmc, 1, quantile, c(0.025, 0.5, 0.975))
  inc.names = parse.MatrixNames(colnames(ci),numeric=TRUE)
  var.pred.inc <- apply( inc.mcmc, 1, var)
  
  
  plot(data$time[-1], inc.ci[2, ], type = "n",
       ylim = range(inc.ci, na.rm = TRUE), ylab = "Increment (mm)", xlab="Year")
  ciEnvelope(data$time[-1], inc.ci[1, ], inc.ci[3, ], col = "lightBlue")
  ciEnvelope(data$time[-1], inc.ci[1, ], inc.ci[3, ], col = "lightBlue")
  points(data$time, data$y[i, ] , pch = "+", cex = 1.5, type = "b", lty = 2)
  
  in.sample.inc[[i]] <- data.frame(inc.data = data$y[i, -1],
                                   year = data$time[-1],
                                   predvar = var.pred.inc, # calculate varince of the predictions
                                   min.ci = inc.ci[1,],
                                   mean.ci = inc.ci[2,],
                                   max.ci = inc.ci[3,])
  
}

dev.off()

#------------------------Do validation & diagnostics for increment----------------------------

# plot up the out of sample predictions for increment:
in.sample.inc.df <- do.call(rbind,  in.sample.inc)
summary.stats <- summary(lm(inc.data ~ mean.ci, data =in.sample.inc.df))

saveRDS(in.sample.inc.df, paste0(output.base.name,"pred.obs.out.of.sample.inc.rds"))


p.o.inc.in.sample <- ggplot()+
  geom_errorbar(data = in.sample.inc.df, aes(inc.data, ymin = min.ci, ymax = max.ci), color = "grey")+
  geom_point(data = in.sample.inc.df, aes(inc.data, mean.ci), size = 0.75)+
  geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+
  ylab("Predicted increment (cm)")+xlab("Measured Increment (in-sample)")+
  theme_bw(base_size = 12)+theme(panel.grid = element_blank())+ylim(0, 2.5)+xlim(0, 2.5)#+
#geom_text(data=data.frame(summary.stats$r.squared), aes( label = paste("R.sq: ", round(summary.stats$r.squared, digits = 3), sep="")),parse=T,x=0.5, y=2.5)




png(height = 5, width = 5, units = "in", res = 300, paste0(output.base.name, "_increment_in_sample_p.o.plots.png"))
p.o.inc.in.sample
dev.off()


#-------------------------------------------------------------------------------------
# calculating Error statistics for model valiation: Increments
#-------------------------------------------------------------------------------------
# want to calculate:
# MSPE-mean squared predictive error
# RMSE-root mean squared predictive error
# MAPE-mean absolute predictive error
# V1-check for bias over time
# V2-accuracy of MSPE estimates
# V3-goodness of fit to compare across models
# PPL- calculating posterior predictive loss for model comparison:

#PPL = sum((Zobs - predZ)^2) - sum(var(predZ))


# in of sample calculations for increment
in.sample.validation.inc.metrics <- in.sample.inc.df %>% summarise(MSPE = mean((inc.data-mean.ci)^2, na.rm =TRUE), 
                                                                   RMSPE = sqrt(mean((inc.data-mean.ci)^2, na.rm =TRUE)),
                                                                   MAPE = mean(abs(inc.data-mean.ci), na.rm =TRUE), 
                                                                   V1 = mean(inc.data-mean.ci, na.rm =TRUE)/(sum(predvar)^(1/2))/n(), # estimate of bias in predictors over time (close to 0 = unbiased)
                                                                   V2 = (mean((inc.data-mean.ci)^2, na.rm =TRUE)/(sum(predvar)/n()^(1/2))),  # estimate of accuracy of MSPEs (close to 1 = accurate)
                                                                   V3 = (mean((inc.data-mean.ci)^2,na.rm =TRUE)^(1/2)),
                                                                   PPL = sum((inc.data - mean.ci)^2, na.rm = TRUE) - sum(predvar, na.rm = TRUE)) # posterior predictive loss# goodness of fit estimate (small = better fit)
in.sample.validation.inc.metrics$validation <- "in-sample"
# concatenate together + add the model name + description:
in.sample.validation.inc.metrics$model <- output.base.name
in.sample.validation.inc.metrics$description <- "Model with SDI, no SI, random BetaX effects, plot random slopes"

write.csv(in.sample.validation.inc.metrics, paste0(output.base.name, "model.validation.stats.inc.csv"), row.names = FALSE)



