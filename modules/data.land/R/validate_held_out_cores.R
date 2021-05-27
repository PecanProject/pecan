# plot the predicted vs held out tree ring increments:

validation.cores # has the index for the plots
cores.valid <- read.csv("INV_FIA_DATA/data/validation_cores_SDI_plot.csv")

# read in the RWL files

rwls <- readRDS(gzcon(url("https://de.cyverse.org/dl/d/0A6040DD-4CC0-4F4C-9351-CF922985B1EE/validation.rwl.year.df.rds")))
cores.valid$FILE2 <- paste0(cores.valid$FILE,"-A.rwl") # append to match rwl filenames

unique(rwls$FILE) %in% cores.valid$FILE2

# read in the posterior predictions for increment for all the cores

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
file.base.name <- "Full.model.validation."
output.base.name <- "Full.model.validation"
stage2 <- TRUE
workingdir <- "/home/rstudio/"
#workingdir <- "/Users/kah/Documents/docker_pecan/pecan"
climate <- "wintP.wateryr"
cov.data = cov.data


jags.comb <- NULL

for(i in 390:400){ # note this model stopped early b/c convergence
  load(paste0(workingdir,"IGF_PIPO_AZ_mcmc/", file.base.name,i,".RData"))
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

model.out <- jags.comb
### DBH par(mfrow=c(3,2))
layout(matrix(1:8, 4, 2, byrow = TRUE))
out      <- as.matrix(model.out) ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
x.cols   <- which(substr(colnames(out), 1, 1) == "x") # grab the state variable columns
ci       <- apply(out[, x.cols], 2, quantile, c(0.025, 0.5, 0.975))
ci.names <- parse.MatrixNames(colnames(ci), numeric = TRUE)
out.sample.rw.obs <- list()
smp <- sample.int(data$ni, min(8, data$ni)) # select a random sample of 8 trees to plot
smp <- 1:data$ni
for (i in smp) {
  sel <- which(ci.names$row == i)
  rng <- c(range(ci[, sel], na.rm = TRUE), range(data$z[i, ], na.rm = TRUE))
  
  # plot(data$time, ci[2, sel], type = "n", 
  #      ylim = range(rng), ylab = "DBH (cm)", main = i)
  # ciEnvelope(data$time, ci[1, sel], ci[3, sel], col = "lightBlue")
  # points(data$time, data$z[i, ], pch = "+", cex = 1.5)
  # lines(data$time,z0[i,],lty=2)
  
  ## growth
  sel      <- which(ci.names$row == i)
  inc.mcmc <- apply(out[, x.cols[sel]], 1, diff)
  inc.ci   <- apply(inc.mcmc, 1, quantile, c(0.025, 0.5, 0.975)) * 5
  # inc.names = parse.MatrixNames(colnames(ci),numeric=TRUE)
  
  plot(data$time[-1], inc.ci[2, ], type = "n", 
       ylim = range(inc.ci, na.rm = TRUE), ylab = "Increment (mm)")
  ciEnvelope(data$time[-1], inc.ci[1, ], inc.ci[3, ], col = "lightBlue")
  points(data$time, data$y[i, ]*5 , pch = "+", cex = 1.5, type = "b", lty = 2)
  
  
  
  out.sample.rw.obs[[i]] <- data.frame(
    year = 1967:2010, 
    
    min.ci = inc.ci[1,],
    mean.ci = inc.ci[2,],
    max.ci = inc.ci[3,])
  
}
out.sample.rw.obs[[835]]
# subset by out of sample predictions that we have in the downloaded data
validation.w.files$FILE2 <- paste0(validation.w.files$FILE, "-A.rwl")
rwl.index<- as.numeric(row.names(validation.w.files[validation.w.files$FILE2 %in% unique(rwls$FILE) ,]))
validation.w.files$data.index <- 516:850
files.to.validate <- validation.w.files[validation.w.files$FILE2 %in% unique(rwls$FILE) ,]
length(rwl.index)

#files.to.validate$data.index <- rwl.index+ 515


out.rw.preds <- do.call(rbind, out.sample.rw.obs)
out.rw.preds$index <- rep(1:850, sapply(out.sample.rw.obs , nrow)) # add the tellervo file name


pred.index <- rwl.index + 515
# filter the out.rw.preds by the rwl indices:
out.sample.rw.df <- out.rw.preds %>% filter(index %in% pred.index)
files.to.validate
crosswalk <- unique(files.to.validate[,c("FILE2", "data.index")])
colnames(crosswalk) <- c("FILE", "index")
out.df<- left_join(out.sample.rw.df, crosswalk)
p.o.df <- merge(out.df,rwls, by = c("year", "FILE") )

# predicted vs observed overall
ggplot(p.o.df, aes(rwl, mean.ci))+geom_point()

summary.stats <- summary(lm(p.o.df$rwl ~ p.o.df$mean.ci))

p.o.out.of.sample <- ggplot()+
  geom_errorbar(data = p.o.df, aes(rwl, ymin = min.ci, ymax = max.ci), color = "grey")+
  geom_point(data = p.o.df, aes(rwl, mean.ci), size = 0.75)+
  geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+
  ylab("Predicted increment growth (mm)")+xlab("Measured Increment Growth (held-out samples)")+
  theme_bw(base_size = 12)+theme(panel.grid = element_blank())+ylim(0, 6)+xlim(0, 6) +
  geom_text(data=data.frame(summary.stats$r.squared), aes( label = paste("R.sq: ", round(summary.stats$r.squared, digits = 3), sep="")),parse=T,x=0.5, y=5.5)



png(height = 4, width = 6, units = "in", res = 200, "predicted.observed.held.out.rw.png")
p.o.out.of.sample
dev.off()

png(height = 8, width = 10, units = "in", res = 300, "predicted.observed.held.out.rw_individual.png")

ggplot()+geom_ribbon(data = p.o.df, aes(x = year, ymin = min.ci, ymax = max.ci), fill = "lightblue")+
  geom_point(data =p.o.df, aes(x = year, y = rwl), size = 0.5)+geom_line(data =p.o.df, aes(x = year, y = rwl), linetype = "dashed")+facet_wrap(~index)
dev.off()
