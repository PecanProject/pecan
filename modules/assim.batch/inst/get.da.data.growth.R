## Carl Davidson code for dealing with height growth data for emulator-based DA
## ported by M. Dietze 08/30/12
## some of this is redundant with other parts of PEcAn and needs to be cleaned up 

#library(hdf5)
#source('./code/R/edview.base.R')
#source('./code/R/utils.R')
#source('./code/R/model.specific.R')

get.height <- function(growth, monthly, j, year.i0) {
  monthly$HITE[year.i0][growth$plot[j] == monthly$PATCH_NAME[year.i0] & 
                          growth$individual[j] == monthly$COHORT_NAME[year.i0] & 
                          monthly$HITE[year.i0] > 0.01]
  # ignore new cohorts by matching against hgt_min
} # get.height

# LIKELIHOOD
calculate.growth.L <- function(yearly, growth, error, years) {
  
  year.names <- as.character(years)
  year.ranges <- c(0, cumsum(yearly$NCOHORTS_GLOBAL))
  years.i <- lapply(seq(years), function(i) {
    year.ranges[i]:year.ranges[i + 1]
  })
  names(years.i) <- year.names
  
  cumLogL <- 0
  squares <- c()
  
  for (i in seq(years)[-1]) {
    year <- year.names[i]
    year.i1 <- years.i[[i]]
    year.i0 <- years.i[[i - 1]]
    for (j in seq_len(nrow(growth))) {
      observed.growth <- growth[j, year]
      # print(observed.growth)
      if (!is.na(observed.growth)) {
        height1 <- get.height(growth, yearly, j, year.i1)[1]
        height0 <- get.height(growth, yearly, j, year.i0)[1]
        if (length(height0) != length(height1)) {
          warnings("0 growth")
        }
        modeled.growth <- height1 - height0
        
        if (length(modeled.growth) > 0 && !is.na(modeled.growth)) {
          # print(growth$pft[j]) print(paste('diff ', (modeled.growth - observed.growth)))
          if (any(modeled.growth < 0)) {
            warnings("negative growth")
          }
          # squares <- c(squares, (modeled.growth - observed.growth)^2)
          logL <- stats::dnorm((observed.growth), (modeled.growth), error, log = TRUE)
          if (any(is.infinite(logL))) {
            stop("Infinite likelihood")  #AKA really large value
          }
          # else print(paste(growth$pft[j], 'logL', logL))
          cumLogL <- cumLogL - sum(logL, na.rm = TRUE)
          if (is.infinite(cumLogL)) {
            stop("Infinite likelihood")
          }
        }
      }
    }
  }
  # return(squares)
  print(paste("cumLogL", cumLogL))
  return(cumLogL)
} # calculate.growth.L

get.da.data.growth <- function() {
  
  out.dir <- "./pecan/Toolik/growth/"
  
  buds <- utils::read.csv("./toolik/validation/Survey/ToolikVegSurvey.csv", sep = "\t")
  buds <- buds[!is.na(buds$length), ]
  buds <- buds[buds$pft != "graminoid", ]
  heights <- buds[, c("length", paste0("X", 2010:2003))]
  heights <- as.matrix(heights[!is.na(heights$length), ]) / 1000
  growth <- do.call(rbind, lapply(1:nrow(heights), function(i) - (diff(as.numeric(heights[i, ])))))
  colnames(growth) <- 2011:2004
  growth <- cbind(buds[, c("plot", "individual", "pft")], growth)
  
  ensemble.size <- 500
  samples.file <- paste(out.dir, "samples.Rdata" , sep = "")
  
  if(file.exists(samples.file)) {
    samples <- new.env()
    load(samples.file, envir = samples)
    ensemble.samples <- samples$ensemble.samples
    sa.samples <- samples$sa.samples
  } else {
    PEcAn.logger::logger.error(samples.file, "not found, this file is required by the get.da.data function")
  }
  
  pfts <- names(ensemble.samples)
  pfts <- pfts[pfts != "env"]
  
  # ENSEMBLE
  omitted <- c(87)
  ensemble.run.ids <- PEcAn.utils::get.run.id("ENS", PEcAn.utils::left.pad.zeros((1:ensemble.size)[-omitted]))
  ensemble.x <- do.call(cbind, ensemble.samples[pfts])[(1:ensemble.size)[-omitted], ]
  
  # SENSITIVITY ANALYSIS
  sa.x <- list()
  for (pft in pfts) {
    MEDIAN <- "50"
    
    median.samples <- list()
    for (i in seq_along(sa.samples)) {
      median.samples[[i]] <- sa.samples[[i]][MEDIAN, ]
    }
    names(median.samples) <- names(sa.samples)
    run.id <- PEcAn.utils::get.run.id("SA", "median")
    sa.x[[run.id]] <- do.call(cbind, trait.samples)
    ## loop over pfts
    for (i in seq(names(sa.samples))) {
      
      traits <- colnames(sa.samples[[i]])
      quantiles.str <- rownames(sa.samples[[i]])
      
      ## loop over variables
      for (trait in traits) {
        for (quantile.str in quantiles.str) {
          if (quantile.str != MEDIAN) {
            quantile <- as.numeric(quantile.str) / 100
            trait.samples <- median.samples
            trait.samples[[i]][trait] <- sa.samples[[i]][quantile.str, trait]
            run.id <- PEcAn.utils::get.run.id("SA", round(quantile, 3),
                                 trait = trait, 
                                 pft.name = names(trait.samples)[i])
            sa.x[[run.id]] <- do.call(cbind, trait.samples)
          }
        }
      }
    }
  }
  sa.x <- do.call(rbind, sa.x)
  sa.run.ids <- rownames(sa.x)
  run.ids <- ensemble.run.ids  # c(ensemble.run.ids, sa.run.ids)
  x <- ensemble.x  # rbind(ensemble.x, sa.x)
  
  # run.ids<-ensemble.run.ids 
  # x <- ensemble.x
  y <- t(as.data.frame(lapply(run.ids, function(run.id) {
    print(run.id)
    yearly <- paste0(run.id, "-E-(((", 
                     paste(paste0("(", 2003:2010, ")"), collapse = "|"), 
                     ")-01)|(2011-07))")
    yearly <- read.output.type(out.dir, outname = yearly, pattern = "-E-")
    if (length(yearly) <= 0) {
      return(NA)
    }
    calculate.growth.L(yearly, growth, 0.023, years = 2003:2011)
    # error param was calculated from stddev, see below
  })))
  "squares<-lapply(run.ids, 
      function(run.id){ 
        print(run.id)
        yearly <-paste(run.id, '-E-(((', 
            paste(paste('(', 2003:2010, ')', sep=''), collapse='|'), ')-01)|(2011-07))', 
            sep='')
        yearly <- read.output.type(out.dir,
            outname=yearly, pattern='-E-')
        if(length(yearly) <= 0){
          return(NA)
        }
        squares <- calculate.growth.L(yearly, growth, 0.0278, years=2003:2011)
        #observation process 
        return(squares)
      })"
  # stddev <- sqrt(mean(unlist(squares))) print(stddev)
  
  # filter out runs that have not completed log likelihoods default to 0
  x <- x[y != 0, ]
  y <- y[y != 0]
  print(y)
  save(x, y, file = paste(out.dir, "L.nee.Rdata", sep = ""))
  print("save sucessful")
  warnings()
} # get.da.data.growth
