## Carl Davidson code for dealing with flux data for emulator-based DA
## ported by M. Dietze 08/30/12
## some of this is redundant with other parts of PEcAn and needs to be cleaned up

#library(hdf5)
#source('./code/R/edview.base.R')
#source('./code/R/utils.R')
#source('./code/R/model.specific.R')

dlaplace <- function(x, mean, shape, ...) {
  stats::dexp(abs(mean - x), shape, ...)
} # dlaplace


# LIKELIHOOD
calculate.nee.L <- function(yeardoytime, model.i.nee, observed.flux, be, bu) {
  model.flux <- data.frame(yeardoytime = yeardoytime[seq(model.i.nee)],
                           model.i.nee = model.i.nee)
  all.fluxes <- merge(observed.flux, model.flux, by = "yeardoytime")

  sigma <- with(all.fluxes, stats::coef(stats::lm(abs(model.i.nee - FC) ~ abs(model.i.nee))))

  ## calculate likelihood
  logL <- rep(NA, sum(all.fluxes$model.i.nee != 0))
  emissions <- which(all.fluxes$model.i.nee < 0)
  uptake <- which(all.fluxes$model.i.nee > 0)

  ## are these calculations correct, with respect to slope and intercepts?
  logL[emissions] <- with(all.fluxes[emissions, ],
                          dlaplace(FC, model.i.nee, 1 / (be[1] + be[2] * abs(model.i.nee)), log = TRUE))
  logL[uptake] <- with(all.fluxes[uptake, ],
                       dlaplace(FC, model.i.nee, 1/(bu[1] + bu[2] * abs(model.i.nee)), log = TRUE))

  # NEE.acf <- stats::acf(all.fluxes$model.i.nee, 100, plot=FALSE)
  ar.coef <- stats::ar(model.i.nee, FALSE, 1)$ar
  weight <- (1 - ar.coef) / (1 + ar.coef)
  print(weight)
  nlogL <- -sum(logL, na.rm = TRUE) * weight
  return(print(nlogL))
} # calculate.nee.L


get.da.data <- function(out.dir, ameriflux.dir, years, be, bu, ensemble.size = 199) {
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

  # OBSERVED
  observed <- lapply(years, function(year) {
    PEcAn.uncertainty::read.ameriflux.L2(paste(ameriflux.dir, year, "L2.csv", sep = "_"), year)
  })
  observed <- do.call(rbind, observed)
  observed$yeardoytime <- observed$time

  # filter out winter observations March 1 and November 1 are chosen based on Yoshi's methods
  observed <- observed[observed$DTIME > 60 & observed$DTIME < 305, ]
  stopifnot(all(abs(observed$FC) <= 500, na.rm = TRUE))

  # ENSEMBLE
  ensemble.run.ids <- PEcAn.utils::get.run.id("ENS", PEcAn.utils::left.pad.zeros(1:ensemble.size))
  ensemble.x <- do.call(cbind, ensemble.samples[pfts])[1:ensemble.size, ]

  # SENSITIVITY ANALYSIS
  p.rng <- do.call(rbind, lapply(pfts, function(pft) {
    t(sa.samples[[pft]][c(1, nrow(sa.samples[[pft]])), ])
  }))
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

  points.per.day <- 48
  dtime <- do.call(c, lapply(years,
          function(year) {
            nodays <- PEcAn.utils::days_in_year(year)
            year + seq(1, nodays, by = 1 / points.per.day)[-1] / nodays
          }))

  # run.ids<-ensemble.run.ids
  # x <- ensemble.x
  y <- t(as.data.frame(lapply(run.ids, function(run.id) {

    outname <- paste0(run.id, "-T-(", paste(paste("(", years, ")", sep = ""), collapse = "|"), ")")
    data <- read.output.type(out.dir, outname = outname, pattern = "-T-")
    data <- data$AVG_GPP - data$AVG_PLANT_RESP - data$AVG_HTROPH_RESP
    calculate.nee.L(dtime, data,
                    observed[c("yeardoytime", "FC")],
                    be, bu)
  })))

  save(x, y, file = paste(out.dir, "L.nee.Rdata", sep = ""))
  print("save sucessful")
  warnings()
} # get.da.data


#get.da.data('./pecan/BarrowDA5param/', 'barrow/validation/usakbarr', years=1998:2006,
#    be=c(0.20,  0.04), bu=c(0.31, -0.05))
#get.da.data('./pecan/AtqasukDA5param/', 'atqasuk/validation/usatqasu', years=2000:2006,
#    be=c(0.75,  0.23), bu=c(1.08, -0.21))
