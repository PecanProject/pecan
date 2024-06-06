## Carl Davidson's code for plotting results from emulator-based DA
## ported by M. Dietze 08/30/12
## some of this is redundant with other parts of PEcAn and needs to be cleaned up 

plot_da <- function(prior.dir, prior.file, in.dir, out.dir, next.run.dir) {

  # source('code/R/approx.posterior.R') source('code/R/utils.R')

  # prior.dir <- './pecan/Toolik/growth/' prior.file<-'/post.distns.Rdata' in.dir <-
  # './pecan/Toolik/growth/' out.dir <- './pecan/Toolik/growth/' next.run.dir <-
  # './pecan/Toolik/growth/'
  
  # prior.dir <- './pecan/Toolik/growth/' prior.file<-'/da.post.distns.Rdata' in.dir <-
  # './pecan/BarrowDA5param/' out.dir <- './pecan/BarrowDA5param/' next.run.dir <-
  # './pecan/AnaktuvukControl/'
  
  prior.dir    <- "./pecan/BarrowDA5param/"
  prior.file   <- "/da.post.distns.Rdata"
  in.dir       <- "./pecan/AtqasukDA5param/"
  out.dir      <- "./pecan/AtqasukDA5param/"
  next.run.dir <- "./pecan/AnaktuvukControl/"
  
  num.run.ids <- 5  #commandArgs(trailingOnly = TRUE)
  print(num.run.ids)
  
  samples.file <- paste(in.dir, "samples.Rdata", sep = "")
  L.nee.file <- paste(in.dir, "L.nee.Rdata", sep = "")
  
  if(file.exists(samples.file)) {
    samples <- new.env()
    load(samples.file, envir = "samples")
    ensemble.samples <- samples$ensemble.samples
    sa.samples <- samples$sa.samples
  } else {
    PEcAn.logger::logger.error(samples.file, "not found, this file is required by the plot_da function")
  }
  
  if(file.exists(L.nee.file)) {
    L.nee <- new.env()
    load(L.nee.file, envir = "L.nee")
    x <- L.nee$x
    y <- L.nee$y
  } else {
    PEcAn.logger::logger.error(L.nee.file, "not found, this file is required by the plot_da function")
  }

  prior.x <- x
  prior.y <- y
  
  ddist <- function(x, prior) {
    if (prior$distn == "exp") {
      return(stats::dexp(x, prior$parama))
    }
    eval(parse(text = paste("d", prior$distn, sep = "")))(x, prior$parama, prior$paramb)
  }
  rdist <- function(x, prior) {
    if (prior$distn == "exp") {
      return(stats::rexp(x, prior$parama))
    }
    eval(parse(text = paste("r", prior$distn, sep = "")))(x, prior$parama, prior$paramb)
  }
  pfts <- names(ensemble.samples)
  pfts <- pfts[pfts != "env"]

  grDevices::pdf(paste(out.dir, "/da.plots.pdf", sep = ""), height = 8, width = 11)

  # ORIGINAL PRIORS
  priors <- do.call(rbind, lapply(pfts, function(pft) {
    traits <- names(ensemble.samples[[pft]])
    load(paste("./pecan/Toolik/growth/", pft, "/post.distns.Rdata", sep = ""))
    return(post.distns[traits, ])
  }))
  traits <- rownames(priors)
  
  priors0 <- do.call(rbind, lapply(pfts, function(pft) {
    traits <- names(ensemble.samples[[pft]])
    load(paste("./pecan/Toolik/growth/", pft, "/da.post.distns.Rdata", sep = ""))
    return(post.distns[traits, ])
  }))
  traits <- rownames(priors)
  
  traits <- rownames(priors)
  
  # IMMEDIATE PRIORS
  priors2 <- do.call(rbind, lapply(pfts, function(pft) {
    traits <- names(ensemble.samples[[pft]])
    load(paste(prior.dir, pft, "/", prior.file, sep = ""))
    return(post.distns[traits, ])
  }))
  
  p.rng <- do.call(rbind, lapply(pfts, function(pft) {
    t(sa.samples[[pft]][c(1, nrow(sa.samples[[pft]])), ])
  }))
  
  # PLOT LIKELIHOODS
  graphics::par(mfrow = c(3, 5))
  good.runs <- y < stats::quantile(y, 0.95)
  print(nrow(x))
  print(length(good.runs))
  for (i in seq_along(x)) {
    graphics::plot(x[good.runs, i], y[good.runs],
         main = traits[i]$figid,
         xlim = p.rng[i, ],
         xlab = traits[i]$units,
         ylab = "-log(likelihood)",
         pch = 1)
    graphics::points(prior.x[, i], prior.y, col = "grey")
  }

  samp <- lapply(seq(num.run.ids), function(run.id) {
    print(paste0(in.dir, "./mcmc", run.id, ".Rdata"))
    run.id.file <- paste0(in.dir, "./mcmc", run.id, ".Rdata")
    
    if(file.exists(run.id.file)) {
      run.env <- new.env()
      load(run.id.file, envir = "run.env")
      m <- run.env$m
    } else {
      PEcAn.logger::logger.error(run.id.file, "not found, this file is required by the plot_da function")
    }
    
    return(m)
  })
  
  samp <- unlist(samp, recursive = FALSE)
  nmcmc <- nrow(samp[[1]])
  print(nmcmc)
  thin <- seq(500, nmcmc, by = 7)
  graphics::par(mfrow = c(2, 3))
  for (i in seq_along(samp[[1]])) {
    all <- do.call(rbind, lapply(samp, function(chain) chain[thin, i]))
    
    # MCMC chain
    graphics::plot(c(), ylim = range(all, na.rm = TRUE), xlim = c(1, length(thin)), ylab = "", type = "l")
    for (chain in seq(samp)) {
      graphics::lines(samp[[chain]][thin, i], col = chain)
    }
    
    # Autocorrelation plots
    samp.mcmc <- coda::as.mcmc.list(lapply(samp, function(chain) coda::as.mcmc(chain[thin, i])))
    coda::gelman.plot(samp.mcmc, auto.layout = FALSE, ylab = "")
    coda::autocorr.plot(samp.mcmc[[1]], auto.layout = FALSE)
  }
  
  graphics::par(mfrow = c(3, 5))
  for (i in seq_along(samp[[1]])) {
    all <- do.call(rbind, lapply(samp, function(chain) chain[thin, i]))
    
    # Density plots
    graphics::plot(stats::density(all),
         xlim = p.rng[i, ], 
         main = paste(traits[i]$figid),
         type = "l", 
         ylab = "", 
         xlab = traits[i]$units)
    x <- seq(p.rng[i, 1], p.rng[i, 2], length = 1000)
    graphics::lines(x, ddist(x, priors[traits[i]$id, ]), col = "grey")
    graphics::lines(x, ddist(x, priors2[traits[i]$id, ]), col = "grey", lty = 2)
  }
  
  # Now approximate posteriors to data assimilation and store them with posteriors from meta
  # analysis
  da.post.i <- 1
  # MOST SENSITIVE, BY HEIGHT dummy parameters with no sensitivity
  da.traits.hite <- list(tundra.grass = c("plant_min_temp", "hgt_min", "seed_rain"), 
                         tundra.evergreen = c("b1Bs", "SLA", "b1Bl", "seedling_mortality", "Vcmax"),
                         tundra.deciduous = c("b1Ht", "b2Ht", "r_fract", "b1Bs", "growth_resp_factor"))
  # MOST SENSITIVE, BY NEE
  da.traits <- list(tundra.grass = c("seedling_mortality", "f_labile", "root_turnover_rate", "Vcmax", "leaf_width"),
                    tundra.evergreen = c("b1Bs", "growth_resp_factor", "r_fract", "Vcmax", "b1Bl"), 
                    tundra.deciduous = c("r_fract", "f_labile", "growth_resp_factor", "b1Bl", "stomatal_slope"))
  # MOST SENSITIVE, BY NEE
  da.traits.shared <- list(tundra.grass = c("root_turnover_rate", "seed_rain", "hgt_min", "seedling_mortality", 
                                            "growth_resp_factor", "Vcmax", "SLA", "f_labile"), 
                           tundra.evergreen = c("growth_resp_factor", "b1Bs", "b1Bl", "b2Ht", "b1Ht", "SLA", "Vcmax", "r_fract"), 
                           tundra.deciduous = c("b1Ht", "b1Bs", "b2Ht", "growth_resp_factor", "Vcmax", "stomatal_slope", "r_fract", "root_turnover_rate"))
  
  cv <- function(foo) stats::sd(foo)/mean(foo)
  foo <- matrix(NA, nrow(priors), 8)
  for (pft in pfts) {
    print(pft)
    load(paste(prior.dir, "/", pft, "/", prior.file, sep = ""))
    for (i in which(rownames(post.distns) %in% da.traits[[pft]])) {
      samp.i <- list()
      samp.i[[rownames(post.distns)[[i]]]] <- 
        unlist(lapply(samp, function(chain) chain[thin, da.post.i]))
      
      print(rownames(post.distns)[[i]])
      cv.prior1 <- cv(rdist(1e+06, priors2[da.post.i, ]))
      cv.prior2 <- cv(rdist(1e+06, post.distns[i, ]))
      cv.post <- cv(samp.i[[rownames(post.distns)[[i]]]])
      foo[da.post.i, ] <- c(pft, rownames(post.distns)[[i]], 
                            cv.prior1, 
                            cv.prior2,
                            cv.post, 
                            (cv.prior1 - cv.prior2) / cv.prior1, 
                            (cv.prior2 - cv.post)/cv.prior2, 
                            (cv.prior1 - cv.post)/cv.prior1)
      post.distns[i, ] <- PEcAn.MA::approx.posterior(samp.i, post.distns[i, ])
      da.post.i <- da.post.i + 1
    }
    print(post.distns)
    save(post.distns, file = paste0(out.dir, pft, "/da.post.distns.Rdata"))
    prior.distns <- post.distns
    save(prior.distns, file = paste0(next.run.dir, pft, "/prior.distns.Rdata"))
  }
  foo <- as.data.frame(foo)
  names(foo) <- c("pft", "trait", "cv1", "cv2", "cv3", "reduction1", "reduction2", "reductiontot")
  
  graphics::par(mfrow = c(1, 1), cex = 0.5)
  # graphics::plot(foo[,6] ~ as.factor(rownames(priors)))
  
} # plot_da
