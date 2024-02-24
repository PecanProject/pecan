#' PROSPECT (4, 5, or 5B) model
#' 
#' R wrapper for PROSPECT models
#' @author Alexey Shiklomanov
#' @param param Vector of PROSPECT parameter values:
#'     * N: Effective number of leaf layers (>1)
#'     * Cab: Leaf chlorophyll content (ug/cm2) (>0)
#'     * (5) Car: Leaf carotenoid content (ug/cm2) (>0)
#'     * (D) Canth: Leaf anthocyanin content (ug/cm2) (>0)
#'     * (5B, D) Cbrown: Leaf brown matter content (ug/cm2) (>0)
#'     * Cw: Leaf water content (cm) (>0)
#'     * Cm: Leaf dry matter content (ug/cm2) (>0)
#' @param version PROSPECT version: 4, 5, or '5B'
#' @return Object of class `spectra` (see [spectra()]) with simulated 
#' reflectance (column 1) and transmittance (column 2) from 400 to 2500 nm
#' @export
#' @useDynLib PEcAnRTM

prospect <- function(param, version) {
  version  <- toupper(as.character(version))
  plist    <- as.list(param)
  plist$RT <- matrix(0, 2101, 2)
  if (version == "4") {
    if (length(plist) != 5) {
      stop("Wrong number of parameters")
    }
    inlist <- c("prospect_4", plist)
  } else if (version == "5") {
    if (length(plist) != 6) {
      stop("Wrong number of parameters")
    }
    inlist <- c("prospect_5", plist)
  } else if (version == "5B") {
    if (length(plist) != 7) {
      stop("Wrong number of parameters")
    }
    inlist <- c("prospect_5b", plist)
  } else if (version == "D") {
    if (length(plist) != 8) {
      stop("Wrong number of parameters")
    }
    inlist <- c("prospect_d", plist)
  } else {
    stop("Version must be 4, 5, 5B, or D")
  }
  
  inlist <- c(inlist, PACKAGE="PEcAnRTM")
  outlist <- do.call(.Fortran, inlist)
  out <- spectra(outlist[[length(outlist)]], 400:2500)
  colnames(out) <- c("reflectance", "transmittance")
  out
} # prospect

#' Shortcut lists for PROSPECT parameter names

#' @name params.prospect4
#' @title PROSPECT 4 parameters
#' @export
params.prospect4 <- c("N", "Cab", "Cw", "Cm")

#' @name params.prospect5
#' @title PROSPECT 5 parameters
#' @export
params.prospect5 <- c("N", "Cab", "Car", "Cw", "Cm")

#' @name params.prospect5b
#' @title PROSPECT 5B parameters
#' @export
params.prospect5b <- c("N", "Cab", "Car", "Cbrown", "Cw", "Cm")

#' @name params.prospectd
#' @title PROSPECT D parameters
#' @export
params.prospectd <- c("N", "Cab", "Car", "Canth", "Cbrown", "Cw", "Cm")

#' Default settings for PROSPECT inversion
#' @name default.settings.prospect
#' @title Defult inversion settings for PROSPECT 5 models
#' @export
default.settings.prospect <- list(
  model = function(params, seed=NULL) prospect(params, 5)[,1],
  inits.function = function() 
      with(prior.defaultvals.prospect(sd.inflate=3), 
           rlnorm(5, mu, sigma) + c("N"=1,"Cab"=0,"Car"=0,"Cw"=0,"Cm"=0)),
  prior.function = with(prior.defaultvals.prospect(sd.inflate=3), priorfunc.prospect(mu,sigma)),
  param.mins = c(1, 0, 0, 0, 0),
  param.maxs = c(Inf, Inf, Inf, Inf, Inf),
  ngibbs = 10000,
  nchains = 5,
  burnin = 8000,
  ngibbs.max = 1e7,
  ngibbs.min = 5000,
  ngibbs.step = 1000,
  return.samples = TRUE,
  target = 0.234,
  do.lsq = FALSE,
  save.samples = NULL,
  quiet = FALSE,
  adapt = 100,
  adj_min = 0.1)
