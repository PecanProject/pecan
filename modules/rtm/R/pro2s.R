#' Coupled PROSPECT-Two-stream model
#' 
#' @param param Model parameters, in the following order: N, Cab, (Car, 
#' Cbrown), Cw, Cm, solar zenith angle, LAI, soil_moisture
#' @param prospect.version Version of PROSPECT to use (4, 5, or '5B'; default=5)
#' @return Spectra matrix (see [spectra()]) for wavelengths 400 to 2500nm containing the following columns:
#'    * `alpha.c` -- Direct ("collimated") reflectance
#'    * `alpha.i` -- Diffuse ("isotropic") reflectance
#'    * `Tc` -- Direct transmittance to background
#'    * `Ti` -- Diffuse reflectance to background
#'    * `Ac` -- Direct absorbance by canopy
#'    * `Ai` -- Diffuse absorbance by canopy
#' @export
pro2s <- function(param, prospect.version = 5) {
  prospect.version <- toupper(as.character(prospect.version))
  plist <- as.list(param)
  if (prospect.version == "4") {
    if (length(plist) != 7) {
      stop("Wrong number of parameters")
    }
    modname <- "pro42s"
  } else if (prospect.version == "5") {
    if (length(plist) != 8) {
      stop("Wrong number of parameters")
    }
    modname <- "pro52s"
  } else if (prospect.version == "5B") {
    if (length(plist) != 9) {
      stop("Wrong number of parameters")
    }
    modname <- "pro5b2s"
  } else {
    stop("prospect.version must be 4, 5, or 5B")
  }
  nw <- 2101  # Length of PROSPECT output vector (400-2500nm, in 1 nm increments)
  out.names     <- c("alpha.c", "alpha.i", "Tc", "Ti", "Ac", "Ai")
  plist$alpha.c <- numeric(nw)
  plist$alpha.i <- numeric(nw)
  plist$Tc <- numeric(nw)
  plist$Ti <- numeric(nw)
  plist$Ac <- numeric(nw)
  plist$Ai <- numeric(nw)
  inlist   <- c(modname, plist, PACKAGE="PEcAnRTM")
  outlist  <- do.call(.Fortran, inlist)
  out.mat  <- do.call(cbind, outlist[out.names])
  spectra(out.mat, 400:2500)
} # pro2s
