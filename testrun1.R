## Test run of Bayesian inversion of PROSPECT

source("prospect_bayesinv.R")
source("specdataproc.r")
specdat <- load.all.spec()
jrsd <- 5e-4

## Testrun for Grape
grape.cond <- expression(Species == "Grape" & Spectra_Type == "Refl" & Wavelength >= 400)
grapemat <- specmatrix(specdat, grape.cond)
grape.test <- pinvbayes(grapemat, ngibbs=10^6, JumpRSD=jrsd)

## Testrun for Pepper
pepper.cond <- expression(Species == "CAAN-Red Pepper" & Spectra_Type == "Refl" &
                            Wavelength >= 400)
peppermat <- specmatrix(specdat, pepper.cond)
pepper.test <- pinvbayes(peppermat, ngibbs=10^6, JumpRSD=jrsd)

## Testrun for Lemon
lemon.cond <- expression(Species == "LemonTree" & Spectra_Type == "Refl" &
                            Wavelength >= 400)
lemonmat <- specmatrix(specdat, lemon.cond)
lemon.test <- pinvbayes(lemonmat, ngibbs=10^6, JumpRSD=jrsd)

## Testrun for Orange
orange.cond <- expression(Species == "MandarinOrange" & Spectra_Type == "Refl" &
                            Wavelength >= 400)
orangemat <- specmatrix(specdat, orange.cond)
orange.test <- pinvbayes(orangemat, ngibbs=10^6, JumpRSD=jrsd)

## Testrun for Palm
palm.cond <- expression(Species == "ShortDatePalm" & Spectra_Type == "Refl" &
                            Wavelength >= 400)
palmmat <- specmatrix(specdat, palm.cond)
palm.test <- pinvbayes(palmmat, ngibbs=10^6)

