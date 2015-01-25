##' @title Prospect model setup
##' 
##' @details {
##' Load PROSPECT C script and absorption features into environment
##' }
##' @author Alexey Shiklomanov
##' @export

setwd("R/")
library(Rcpp)
sourceCpp("prospect_c.cpp")

load("../data/dataSpec_p4.RData")    
dataSpec_p4 <- as.matrix(dataSpec_p4)

guess.inits <- c(N=1.4, 
                 Cab=30,
                 Cw=0.017,
                 Cm=0.006
)

n.a <- dataSpec_p4[,"refractive_index"]                    # Column 2
cab.a <- dataSpec_p4[,"specific_abs_coeff_chl"]    # Column 3
w.a <- dataSpec_p4[,"specific_abs_coeff_cw"]        # Column 5
m.a <- dataSpec_p4[,"specific_abs_coeff_cm"]        # Column 6

prospect <- function(N, Cab, Cw, Cm) prospect4(N, Cab, Cw, Cm, n.a, cab.a, w.a, m.a)
