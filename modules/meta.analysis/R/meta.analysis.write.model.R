#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Convert template ma.model.template.R to a JAGS model.
##'
##'  Writes a meta-analysis model based on available data and prior specification. Adapted from \code{\link[R2WinBUGS]{write.model}}
##' @name write.ma.model
##' @title write.ma.model
##' @param modelfile model template file (ma.model.template.R)
##' @param outfile file name of model created 
##' @param reg.model structure of regression model
##' @param pr.dist A string representing the root distribution name used by R, e.g. 'norm', 'lnorm', 'gamma', 'beta', etc.
##' @param pr.param.a first parameter value accepted by \code{pr.dist}
##' @param pr.param.b second parameter value accepted by \code{pr.dist}
##' @param n number of observations in data
##' @param trt.n number of distinct treatments in data
##' @param site.n number of distinct sites in data
##' @param ghs.n = 1 if only non-greenhouse or greenhouse studies included, 2 if both
##' @param tauA parameter a for gamma prior on precision 
##' @param tauB parameter b for gamma prior on precision
##' @return Nothing, but as a side effect, the model is written
##' @author David LeBauer and Mike Dietze, based on original work on the \code{write.model} function in the \code{R2WinBUGS} package by Jouni Kerman and Uwe Ligges.
write.ma.model <- function (modelfile, outfile, reg.model, pr.dist, pr.param.a, pr.param.b, n, trt.n, site.n, ghs.n, tauA, tauB) {
  model.text <- scan(file=modelfile, what="character",sep="@")
  ## chose an uncommon separator in order to capture whole lines
  model.text <- gsub("%_%", "", model.text)
  model.text <- gsub("REGMODEL", reg.model, model.text)
  model.text <- gsub("PRIORDIST", paste("d", pr.dist,sep=""), model.text)
  model.text <- gsub("PRIORPARAMA", pr.param.a, model.text)
  model.text <- gsub("PRIORPARAMB", pr.param.b, model.text)
  model.text <- gsub("LENGTHK", n, model.text)
  model.text <- gsub("LENGTHJ", trt.n, model.text)
  model.text <- gsub("LENGTHG", site.n, model.text)
  model.text <- gsub("TAUA", format(signif(tauA,2), scientific=FALSE), model.text)
  model.text <- gsub("TAUB", format(signif(tauB,2), scientific=FALSE), model.text)
  if(ghs.n == 1)  model.text <- gsub("\\#GGG", '\\#', model.text)
  if(site.n == 1) model.text <- gsub("\\#SSS", '\\#', model.text)
  if(trt.n == 1)  model.text <- gsub("\\#TTT", '\\#', model.text)
  if(ghs.n > 1)   model.text <- gsub("\\#GGG", '', model.text)
  if(site.n > 1)  model.text <- gsub("\\#SSS", '', model.text)
  if(trt.n > 1)   model.text <- gsub("\\#TTT", '', model.text)
  if(pr.dist == 'beta' & pr.param.b < 1) {
    model.text <- gsub("\\#BBB", "T(,0.9999)", model.text)
  }
  writeLines(model.text, con = outfile)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.      				
####################################################################################################
