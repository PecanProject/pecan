#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Converts a met CF file to a model specific met file. The input
##' files are calld <in.path>/<in.prefix>.YYYY.cf
##'
##' @name met2model.BASGRA
##' @title Write BASGRA met files
##' @param in.path path on disk where CF file lives
##' @param in.prefix prefix for each file
##' @param outfolder location where model specific output is written.
##' @param start_date beginning of the weather data
##' @param end_date end of the weather data
##' @return OK if everything was succesful.
##' @export
##' @author Istem Fer
##-------------------------------------------------------------------------------------------------#
met2model.BASGRA <- function(in.path, in.prefix, outfolder, overwrite = FALSE, 
                             start_date, end_date, ...) {

  
} # met2model.BASGRA
