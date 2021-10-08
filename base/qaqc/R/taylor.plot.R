#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' Plot taylor diagram for benchmark sites
##'
##' @param dataset data to plot
##' @param runid a numeric vector with the id(s) of one or more runs (folder in runs) to plot
##' @param siteid vector of sites to plot
new.taylor <- function(dataset, runid, siteid) {
  for (run in runid) {
    for (si in siteid) {
      sitemask <- dataset$site %in% si
      obs <- dataset$obs[sitemask]
      mod <- dataset[sitemask, paste0("model", run)]
      R <- stats::cor(obs, mod, use = "pairwise")
      sd.f <- stats::sd(mod)
      lab  <- paste(paste0("model", run), paste0("site", si))
      if (run == runid[1] && si == siteid[1]) {
        plotrix::taylor.diagram(obs, mod, pos.cor = FALSE)
      } else {
        plotrix::taylor.diagram(obs, mod, pos.cor = FALSE, add = TRUE)
      }
      graphics::text(sd.f * R, sd.f * sin(acos(R)), labels = lab, pos = 3)
    }
  }
} # new.taylor
