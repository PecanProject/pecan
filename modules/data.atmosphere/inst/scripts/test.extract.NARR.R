rm(list = setdiff(ls(), lsf.str()))
source('~/pecan/utils/R/convert.input.R')
source('~/pecan/db/R/dbfiles.R')

newsite <- 340

source('~/pecan/modules/data.atmosphere/inst/scripts/met.workflow.params.NARR.R')

source("~/pecan/modules/data.atmosphere/inst/scripts/met.workflow.NARR.R")
