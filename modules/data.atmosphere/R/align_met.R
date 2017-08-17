##' Align meteorology datasets for debiasing
# -----------------------------------
# Description
# -----------------------------------
##'
##' @title align.met
##' @family debias - Debias & Align Meteorology Datasets into continuous time series
##' @author Christy Rollinson
##' @description This script aligns meteorology datasets in at temporal resolution for debiasing & 
##'              temporal downscaling.  
##'              Note: The output here is stored in memory!  
##'              Note: can probably at borrow from or adapt align_data.R in Benchmarking module, but 
##'              it's too much of a black box at the moment.
# -----------------------------------
# Parameters
# -----------------------------------
##' @param train.path - path to the dataset to be used to downscale the data
##' @param source.path - data to be bias-corrected aligned with training data (from align.met)
##' @param pair.mems - logical stating whether ensemble members should be paired
##' @param n.ens  - number of ensemble members to generate and save
##' @param verbose
##' @export
# -----------------------------------
# Workflow
# -----------------------------------
# 1. Determine if training data is a single series or ensemble
#    - note: assumes training data is at the temporal resolution you want to work with
# 2. If not already, coerce training data into the size of the desired output ensemble
# 3. Aggregate or coerce source data into temporal resolution of training data
#    - note: source data should remain a single time series
# 4. export data (stored in memory) for input into the debiasing or temporal downscaling workflow
# -----------------------------------

#----------------------------------------------------------------------
# Begin Function
#----------------------------------------------------------------------
align.met <- function(train.path, source.path, pair.mems = TRUE, verbose = FALSE) {
  # 
}
  