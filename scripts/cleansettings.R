#!/usr/bin/env Rscript

library(PEcAn.settings)

args <- commandArgs(trailingOnly = TRUE)
clean.settings(args[1], args[2])
