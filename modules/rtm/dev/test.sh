#!/bin/bash
R CMD INSTALL ..
Rscript testinversion.R
evince Rplots.pdf &
