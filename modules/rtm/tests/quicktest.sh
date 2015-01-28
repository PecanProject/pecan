#!/bin/bash

## Script for test run of PROSPECT model.
## Arguments to "runpi.R" are as follows:
##    [1] Starting SD of Jump distribution; larger values indicate larger jumps
##    [2] Species; must EXACTLY match species tag in spectra filenames
##    [3] Whether to use a single precison value for all wavelengths (1) or an individual precision at each wavelength (0)
##    [4] Random effects; current options are 'none' and 'leaf'
##    [5] How to generate initial conditions; OPTIONS: 'random', 'mle', 'guess'
##    [6] Number of MCMC steps.
##    [7] Filename tag to identify run results
##    [8] Sub-folder in "run_results" for storing output.

touch ../run_results/testfolder/empty
rm ../run_results/testfolder/*
cd ../R
Rscript runpi.R 0.05 FFT.ACRU 1 leaf_FFT random 50 test1 testfolder
