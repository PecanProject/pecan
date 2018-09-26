library(PEcAnRTM)

# For developing, comment out `library` and use this:
library(devtools)
load_all('~/Projects/pecan/pecan/modules/rtm')

data_dir <- '~/Projects/nasa-rtm/edr-da/run-ed/1cohort/dbh20/early_hardwood'

paths <- list(ed2in = file.path(data_dir, 'ED2IN'),
              history = file.path(data_dir, 'outputs'))

output_dir <- tempdir()
file.copy(from = '~/Projects/ED2/EDR/build/ed_2.1-opt',
          to = file.path(output_dir, 'ed_2.1-opt'), 
          overwrite = TRUE)

# Case 1 -- Two PFTs: spectra and traits for both
spectra_list <- list(temperate.Early_Hardwood = prospect(c(1.4, 40, 0.01, 0.01), 4, TRUE),
                     temperate.Late_Hardwood = prospect(c(1.5, 25, 0.01, 0.012), 4, TRUE))

trait.values <- list(temperate.Early_Hardwood = list(orient_factor = 0.5),
                     temperate.Late_Hardwood = list(orient_factor = 0.5))

output <- EDR(paths = paths,
              spectra_list = spectra_list,
              par.wl = 400:800,
              nir.wl = 801:2500,
              datetime = ISOdate(2004, 07, 01, 16, 00, 00),
              trait.values = trait.values,
              output.path = output_dir)

# Case 2 -- Two PFTs: only spectra, no traits
spectra_list <- list(temperate.Early_Hardwood = prospect(c(1.4, 40, 0.01, 0.01), 4, TRUE),
                  temperate.Late_Hardwood = prospect(c(1.5, 25, 0.01, 0.012), 4, TRUE))

trait.values <- list(temperate.Early_Hardwood = list(),
                     temperate.Late_Hardwood = list())

output <- EDR(paths = paths,
              spectra_list = spectra_list,
              par.wl = 400:800,
              nir.wl = 801:2500,
              datetime = ISOdate(2004, 07, 01, 16, 00, 00),
              trait.values = trait.values,
              output.path = output_dir)
