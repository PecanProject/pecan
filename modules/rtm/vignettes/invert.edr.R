library(PEcAnRTM)

# All inversion settings are defined in a list object, like in PEcAn. The 
# `default.settings.prospect` is a template that we then modify (rather than 
# building the entire list from scratch).
invert.options <- default.settings.prospect

# Define the model. The dimensions of the output of this function must match 
# the observations.
invert.options$model <- function(params) {

    ## `params` is a 1D vector, which is sampled as a block by the inversion 
    ## algorithm. This means that values in `params` need to be distributed 
    ## into the `prospect.params` object (for the PROSPECT call) and 
    ## `trait.values`, which is a list of lists (outer list = PFT, inner list = 
    ## trait).

    ## It helps for `params` to be a named vector. Those names are preserved by 
    ## the sampler.

    ## Any constants should be defined in here.

    # Set paths for ED executable and history files
    paths <- list(ed2in = "/path/to/ED2IN",  # Points directly at FILE
                  history = "/path/to/history/")  # Points to DIRECTORY where history files are stored

    prospect.params <- params[1:5]
    trait.values <- as.list(params[-(1:5)])
    out <- EDR.prospect(prospect.param = prospect.params,
                        prospect.version = 5,
                        trait.values = trait.values,  # ED trait values list, written to the `config.xml` parameter file
                        paths = paths,
                        par.wl = 400:2499, # Wavelengths for PAR vector
                        nir.wl = 2500,  # Wavelengths for NIR vector
                        datetime = ISOdate(2004, 07, 01, 12, 0, 0, tz="UTC"),  # Datetime at which EDR will run
                        edr.exe.name = 'ed_2.1-opt', # Name of the EDR executable. 
                        # It must be located in `output.path`.
                        # I recommend using a symlink.
                        output.path = "/path/to/output", # Where all outputs are stored
                        settings = list(model = list(revision = "git",
                                                     config.header = NULL)) # Required PEcAn settings
                        )
    return(out)
}

# This function should return a valid `params` vector for initializing the 
# MCMC. The initial conditions can be fixed or, preferably, sampled randomly 
# from a distribution (but the values must be valid; otherwise, the MCMC may 
# fail).
invert.options$inits.function <- function(){
    params <- c("N" = 1, "Cab" = 0, "Car" = 0,
                "Cw" = 0, "Cm" = 0,
                "clumping.factor" = 0.5)    # Etc. Etc.
    return(params)
}

# This function returns the log density of the `params` vector for the priors.
invert.options$prior.function <- function(){
    prior.dens <- dlnorm(params, prior.logmus, prior.logsigma, log=TRUE)
    return(sum(prior.dens))
}

# Vector of minimum values for parameters. If any parameters are sampled below 
# this minimum, the likelihood is set to extremely low probability.
invert.options$param.mins <- rep(0, length(invert.options$inits.function()))

# Other inversion settings
invert.options$ngibbs <- 500000        # Number of iterations
invert.options$nchains <- 6            # Number of chains
invert.options$burnin <- 300000        # Number of iterations discarded as "burnun"
invert.options$n.tries <- 5            # Number of attempts for invert.auto

# Generate some synthetic data. The `generate.noise` function generates some 
# random ,autocorrelated noise.
obs <- EDR.prospect(...) + generate.noise()

# Run the inversion.
inversion.output <- invert.auto(observed = obs,
                                invert.options = invert.options,
                                return.samples = TRUE,
                                save.samples = "file/name/for/saving/samples",  # Or NULL if not saving samples
                                parallel = TRUE,    # Whether or not to run in parallel
                                parallel.cores = 3 # How many parallel cores
                                )
