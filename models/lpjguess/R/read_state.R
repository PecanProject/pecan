library(stringr)

# this fcn is for potential natural vegetation only
# when there is landcover, there will be more stand types

# also for cohort mode only

# Gridcell: Top-level object containing all dynamic and static data for a particular gridcell
# Gridcellpft: Object containing data common to all individuals of a particular PFT in a particular gridcell
# Gridcellst : Object containing data common to all stands of a particular stand type (ST) in a particular gridcell 
# Climate : Contains all static and dynamic data relating to the overall environmental properties, other than soil properties, of a gridcell
# Soiltype : Stores soil static parameters. One object of class Soiltype  is  defined  for  each  gridcell.
# Stand : Object containing all dynamic and static data for a particular stand
# Patch : Stores data specific to a patch. In cohort and individual modes, replicate patches are required in each stand to accommodate stochastic variation across the site.
# Patchpft : Object containing data common to all individuals of a particular PFT in a particular patch, including litter pools. 
# Vegetation : A dynamic list of Individual objects, representing the vegetation of a particular patch
# Soil : Stores state variables for soils and the snow pack. One object of class Soil is defined for each patch.
# Fluxes : The Fluxes class stores accumulated monthly and annual fluxes. One object of type Fluxes is defined for each patch.
# Individual : Stores state variables for an average individual plant. In cohort mode, it is the average individual of a cohort of plants approximately the same age and from the same patch.

# maybe put guess.h and guess.cpp for each model version into the model package
guesscpp_loc <- "/fs/data5/pecan.models/LPJ-GUESS/framework/guess.cpp"
guessh_loc   <- "/fs/data5/pecan.models/LPJ-GUESS/framework/guess.h"

# guess.cpp has the info of what is being written
guesscpp_in <- readLines(guesscpp_loc)
# guess.h has the types so that we know what streamsize to read
guessh_in <- readLines(guessh_loc)
################################ check class compatibility ################################
# between model versions we don't expect major classes or hierarchy to change
# but give check and fail if necessary
LPJ_GUESS_CLASSES <- c("Gridcell", "Climate", "Gridcellpft", "Stand", "Standpft", "Patch", "Patchpft",
                       "Individual", "Soil", "Sompool", "Fluxes", "Vegetation")

lpjguess_classes <- list()
ctr <- 1
for(i in seq_along(guessh_in)){
  # search for "class XXX : public Serializable {"
  res <- str_match(guessh_in[i], "class (.*?) : public Serializable")
  if(is.na(res[,2])){
    # try "class XXX : public ..., public Serializable {" pattern
    res <- str_match(guessh_in[i], "class (.*?) : public .* Serializable")
  }
  if(!is.na(res[,2])){
    lpjguess_classes[[ctr]] <- res[,2]
    ctr <- ctr + 1  
  }
}

# all match?
if(!setequal(unlist(lpjguess_classes), LPJ_GUESS_CLASSES)){
  PEcAn.logger::logger.severe("This function can only read the following class objects: ", paste(LPJ_GUESS_CLASSES, collapse="--"))
}
