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
# NOTE THAT THESE PATTERNS ASSUME SOME CODING STYLE, thanks to LPJ-GUESS developers this might not be an issue in the future 
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

# Gridcell is the top-level container, start parsing from there
beg_end <- serialize_starts_ends(file_in = guesscpp_in, pattern = "void Gridcell::serialize")

# now we will parse the stuff between these lines
# first find what is being written
find_stream <- function(file_in = guesscpp_in, line_nos = beg_end)
  
# helper function that lists streamed variables, it just returns the names, types are checked by other fucntion
find_stream <- function(file_in, line_nos){
  
  streaming_list <- list()
  str.i <- 1
  arch_save <- FALSE
  
  for(i in line_nos[1]:line_nos[2]){
    
    # some functions (Vegetation, Patch, Stand, Gridcell) have two modes: saving / reading
    # we only need the stream that is saved
    if(grepl("arch.save()", file_in[i])){
      arch_save <- TRUE
      find_closing()
    } 
    
    # all streams start with arch &
    if(grepl("arch & ", file_in[i])){
      # get variable name
      streaming_list[[str.i]] <- sub(".*arch & ", "", file_in[i])
      str.i <- str.i + 1
      # check for ampersand for the subsequent variable names
      repeat{
        i <- i + 1
        if(!grepl(".*& ", file_in[i])) break # ONLEY NEED TO READ arch.save()e
        streaming_list[[str.i]] <- sub(".*& ", "", file_in[i])
        str.i <- str.i + 1
      }
    }
    sub("arch & ", "", file_in[1071])
  }
}


# helper function that scans LPJ-GUESS that returns the beginning and the ending lines of serialized object
serialize_starts_ends <- function(file_in, pattern = "void Gridcell::serialize"){
  # find the starting line from the given pattern
  starting_line <- which(!is.na(str_match(file_in, pattern)))
  if(length(starting_line) != 1){ # check what's going on 
    PEcAn.logger::logger.severe("Couldn't find the starting line with this pattern ***",pattern, "***.")
  }
    
  # screen for the closing curly bracket after function started 
  # closing bracket it i its own line without any tabs, note that this again assumes a certain coding style
  ending_line <- starting_line
  repeat{
    ending_line <- ending_line + 1
    if(file_in[ending_line] == "}") break
  }
  
  # probably a check is required, alternatively keep track of opening-closing brackets
  
  return(c(starting_line, ending_line))
}


find_closing <- function(find = "}", line_no, file_in){
  opened <- 1
  closed <- 0
  if(find == "}"){
    start_char <- "{"
    end_char   <- "}"
  }else{
    #there can be else-ifs, find closing paranthesis / square breacket etc
  }
  repeat{
    line_no <- line_no + 1
    if(grepl(start_char, file_in[line_no], fixed = TRUE))  opened <- opened + 1
    if(grepl(end_char,   file_in[line_no], fixed = TRUE))  closed <- closed + 1
    if(opened == closed) break
  }
  return(line_no)
}