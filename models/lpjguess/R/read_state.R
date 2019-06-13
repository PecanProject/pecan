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

############ open

# open connection to the binary state file
zz <- file("0.state", "rb")


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

# there are couple of LPJ-GUESS specific types that we'll need below
lpjguess_types <- list()
ctr <- 1
# NOTE THAT THESE PATTERNS ASSUME SOME CODING STYLE, thanks to LPJ-GUESS developers this might not be an issue in the future 
for(i in seq_along(guessh_in)){
  if(grepl("typedef enum {", guessh_in[i], fixed = TRUE)){
    this_line <- find_closing("}", i, guessh_in)
    l_type <- gsub(".*}(.*?);.*", "\\1", guessh_in[this_line]) 
    l_type <- gsub(" ", "", l_type)
    lpjguess_types[[ctr]] <- l_type
    ctr <- ctr + 1  
  }
}
LPJ_GUESS_TYPES <- unlist(lpjguess_types)

# Gridcell is the top-level container, start parsing from there
beg_end <- serialize_starts_ends(file_in = guesscpp_in, pattern = "void Gridcell::serialize")

# now we will parse the stuff between these lines
# first find what is being written
streamed_vars_gridcell <- find_stream_var(file_in = guesscpp_in, line_nos = beg_end)

# Now I can use streamed_vars_gridcell to loop over them
# We read everything in this loop, Gridcell list is going to be the top container
# there will be nested loops, the hierarchy will follow LPJ-GUESS architecture
Gridcell <- list()
for(g_i in seq_along(streamed_vars_gridcell)){ # Gridcell-loop starts
  
  current_stream <- streamed_vars_gridcell[g_i]
  current_stream_type <- find_stream_type(NULL, current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
  
  Gridcell[[length(Gridcell)+1]] <- list()
  names(Gridcell)[length(Gridcell)] <- current_stream_type$name
  if(current_stream_type$type == "class"){

    # CLASS
    class_name <- current_stream_type$name
    
    beg_end <- serialize_starts_ends(file_in = guesscpp_in, 
                                     pattern = paste0("void ",
                                                      tools::toTitleCase(current_stream_type$name), 
                                                      "::serialize"))
    streamed_vars <- find_stream_var(file_in = guesscpp_in, line_nos = beg_end)
    
    #for(sv_i in seq_along(streamed_vars)){
    for(sv_i in 1:26){
      current_stream <- streamed_vars[sv_i] #it's OK to overwrite
      current_stream_type <- find_stream_type(class_name, current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
      if(current_stream_type$type == "class"){
        
        # CLASS
        class_name <- current_stream_type$name
        
      }else{
        current_stream_specs <- find_stream_size(current_stream_type, guessh_in, LPJ_GUESS_TYPES)
        # and read!
        if(current_stream_specs$single){
          Gridcell[[length(Gridcell)]][[current_stream_type$name]] <- readBin(con  = zz, 
                                                                              what = current_stream_specs$what, 
                                                                              n    = current_stream_specs$n, 
                                                                              size = current_stream_specs$size)
        }else{
          for(css.i in seq_along(current_stream_specs$what)){
            Gridcell[[length(Gridcell)]][[current_stream_specs$names[css.i]]] <- readBin(con  = zz, 
                                                                                what = current_stream_specs$what[css.i], 
                                                                                n    = current_stream_specs$n[css.i], 
                                                                                size = current_stream_specs$size[css.i])
          }
        }

      }
    }
  }else{
    # NOT CLASS
  }

  
  
} # Gridcell-loop ends
  
# helper function that determines the stream size to read
find_stream_size <- function(current_stream_type, guessh_in, LPJ_GUESS_TYPES){
  
  possible_types <- c("double", "bool", "int")
  possible_types <- c(possible_types, LPJ_GUESS_TYPES)
  n_sizes  <- c(8, 1, 4, rep(4, length(LPJ_GUESS_TYPES) ))
  rbin_tbl <- c("double", "logical", "integer", rep("integer", length(LPJ_GUESS_TYPES)))
  
  specs <- list()
  
  sub_string <- current_stream_type$substring
  
  #is there a ; immediately after?
  if(grepl(paste0(current_stream_type$type, " ", current_stream_type$name, ";"), sub_string, fixed = TRUE)){
    # this is only length 1
    specs$n <- 1
    specs$what <- rbin_tbl[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
    specs$size <- n_sizes[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
    specs$single <- TRUE
    
  }else if(current_stream_type$type == "Historic"){
    # Historic types are special to LPJ-GUESS
    # They have stored values, current index, and a boolean in that order
    specs$n <- specs$what <- specs$size <- specs$names <- rep(NA, 3)
    # always three, this is a type defined in guessmath.h
    specs$what[1] <- rbin_tbl[sapply(possible_types, grepl, sub_string,  fixed = TRUE)] # I haven't seen any Historic that doesn't store double but...
    specs$size[1] <- n_sizes[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
    specs$names[1] <- current_stream_type$name
    # n is tricky, it can be hardcoded it can be one of the const ints
    to_read <- str_match(sub_string, paste0("Historic<", specs$what[1], ", (.*?)>.*"))[,2]
    #if(to_read %in% LPJ_GUESS_CONST_INTS){
      
    #}else{
      specs$n[1]   <- as.numeric(to_read)
    #}
    specs$what[2] <- "integer" #need to check what size_t is
    specs$size[2] <- 8
    specs$n[2]    <- 1
    specs$names[2] <- "current_index"
    
    specs$what[3] <- "logical"
    specs$size[3] <- 1
    specs$n[3]    <- 1
    specs$names[3] <- "full"   
    
    specs$single <- FALSE
    
  }else{
    # reading a vector
    specs$what   <- rbin_tbl[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
    specs$size   <- n_sizes[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
    specs$n      <- as.numeric(sub(".*\\[(.*)\\].*", "\\1", sub_string, perl=TRUE) )
    specs$single <- TRUE
  }
  
  return(specs)
} # find_stream_size


# helper function to decide the type of the stream
# this function relies on the architecture of LPJ-GUESS and has bunch of harcoded checks, see model documentation
find_stream_type <- function(class = NULL, current_stream_var, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in){

  # it might be difficult to extract the "type" before the varname
  # there are not that many to check
  possible_types <- c("class", "double", "bool", "int")
  
  possible_types <- c(possible_types, LPJ_GUESS_TYPES)
  
  beg_end <- NULL # not going to need it always
  
  # class or not?
  if(current_stream_var %in% tolower(LPJ_GUESS_CLASSES)){
    stream_type <- "class"
    stream_name <- current_stream_var
    sub_string  <- NULL
  }else {# find type from guess.h
    beg_end <- serialize_starts_ends(file_in = guessh_in, 
                                     pattern = paste0("class ",
                                                      tools::toTitleCase(class), 
                                                      " : public "))
    # subset 
    sub_string <- guessh_in[beg_end[1]:beg_end[2]][grepl(paste0(" ", current_stream_var, ";"), guessh_in[beg_end[1]:beg_end[2]], fixed = TRUE)]
    if(length(sub_string) == 0){
      sub_string <- guessh_in[beg_end[1]:beg_end[2]][grepl(paste0(" ", current_stream_var), guessh_in[beg_end[1]:beg_end[2]], fixed = TRUE)]
    }
    if(length(sub_string) > 1){
      PEcAn.logger::logger.severe("Check this out.")
    }
    
    # clean from tabs
    sub_string <- gsub("\t", "", sub_string)
    # clean from commented out lines
    
    if(grepl("Historic", sub_string, fixed = TRUE)){
      # Historic types has the form Historic<T, capacity>& data)
      stream_type <- "Historic"
      stream_name <- current_stream_var
    }else{
      stream_type <- possible_types[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
      stream_name <- current_stream_var
    }

  }
  
  return(list(type = stream_type, name = stream_name, substring = sub_string))
} # find_stream_type
  
  


######################## Helper functions ########################

# helper function that lists streamed variables, it just returns the names, types are checked by other fucntion
find_stream_var <- function(file_in, line_nos){
  
  streaming_list <- list()
  str.i <- 1
  when_here <- NULL
  not_skipping <- TRUE
  
  i <- line_nos[1]
  repeat{
    i <- i + 1
    if(!is.null(when_here)){
      if(i == when_here){
        i <- skip_to
        when_here <- NULL
      }
    }
    
    # some functions (Vegetation, Patch, Stand, Gridcell) have two modes: saving / reading
    # we only need the stream that is saved
    if(grepl("arch.save()", file_in[i])){
      when_here  <- find_closing("}", i, file_in)
      skip_to    <- find_closing("}", i, file_in, if_else_check = TRUE)
    } 
    
    # all streams start with arch &
    if(grepl("arch & ", file_in[i])){
      # get variable name
      streaming_list[[str.i]] <- sub(".*arch & ", "", file_in[i]) # always one var after arch?
      str.i <- str.i + 1
      # check for ampersand for the subsequent variable names
      repeat{
        i <- i + 1
        if(!is.null(when_here)){
          if(i == when_here){
            i <- skip_to
            when_here <- NULL
          }
        }
        if(!grepl(".*& ", file_in[i])) break # when there are no subsequent stream
        this_line <- gsub("[[:space:]]", "", strsplit(file_in[i], "& ")[[1]])
        for(var in this_line){
          if(var != ""){
            streaming_list[[str.i]] <- var
            str.i <- str.i + 1
          }
        }
      }
    }
    if(i == line_nos[2]) break
  }
  
  #unlist and nix the ;
  returnin_stream <- gsub(";", "", unlist(streaming_list), fixed = TRUE)
  return(returnin_stream)
} # find_stream_var



# helper function that scans LPJ-GUESS that returns the beginning and the ending lines of serialized object
serialize_starts_ends <- function(file_in, pattern = "void Gridcell::serialize"){
  # find the starting line from the given pattern
  starting_line <- which(!is.na(str_match(file_in, pattern)))
  if(length(starting_line) != 1){ # check what's going on 
    PEcAn.logger::logger.severe("Couldn't find the starting line with this pattern ***",pattern, "***.")
  }
    
  # screen for the closing curly bracket after function started 
  # keep track of opening-closing brackets
  ending_line <- find_closing(find = "}", starting_line, file_in)
  
  return(c(starting_line, ending_line))
} # serialize_starts_ends

# helper function that finds the closing bracket, can work over if-else
find_closing <- function(find = "}", line_no, file_in, if_else_check = FALSE){
  opened <- 1
  closed <- 0
  if(find == "}"){
    start_char <- "{"
    end_char   <- "}"
  }else{
    #there can be else-ifs, find closing paranthesis / square breacket etc
  }
  
  # check the immediate line and return if closed there already
  if(grepl(end_char,   file_in[line_no], fixed = TRUE))   return(line_no)
  
  repeat{
    line_no <- line_no + 1
    if(grepl(start_char, file_in[line_no], fixed = TRUE))  opened <- opened + 1
    if(grepl(end_char,   file_in[line_no], fixed = TRUE))  closed <- closed + 1
    if(if_else_check){
      else_found <- FALSE
      same_line_check <- grepl("else",   file_in[line_no], fixed = TRUE) #same line
      next_line_check <- grepl("else",   file_in[line_no + 1], fixed = TRUE) #next line
      if(same_line_check | next_line_check){
        closed <- closed - 1
        if(next_line_check) line_no <- line_no + 1
      }
    }
    if(opened == closed) break
  }
  return(line_no)
} # find_closing

