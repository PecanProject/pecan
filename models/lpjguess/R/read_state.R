
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
        check1 <- !grepl(".*& ", file_in[i]) # when there are no subsequent stream
        check2 <- !grepl(".*& ", file_in[i+1]) # sometimes following line is empty or commented, check the next one too
        if(check1 & !check2) i <- i+1
        if(check1 &  check2) break # looks like there are no subsequent stream
        this_line <- gsub("[[:space:]]", "", strsplit(file_in[i], "& ")[[1]])
        for(var in this_line){
          if(var != ""){
            if(var != "arch"){
              streaming_list[[str.i]] <- var
              str.i <- str.i + 1
            }
          }
        }
        if(!is.null(when_here)){ # now that increased i check this just in case
          if(i == when_here){
            i <- skip_to
            when_here <- NULL
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
    # new versions serialize structs too
    pattern <- gsub("class", "struct", pattern)
    starting_line <- which(!is.na(str_match(file_in, pattern)))
    if(length(starting_line) != 1){
      PEcAn.logger::logger.severe("Couldn't find the starting line with this pattern ***", pattern, "***.")
    }
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


# helper function that determines the stream size to read
find_stream_size <- function(current_stream_type, guessh_in, LPJ_GUESS_TYPES, LPJ_GUESS_CONST_INTS){
  
  possible_types <- c("double ", "bool ", "int " , "long ") # space because these can be part of other words
  possible_types <- c(possible_types, LPJ_GUESS_TYPES)
  n_sizes  <- c(8, 1, 4, 8, rep(4, length(LPJ_GUESS_TYPES) ))
  rbin_tbl <- c("double", "logical", "integer", "integer", rep("integer", length(LPJ_GUESS_TYPES)))
  
  specs <- list()
  
  sub_string <- current_stream_type$substring
  
  #is there a ; immediately after?
  if(grepl(paste0(current_stream_type$type, " ", current_stream_type$name, ";"), sub_string, fixed = TRUE) |
     grepl(paste0(current_stream_type$type, " ", current_stream_type$name, ","), sub_string, fixed = TRUE)){ # e.g. "double alag, exp_alag;"
    # this is only length 1
    specs$n <- 1
    specs$what <- rbin_tbl[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
    specs$what <- unique(specs$what)
    specs$size <- n_sizes[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
    specs$size <- unique(specs$size)
    specs$single <- TRUE
    
  }else if(current_stream_type$type == "Historic"){
    possible_types <- c("double", "bool", "int" , "long") # # I haven't seen any Historic that doesn't store double but... historic has a comma after type: double,
    possible_types <- c(possible_types, LPJ_GUESS_TYPES)
    
    # Historic types are special to LPJ-GUESS
    # They have stored values, current index, and a boolean in that order

    #is there a following bracket? 
    if(grepl("\\[*\\]", sub_string)){ # e.g. "Historic<double, 20> hmtemp_20[12];"
      to_read <- str_match(sub_string, paste0("Historic<double, (.*?)>.*"))[,2]
      if(to_read %in% LPJ_GUESS_CONST_INTS$var){
        nvar <- LPJ_GUESS_CONST_INTS$val[LPJ_GUESS_CONST_INTS$var == to_read]
      }else{
        nvar <- as.numeric(to_read)
      }

      ntimes <- as.numeric(str_match(sub_string, paste0("Historic<.*>.*\\[(.*?)\\]"))[,2])
      
      specs <- vector("list", 3*ntimes)
      for(specs.i in seq_along(specs)){
        specs[[specs.i]] <- list()
        if(specs.i %% 3 == 1){
          specs[[specs.i]]$what <- "double"
          specs[[specs.i]]$n    <- 20
          specs[[specs.i]]$size <- 8
        }else if(specs.i %% 3 == 2){
          specs[[specs.i]]$what <- "integer"
          specs[[specs.i]]$n    <- 1
          specs[[specs.i]]$size <- 8
        }else if(specs.i %% 3 == 0){
          specs[[specs.i]]$what <- "logical"
          specs[[specs.i]]$n    <- 1
          specs[[specs.i]]$size <- 8
        }

      }
      specs$name   <- current_stream_type$name
      specs$single <- FALSE
    }else{ # e.g. "Historic<double, 31> deet_31;"
      
      specs$n <- specs$what <- specs$size <- specs$names <- rep(NA, 3)
      # always three, this is a type defined in guessmath.h
      specs$what[1]  <- rbin_tbl[sapply(possible_types, grepl, sub_string,  fixed = TRUE)] 
      specs$size[1]  <- n_sizes[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
      specs$names[1] <- current_stream_type$name
      # n is tricky, it can be hardcoded it can be one of the const ints
      to_read <- str_match(sub_string, paste0("Historic<", specs$what[1], ", (.*?)>.*"))[,2]
      if(to_read %in% LPJ_GUESS_CONST_INTS$var){
        specs$n      <- LPJ_GUESS_CONST_INTS$val[LPJ_GUESS_CONST_INTS$var == to_read]
      }else{
        specs$n[1]   <- as.numeric(to_read)
      }
      specs$what[2]  <- "integer" #need to check what size_t is
      specs$size[2]  <- 8
      specs$n[2]     <- 1
      specs$names[2] <- "current_index"
      
      specs$what[3]  <- "logical"
      specs$size[3]  <- 1
      specs$n[3]     <- 1
      specs$names[3] <- "full"   
      
      specs$single <- FALSE
    }


    
  }else if(current_stream_type$type == "struct"){
    if(current_stream_type$name != "solvesom"){
      PEcAn.logger::logger.debug("Another struct type.")
    }

    # with these you're supposed to first read the dimensions of the array and then the values stored
    # hardcoding this for now
    specs$n <- 1
    specs$what <- "integer"
    specs$size <- 8
    specs$single <- TRUE
    
  }else if(grepl(glob2rx(paste0(current_stream_type$type, "*", current_stream_type$name, ";")), sub_string)){
    
    # this is only length 1
    specs$n <- 1
    specs$what <- rbin_tbl[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
    specs$size <- n_sizes[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
    specs$single <- TRUE
    
  }else if(length(regmatches(sub_string, gregexpr("\\[.+?\\]", sub_string))[[1]]) > 1){
    #looks like we have a matrix
    spec_dims <- regmatches(sub_string, gregexpr("\\[.+?\\]", sub_string))[[1]]
    spec_dims <- gsub("\\].*", "", gsub(".*\\[", "", spec_dims))
    for(spec_dims_i in seq_along(spec_dims)){
      if(any(sapply(LPJ_GUESS_CONST_INTS$var, grepl, spec_dims[spec_dims_i],  fixed = TRUE))){ # uses one of the constant ints
        spec_dims[spec_dims_i] <- LPJ_GUESS_CONST_INTS$val[sapply(LPJ_GUESS_CONST_INTS$var, grepl, spec_dims[spec_dims_i],  fixed = TRUE)]
      }else{
        spec_dims[spec_dims_i] <- as.numeric(sub(".*\\[(.*)\\].*", "\\1", spec_dims[spec_dims_i], perl=TRUE))
      }
    }
    spec_dims <- as.numeric(spec_dims)
    
    specs$n      <- prod(spec_dims)
    specs$what   <- rbin_tbl[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
    specs$size   <- n_sizes[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
    specs$single <- TRUE
  }else{
    # reading a vector
    specs$what   <- rbin_tbl[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
    specs$size   <- n_sizes[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
    if(any(sapply(LPJ_GUESS_CONST_INTS$var, grepl, sub_string,  fixed = TRUE))){ # uses one of the constant ints
      specs$n      <- LPJ_GUESS_CONST_INTS$val[sapply(LPJ_GUESS_CONST_INTS$var, grepl, sub_string,  fixed = TRUE)]
    }else{
      specs$n      <- as.numeric(sub(".*\\[(.*)\\].*", "\\1", sub_string, perl=TRUE))
    }
    
    specs$single <- TRUE
  }
  
  return(specs)
} # find_stream_size


# helper function to decide the type of the stream
# this function relies on the architecture of LPJ-GUESS and has bunch of harcoded checks, see model documentation
find_stream_type <- function(class = NULL, current_stream_var, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in){
  
  if(current_stream_var == "seed"){ # a bit of a special case
    return(list(type = "long", name = "seed", substring = "long seed;"))
  }
  
  if(current_stream_var == "nstands"){ # a bit of a special case, it is read by guess.cpp
    return(list(type = "int", name = "nstands", substring = "int nstands;")) #there is not substring like that in guess.h
  }  
  
  # "landcover" differs from version to version
  if(current_stream_var == "landcover" && !is.null(class)){ # a bit of a special case
    return(list(type = "landcovertype", name = "landcover", substring = "landcovertype landcover;"))
  } 
  
  # it might be difficult to extract the "type" before the varname
  # there are not that many to check
  possible_types <- c("class ", "double ", "bool ", "int ")
  
  possible_types <- c(possible_types, paste0(LPJ_GUESS_TYPES, " "))
  
  beg_end <- NULL # not going to need it always
  
  # class or not?
  if(tools::toTitleCase(current_stream_var) %in% LPJ_GUESS_CLASSES){
    stream_type <- "class"
    stream_name <- tools::toTitleCase(current_stream_var)
    sub_string  <- NULL
  }else {# find type from guess.h
    
    if(is.null(class)){
      sub_string <- guessh_in[grepl(paste0(" ", current_stream_var), guessh_in, fixed = TRUE)]
    }else{
      beg_end <- serialize_starts_ends(file_in = guessh_in, 
                                       pattern = paste0("class ",
                                                        tools::toTitleCase(class), 
                                                        " : public "))
      # subset 
      sub_string <- guessh_in[beg_end[1]:beg_end[2]][grepl(paste0(" ", current_stream_var, ";"), guessh_in[beg_end[1]:beg_end[2]], fixed = TRUE)]
    }
    
    if(length(sub_string) == 0){
      sub_string <- guessh_in[beg_end[1]:beg_end[2]][grepl(paste0(" ", current_stream_var), guessh_in[beg_end[1]:beg_end[2]], fixed = TRUE)]
    }
    # e.g. "sompool[i]" in guess.cpp, Sompool sompool[NSOMPOOL]; in guess.h
    if(length(sub_string) == 0){
      current_stream_var <- gsub("\\[|.\\]", "", current_stream_var)
      sub_string <- guessh_in[beg_end[1]:beg_end[2]][grepl(paste0(" ", current_stream_var), guessh_in[beg_end[1]:beg_end[2]], fixed = TRUE)]
      if(tools::toTitleCase(current_stream_var) %in% LPJ_GUESS_CLASSES){
        stream_type <- "class"
        stream_name <- current_stream_var
        sub_string  <- NULL
        return(list(type = gsub(" ", "", stream_type), name = stream_name, substring = sub_string))
      }
    }
    if(length(sub_string) == 0){
      sub_string <- guessh_in[beg_end[1]:beg_end[2]][grepl(paste0(",", current_stream_var), guessh_in[beg_end[1]:beg_end[2]], fixed = TRUE)]
    }
    if(length(sub_string) > 1){
      
      # some varnames are very common characters unfortunately like u, v... check if [] comes after
      if(any(grepl(paste0(" ", current_stream_var, "["), sub_string, fixed = TRUE))){
        sub_string <- sub_string[grepl(paste0(" ", current_stream_var, "["), sub_string, fixed = TRUE)]
      }else if(any(grepl(paste0("double ", current_stream_var), sub_string, fixed = TRUE))){ # just fishing, double is the most common type
        sub_string <- sub_string[grepl(paste0("double ", current_stream_var), sub_string, fixed = TRUE)]
      }else if(any(grepl("///", sub_string, fixed = TRUE))){ # three slashes are very common in commented out code
        sub_string <- sub_string[!grepl("///", sub_string, fixed = TRUE)]
      }
      
      if(length(unique(sub_string)) == 1){
        sub_string <- unique(sub_string)
      }else{
        PEcAn.logger::logger.severe("Check this out.")
      } 
    }
    
    # clean from tabs
    sub_string <- gsub("\t", "", sub_string)
    # clean from commented out lines?
    
    if(grepl("Historic", sub_string, fixed = TRUE)){
      # Historic types has the form Historic<T, capacity>& data)
      stream_type <- "Historic"
      stream_name <- current_stream_var
    }else if(grepl("std::vector", sub_string, fixed = TRUE)){
      stream_type <- "struct"
      stream_name <- current_stream_var
    }else{
      stream_type <- possible_types[sapply(possible_types, grepl, sub_string,  fixed = TRUE)]
      if(length(stream_type) == 2){ # one string nested in the other?
        # try the longer
        if(grepl(stream_type[which.max(nchar(stream_type))], sub_string,  fixed = TRUE)){
          stream_type <- stream_type[which.max(nchar(stream_type))]
        }
      }else if(length(stream_type) > 2){ # haven't seen anything like that yet
        PEcAn.logger::logger.severe("Check this out.")
      }
      stream_name <- current_stream_var
    }
    
  }
  
  return(list(type = gsub(" ", "", stream_type), name = stream_name, substring = sub_string))
} # find_stream_type


###################################### READ STATE

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


# test path
#outdir <- "/fs/data2/output/PEcAn_1000010473/out/1002656304"

# outdir, at least model version, maybe also settings
read_binary_LPJGUESS <- function(outdir, version = "PalEON"){
  
  # find rundir too, params.ins is in there and we need to get some values from there
  rundir <- file.path(dirname(dirname(outdir)), "run", basename(outdir))
  
  # guess.cpp has the info of what is being written
  guesscpp_name <- paste0("guess.", version, ".cpp")  # these are gonna be in the package guess.VERSION.cpp
  guesscpp_in   <- readLines(con = system.file(guesscpp_name, package = "PEcAn.LPJGUESS"), n = -1)
  # guess.h has the types so that we know what streamsize to read
  guessh_name <- paste0("guess.", version, ".h") 
  guessh_in   <- readLines(con = system.file(guessh_name, package = "PEcAn.LPJGUESS"), n = -1)
  # parameters.h has some more types
  paramh_name <- paste0("parameters.", version, ".h") 
  paramh_in   <- readLines(con = system.file(paramh_name, package = "PEcAn.LPJGUESS"), n = -1)
  
  ### these are the values read from params.ins, passed to this fcn
  paramsins <- readLines(file.path(rundir, "params.ins"), n = -1)
  npatches  <- as.numeric(gsub(".*([0-9]+).*$", "\\1", paramsins[grepl("npatch", paramsins, fixed = TRUE)]))
  
  
  ######################################
  ## read meta.bin
  # not sure if the content will change under guessserializer.cpp
  meta_data    <- list()
  meta_bin_con <- file(file.path(outdir, "meta.bin"), "rb")
  meta_data$num_processes    <- readBin(meta_bin_con, integer(), 1, size = 4)
  meta_data$vegmode    <- readBin(meta_bin_con, integer(), 1, size = 4)
  meta_data$npft    <- readBin(meta_bin_con, integer(), 1, size = 4)
  meta_data$pft <- list()
  for(i in seq_len(meta_data$npft)){
    char_len    <- readBin(meta_bin_con, integer(), 1, size = 8)
    meta_data$pft[[i]]  <- readChar(meta_bin_con, char_len)
  }
  close(meta_bin_con)
  
  # how many PFTs are there in this run
  n_pft     <- meta_data$npft
  
  # open connection to the binary state file
  if(meta_data$num_processes == 1){
    zz <- file(file.path(outdir,"0.state"), "rb")
  }else{
    # then file names would be different 1.state etc etc
    PEcAn.logger::logger.severe("This function is implemented to read state from 1 process only.")
  }
  
  
  
  
  ################################ CHECKS AND EXTRACTIONS ################################
  
  # between model versions we don't expect major classes or hierarchy to change
  # but give check and fail if necessary
  LPJ_GUESS_CLASSES <- c("Gridcell", "Climate", "Gridcellpft", "Gridcellst", "Stand", "Standpft", "Patch", "Patchpft",
                         "Individual", "Soil", "Sompool", "Fluxes", "Vegetation", "PhotosynthesisResult", "LitterSolveSOM",
                         "Landcover", "MassBalance")
  
  lpjguess_classes <- list()
  ctr <- 1
  # NOTE THAT THESE PATTERNS ASSUME SOME CODING STYLE, thanks to LPJ-GUESS developers this might not be an issue in the future 
  for(i in seq_along(guesscpp_in)){
    # search for "class XXX : public Serializable {"
    res <- str_match(guesscpp_in[i], "void (.*?)::serialize\\(ArchiveStream\\& arch\\)")
    if(!is.na(res[,2]) && !(res[,2] %in% c("cropindiv_struct", "cropphen_struct"))){ # no crops for now
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
  for(i in seq_along(guessh_in)){
    if(grepl("typedef enum {", guessh_in[i], fixed = TRUE)){
      this_line <- find_closing("}", i, guessh_in)
      l_type <- gsub(".*}(.*?);.*", "\\1", guessh_in[this_line]) 
      l_type <- gsub(" ", "", l_type)
      lpjguess_types[[ctr]] <- l_type
      ctr <- ctr + 1  
    }
  }
  for(i in seq_along(paramh_in)){ #do same for parameters.h
    if(grepl("typedef enum {", paramh_in[i], fixed = TRUE)){
      this_line <- find_closing("}", i, paramh_in)
      l_type <- gsub(".*}(.*?);.*", "\\1", paramh_in[this_line]) 
      l_type <- gsub(" ", "", l_type)
      lpjguess_types[[ctr]] <- l_type
      ctr <- ctr + 1  
    }
  }
  LPJ_GUESS_TYPES <- unlist(lpjguess_types)
  
  
  lpjguess_consts <- list()
  ctr <- 1
  for(i in seq_along(guessh_in)){
    if(grepl("const int ", guessh_in[i], fixed = TRUE)){ # probably won't need "const double"s
      cnst_val <- gsub(".*=(.*?);.*", "\\1", guessh_in[i]) 
      cnst_val <- gsub(" ", "", cnst_val) # get rid of the space if there is one
      cnst_nam <- gsub(".*int(.*?)=.*", "\\1", guessh_in[i]) 
      cnst_nam <- gsub(" ", "", cnst_nam) 
      lpjguess_consts[[ctr]]      <- cnst_val
      names(lpjguess_consts)[ctr] <- cnst_nam
      ctr <- ctr + 1  
    }
  }
  # few cleaning
  dont_need <- c("COLDEST_DAY_NHEMISPHERE", "COLDEST_DAY_SHEMISPHERE", "WARMEST_DAY_NHEMISPHERE", "WARMEST_DAY_SHEMISPHERE", "data[]")
  lpjguess_consts[match(dont_need, names(lpjguess_consts))] <-  NULL
  
  
  # need to parse out few  more constants
  for(i in seq_along(paramh_in)){ #do same for parameters.h
    res <- str_match(paramh_in[i], "typedef enum \\{(.*?)\\} landcovertype\\;")
    if(!is.na(res[,2])){
      lpjguess_consts$NLANDCOVERTYPES <- length(strsplit(res[,2], ",")[[1]]) - 1 # last element is NLANDCOVERTYPES
    }
  }
  for(i in seq_along(guessh_in)){ 
    if(grepl("enum PerPatchFluxType {", guessh_in[i], fixed = TRUE)){
      cl_i <- find_closing("}", i, guessh_in)
      #get rid of commented out lines
      sub_string <- guessh_in[i:cl_i][!grepl("///", guessh_in[i:cl_i], fixed = TRUE)]
      # split and count
      lpjguess_consts$PerPatchFluxType <- length(strsplit(paste(sub_string, collapse = " "), ",")[[1]]) - 1
    }
    if(grepl("enum PerPFTFluxType {", guessh_in[i], fixed = TRUE)){
      cl_i <- find_closing("}", i, guessh_in)
      #get rid of commented out lines
      sub_string <- guessh_in[i:cl_i][!grepl("///", guessh_in[i:cl_i], fixed = TRUE)]
      # split and count
      lpjguess_consts$PerPFTFluxType <- length(strsplit(paste(sub_string, collapse = " "), ",")[[1]]) - 1
    }
    
  }
  
  # this needs to be extracted from guess.h:93-94 , but hardcoding for now 
  # hopefully CENTURY pool names might not change for a while
  lpjguess_consts$NSOMPOOL <- 12
  
  
  LPJ_GUESS_CONST_INTS <- data.frame(var = names(lpjguess_consts), val = as.numeric(unlist(lpjguess_consts)), stringsAsFactors = FALSE)
  
  
  # Gridcell is the top-level container, start parsing from there
  beg_end <- serialize_starts_ends(file_in = guesscpp_in, pattern = "void Gridcell::serialize")
  
  # now we will parse the stuff between these lines
  # first find what is being written
  streamed_vars_gridcell <- find_stream_var(file_in = guesscpp_in, line_nos = beg_end)
  
  ################################## CAUTION : THE FOLLOWING IS A MONSTROUS NESTED-LOOP ##################################
  
  # Now I can use streamed_vars_gridcell to loop over them
  # We read everything in this loop, Gridcell list is going to be the top container
  # there will be nested loops, the hierarchy will follow LPJ-GUESS architecture
  Gridcell <- list()
  level <- "Gridcell"
  for(g_i in seq_along(streamed_vars_gridcell)){ # Gridcell-loop starts
    current_stream <- streamed_vars_gridcell[g_i]
    # weird, it doesn't go into Gridcell st
    if(current_stream == "st[i]")   next #current_stream <- "Gridcellst" 
    if(current_stream == "balance") current_stream <- "MassBalance" #not sure how to make this name matching otherwise
    if(grepl(glob2rx("pft[*]"), current_stream)) current_stream <- paste0(level, "pft") # i counter might change, using wildcard
    if(grepl(glob2rx("(*this)[*].landcover"), current_stream)){ # s counter might change, using wildcard
      # not sure how to handle this better. If we see this, it means we are now looping over Stands
      # this function considers "NATURAL" vegetation only, so there is only one stand
      # this is an integer that tells us which landcover type this stand is
      # so it should be the indice of NATURAL in typedef enum landcovertype (I believe indexing starts from 0)
      
      # note that this is streamed under Gridcell, not Stand in guess.cpp, 
      # but I think this info needs to go together with the Stand sublist
      # so prepend landcovertype to the streamed_vars_stand EDIT: I'll actually just read it here
      Gridcell[["Stand"]][["landcovertype"]] <- readBin(zz, what = integer(), n = 1, size = 4)
      num_stnd <- as.numeric(Gridcell$nstands)
      Gridcell[["Stand"]] <- vector("list", num_stnd) 
      
      next
    } 
    
    # "(*this)[*]" points to different things under different levels, here it is stand
    if(grepl(glob2rx("(*this)[*]"), current_stream)){ # note that first else-part will be evaluated considering the order in guess.cpp
      
      # STAND
      level <- "Stand"
      current_stream <- "Stand"
      current_stream_type <- find_stream_type(NULL, current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
      
      beg_end <- serialize_starts_ends(file_in = guesscpp_in, 
                                       pattern = paste0("void ",
                                                        tools::toTitleCase(current_stream_type$name), 
                                                        "::serialize"))
      streamed_vars_stand <- find_stream_var(file_in = guesscpp_in, line_nos = beg_end)
      # this was previous version
      # streamed_vars_stand <- c("landcover", streamed_vars_stand) # prepending landcovertype to the streamed_vars_stand
      
      
      for(stnd_i in seq_len(num_stnd)){ #looping over the stands
        for(svs_i in seq_along(streamed_vars_stand)){ # looping over the streamed stand vars
          
          current_stream <- streamed_vars_stand[svs_i]
          if(grepl(glob2rx("pft[*]"), current_stream)) current_stream <- paste0(level, "pft") # i counter might change, using wildcard
          
          if(current_stream == "nobj" & level == "Stand"){
            # nobj points to different things under different levels, here it is the number of patches
            # number of patches is set through insfiles, read by write.configs and passed to this fcn
            # but it's also written to the state file, need to move bytes
            nofpatch <- readBin(zz, integer(), 1, size = 4)  
            if(npatches == nofpatch){ # also not a bad place to check if everything is going fine so far
              Gridcell[["Stand"]][[stnd_i]]$npatches <- npatches
              #Gridcell[["Stand"]] <- vector("list", npatches) 
            }else{
              PEcAn.logger::logger.severe("The number of patches set through the instruction file does not match the number read from the state files. Probably a bug in the read.state function! Terminating.")
            }
            next
          }
          
          # "(*this)[*]" points to different things under different levels, here it is patch
          if(grepl(glob2rx("(*this)[*]"), current_stream)){ 
            # PATCH
            level <- "Patch"
            current_stream <- "Patch"
            current_stream_type <- find_stream_type(NULL, current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
            
            beg_end <- serialize_starts_ends(file_in = guesscpp_in, 
                                             pattern = paste0("void ",
                                                              tools::toTitleCase(current_stream_type$name), 
                                                              "::serialize"))
            streamed_vars_patch <- find_stream_var(file_in = guesscpp_in, line_nos = beg_end)
            
            Gridcell[["Stand"]][[stnd_i]][["Patch"]] <- vector("list", npatches) 
            
            for(ptch_i in seq_len(npatches)){ #looping over the patches
              for(svp_i in seq_along(streamed_vars_patch)){ #looping over the streamed patch vars
                current_stream <- streamed_vars_patch[svp_i]
                if(grepl(glob2rx("pft[*]"), current_stream)) current_stream <- paste0(level, "pft") # i counter might change, using wildcard
                
                if(tools::toTitleCase(current_stream) %in% LPJ_GUESS_CLASSES){
                  current_stream_type <- find_stream_type(NULL, current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
                }else{
                  current_stream_type <- find_stream_type("Patch", current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
                }
                
                
                Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][[length(Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]])+1]] <- list()
                names(Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]])[length(Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]])] <- current_stream_type$name
                
                if(current_stream_type$type == "class"){
                  
                  # CLASS
                  class_name <- current_stream_type$name
                  
                  beg_end <- serialize_starts_ends(file_in = guesscpp_in, 
                                                   pattern = paste0("void ",
                                                                    tools::toTitleCase(current_stream_type$name), 
                                                                    "::serialize"))
                  
                  
                  if(class_name == "Vegetation"){
                    # VEGETATION
                    # Vegetation class has a bit of a different structure, it has one more depth, see model documentation
                    streamed_vars_veg <- find_stream_var(file_in = guesscpp_in, line_nos = beg_end)
                    
                    # NOTE : Unlike other parts, this bit is a lot less generalized!!!
                    # I'm gonna asumme Vegetation class won't change much in the future
                    # indiv.pft.id and indiv needs to be looped over nobj times
                    if(!setequal(streamed_vars_veg, c("nobj", "indiv.pft.id", "indiv"))){
                      PEcAn.logger::logger.severe("Vegetation class object changed in this model version, you need to fix read.state")
                    }
                    
                    # nobj points to different things under different levels, here it is the number of individuals
                    number_of_individuals <- readBin(zz, integer(), 1, size = 4) 
                    Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Vegetation"]] <- list()
                    Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Vegetation"]][["number_of_individuals"]] <- number_of_individuals
                    
                    # few checks for sensible vals
                    if(number_of_individuals < 0 | number_of_individuals > 10000){ # should there be an upper limit here too?
                      # if number of individuals is 0 it's a bit suspicious. Not sure if ever will get negative but that'd definitely be wrong
                      PEcAn.logger::logger.warn("Number of individuals under vegetation is", number_of_individuals)
                    }
                    #Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Vegetation"]] <- list()
                    Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Vegetation"]][["Individuals"]] <- vector("list", number_of_individuals) 
                    
                    beg_end <- serialize_starts_ends(file_in = guesscpp_in, 
                                                     pattern = paste0("void Individual::serialize"))
                    streamed_vars_indv <- find_stream_var(file_in = guesscpp_in, line_nos = beg_end)
                    
                    # NO CROPS
                    if("*cropindiv" %in% streamed_vars_indv) streamed_vars_indv <- streamed_vars_indv[!(streamed_vars_indv == "*cropindiv")]
                    
                    # loop over nobj
                    for(indv_i in seq_len(number_of_individuals)){
                      Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Vegetation"]][["Individuals"]][[indv_i]] <- list()
                      # which PFT is this?
                      Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Vegetation"]][["Individuals"]][[indv_i]][["indiv.pft.id"]] <- readBin(zz, integer(), 1, size = 4)
                      # read all the individual class
                      for(svi_i in seq_along(streamed_vars_indv)){ # 
                        
                        current_stream <- streamed_vars_indv[svi_i] 
                        if(current_stream == "photosynthesis") current_stream <- "PhotosynthesisResult"
                        
                        if(tools::toTitleCase(current_stream) %in% LPJ_GUESS_CLASSES){
                          current_stream_type <- find_stream_type(NULL, current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
                        }else{
                          # Only Individual class under Vegetation
                          current_stream_type <- find_stream_type("Individual", current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
                        }
                        
                        if(current_stream_type$type == "class"){
                          
                          if(current_stream_type$name != "PhotosynthesisResult"){
                            PEcAn.logger::logger.debug("Classes other than PhotosynthesisResult enter here.")
                          }
                          # ONLY PhotosynthesisResult HERE SO FAR ******************************************************************
                          beg_end <- serialize_starts_ends(file_in = guesscpp_in, 
                                                           pattern = paste0("void ",
                                                                            tools::toTitleCase(current_stream_type$name), 
                                                                            "::serialize"))
                          streamed_vars_photo <- find_stream_var(file_in = guesscpp_in, line_nos = beg_end)
                          
                          Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Vegetation"]][["Individuals"]][[indv_i]][["PhotosynthesisResult"]] <- list()
                          for(photo_i in seq_along(streamed_vars_photo)){
                            current_stream <- streamed_vars_photo[photo_i] #it's OK to overwrite
                            current_stream_type <- find_stream_type("PhotosynthesisResult", current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
                            
                            current_stream_specs <- find_stream_size(current_stream_type, guessh_in, LPJ_GUESS_TYPES, LPJ_GUESS_CONST_INTS)
                            # and read!
                            
                            Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Vegetation"]][["Individuals"]][[indv_i]][["PhotosynthesisResult"]][[current_stream_type$name]] <- readBin(con  = zz, 
                                                                                                                                                                                           what = current_stream_specs$what, 
                                                                                                                                                                                           n    = current_stream_specs$n, 
                                                                                                                                                                                           size = current_stream_specs$size)
                            
                          }# streamed_vars_photo-loop ends
                          
                        }else{
                          
                          
                          current_stream_type  <- find_stream_type("individual", current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
                          current_stream_specs <- find_stream_size(current_stream_type, guessh_in, LPJ_GUESS_TYPES, LPJ_GUESS_CONST_INTS)
                          
                          if(current_stream_specs$single){
                            Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Vegetation"]][["Individuals"]][[indv_i]][[current_stream_type$name]] <- readBin(con  = zz, 
                                                                                                                                                                 what = current_stream_specs$what, 
                                                                                                                                                                 n    = current_stream_specs$n, 
                                                                                                                                                                 size = current_stream_specs$size)
                          }else{
                            for(css.i in seq_along(current_stream_specs$what)){
                              Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Vegetation"]][["Individuals"]][[indv_i]][[current_stream_specs$names[css.i]]]<- readBin(con  = zz, 
                                                                                                                                                                           what = current_stream_specs$what[css.i], 
                                                                                                                                                                           n    = current_stream_specs$n[css.i], 
                                                                                                                                                                           size = current_stream_specs$size[css.i])
                            }
                          }
                        }
                      }# end loop over stream vars individual
                    } # end loop over number_of_individuals
                    
                    
                    
                    
                    
                  }else if(class_name == "Fluxes"){
                    # FLUXES
                    # this is not generalized at all
                    streamed_vars_flux <- find_stream_var(file_in = guesscpp_in, line_nos = beg_end)
                    
                    if(!setequal(streamed_vars_flux, c("annual_fluxes_per_pft", "monthly_fluxes_patch",  "monthly_fluxes_pft"))){
                      PEcAn.logger::logger.severe("Fluxes class object changed in this model version, you need to fix read.state")
                    }
                    
                    # annual_fluxes_per_pft loops over 
                    # parse from guess.h
                    PerPFTFluxType <- c("NPP", "GPP", "RA", "ISO", "MON")
                    Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Fluxes"]][["annual_fluxes_per_pft"]] <- list()
                    key1 <- readBin(zz, "integer", 1, 8)
                    Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Fluxes"]][["annual_fluxes_per_pft"]][["n_pft"]] <- key1
                    for(fpft_i in seq_len(key1)){ # key1 11 PFTs
                      Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Fluxes"]][["annual_fluxes_per_pft"]][[fpft_i]] <- list()
                      key2 <- readBin(zz, "integer", 1, 8)
                      if(key2 > 10000){ #make sure you dind't read a weird number, this is supposed to be number of fluxes per pft, can't have too many
                        PEcAn.logger::logger.severe("Number of fluxes per pft read from the state file is too high. Check read.state function")
                      }
                      Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Fluxes"]][["annual_fluxes_per_pft"]][[fpft_i]][["key2"]] <- key2
                      for(flux_i in seq_len(key2)){
                        # is this double?
                        Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Fluxes"]][["annual_fluxes_per_pft"]][[fpft_i]][[PerPFTFluxType[flux_i]]] <- readBin(zz, "double", 1, 8)
                      }
                    }
                    
                    # monthly_fluxes_patch read as a vector at once
                    # double monthly_fluxes_patch[12][NPERPATCHFLUXTYPES];
                    # maybe read this as a matrix?
                    n_monthly_fluxes_patch <- 12 * LPJ_GUESS_CONST_INTS$val[LPJ_GUESS_CONST_INTS$var =="PerPatchFluxType"]
                    Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Fluxes"]][["monthly_fluxes_patch"]] <- readBin(zz, "double", n_monthly_fluxes_patch, 8)
                    
                    # monthly_fluxes_pft read as a vector at once
                    # double monthly_fluxes_pft[12][NPERPFTFLUXTYPES];
                    # maybe read this as a matrix?
                    n_monthly_fluxes_pft <- 12 * LPJ_GUESS_CONST_INTS$val[LPJ_GUESS_CONST_INTS$var =="PerPFTFluxType"]
                    Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Fluxes"]][["monthly_fluxes_pft"]] <- readBin(zz, "double", n_monthly_fluxes_pft, 8)
                    
                  }else{
                    # NOT VEGETATION OR FLUX
                    streamed_vars <- find_stream_var(file_in = guesscpp_in, line_nos = beg_end)
                    # NO CROPS, NATURAL VEG
                    if("*cropphen" %in% streamed_vars) streamed_vars <- streamed_vars[!(streamed_vars == "*cropphen")]
                    num_pft <- ifelse(grepl("pft", current_stream_type$name, fixed = TRUE), n_pft, 1)
                    
                    for(varname in streamed_vars){
                      Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][[current_stream_type$name]][[varname]] <- vector("list", num_pft) 
                    }
                    
                    # maybe try modifying this bit later to make it a function
                    for(pft_i in seq_len(num_pft)){
                      for(sv_i in seq_along(streamed_vars)){ 
                        current_stream <- streamed_vars[sv_i] #it's OK to overwrite
                        current_stream_type <- find_stream_type(class_name, current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
                        
                        if(current_stream_type$type == "class"){
                          
                          if(current_stream_type$name != "sompool"){
                            PEcAn.logger::logger.debug("Classes other than sompool enter here.")
                          }
                          # ONLY SOMPOOL HERE SO FAR ******************************************************************
                          # code below is very sompool specific
                          # class_name <- # don't overwrite class_name
                          
                          beg_end <- serialize_starts_ends(file_in = guesscpp_in, 
                                                           pattern = paste0("void ",
                                                                            tools::toTitleCase(current_stream_type$name), 
                                                                            "::serialize"))
                          streamed_vars_sompool <- find_stream_var(file_in = guesscpp_in, line_nos = beg_end)
                          
                          nsompool <- LPJ_GUESS_CONST_INTS$val[LPJ_GUESS_CONST_INTS$var == "NSOMPOOL"]
                          
                          for(varname in streamed_vars_sompool){
                            Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Soil"]][["sompool[i]"]][[varname]] <- vector("list", nsompool) 
                          }
                          
                          names( Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Soil"]])[names( Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Soil"]]) == "sompool[i]"] <- "Sompool"
                          
                          ###################### LOOP OVER NSOMPOOL
                          for(som_i in seq_len(nsompool)){
                            for(sv_sompool_i in seq_along(streamed_vars_sompool)){ 
                              current_stream <- streamed_vars_sompool[sv_sompool_i] 
                              
                              current_stream_type  <- find_stream_type("Sompool", current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
                              current_stream_specs <- find_stream_size(current_stream_type, guessh_in, LPJ_GUESS_TYPES, LPJ_GUESS_CONST_INTS)
                              
                              if(current_stream_specs$single){
                                Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][["Soil"]][["Sompool"]][[current_stream_type$name]][[som_i]] <- readBin(con  = zz,
                                                                                                                                                          what = current_stream_specs$what, 
                                                                                                                                                          n    = current_stream_specs$n, 
                                                                                                                                                          size = current_stream_specs$size)
                              }else{
                                PEcAn.logger::logger.severe("Historic under sompool.") # Not expecting any
                              }
                            }
                          }
                          
                        }else{
                          current_stream_specs <- find_stream_size(current_stream_type, guessh_in, LPJ_GUESS_TYPES, LPJ_GUESS_CONST_INTS)
                          # and read!
                          if(current_stream_specs$single){ # maybe use current_stream in sublist names to find correct place
                            Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][[length( Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]])]][[current_stream_type$name]][[pft_i]] <- readBin(con  = zz, 
                                                                                                                                                                                              what = current_stream_specs$what, 
                                                                                                                                                                                              n    = current_stream_specs$n, 
                                                                                                                                                                                              size = current_stream_specs$size)
                          }else{ # only for historic type?
                            for(css.i in seq_along(current_stream_specs$what)){ # maybe use current_stream in sublist names to find correct place
                              Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][[length( Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]])]][[current_stream_specs$names[css.i]]]<- readBin(con  = zz, 
                                                                                                                                                                                               what = current_stream_specs$what[css.i], 
                                                                                                                                                                                               n    = current_stream_specs$n[css.i], 
                                                                                                                                                                                               size = current_stream_specs$size[css.i])
                            }
                          }
                        }
                      } # streamed_vars-loop ends
                    } # pft-loop ends
                  }
                  
                  
                }else{
                  # NOT CLASS
                  current_stream_specs <- find_stream_size(current_stream_type, guessh_in, LPJ_GUESS_TYPES, LPJ_GUESS_CONST_INTS)
                  # and read!
                  if(current_stream_specs$single){
                    
                    Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][[current_stream_type$name]] <- readBin(con  = zz, 
                                                                                                              what = current_stream_specs$what, 
                                                                                                              n    = current_stream_specs$n, 
                                                                                                              size = current_stream_specs$size)
                  }else{ # probably don't need this but let's keep
                    for(css_i in seq_along(current_stream_specs$what)){
                      # CHANGE ALL THESE HISTORIC TYPES SO THAT cirrent_index and full goes together with the variable
                      Gridcell[["Stand"]][[stnd_i]][["Patch"]][[ptch_i]][[current_stream_specs$names[css_i]]] <- readBin(con  = zz, 
                                                                                                                         what = current_stream_specs$what[css_i], 
                                                                                                                         n    = current_stream_specs$n[css_i], 
                                                                                                                         size = current_stream_specs$size[css_i])
                    }
                  }
                }# end if-class within Patch
              }
            }
            
          }else{
            # NOT PATCH
            
            if(tools::toTitleCase(current_stream) %in% LPJ_GUESS_CLASSES && current_stream != "landcover"){
              current_stream_type <- find_stream_type(NULL, current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
            }else{
              current_stream_type <- find_stream_type("Stand", current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
            }
            
            Gridcell[["Stand"]][[stnd_i]][[length(Gridcell[["Stand"]][[stnd_i]])+1]] <- list()
            names(Gridcell[["Stand"]][[stnd_i]])[length(Gridcell[["Stand"]][[stnd_i]])] <- current_stream_type$name
            
            if(current_stream_type$type == "class"){
              
              # CLASS
              class_name <- current_stream_type$name
              
              beg_end <- serialize_starts_ends(file_in = guesscpp_in, 
                                               pattern = paste0("void ",
                                                                tools::toTitleCase(current_stream_type$name), 
                                                                "::serialize"))
              streamed_vars <- find_stream_var(file_in = guesscpp_in, line_nos = beg_end)
              num_pft <- ifelse(grepl("pft", current_stream_type$name, fixed = TRUE), n_pft, 1)
              
              for(varname in streamed_vars){
                Gridcell[["Stand"]][[stnd_i]][[current_stream_type$name]][[varname]] <- varname
                Gridcell[["Stand"]][[stnd_i]][[current_stream_type$name]][[varname]] <- vector("list", num_pft) 
              }
              
              for(pft_i in seq_len(num_pft)){
                for(sv_i in seq_along(streamed_vars)){
                  current_stream <- streamed_vars[sv_i] #it's OK to overwrite
                  current_stream_type <- find_stream_type(class_name, current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
                  
                  if(current_stream_type$type == "class"){
                    
                    # CLASS, NOT EVER GOING HERE?
                    class_name <- current_stream_type$name
                    
                  }else{
                    current_stream_specs <- find_stream_size(current_stream_type, guessh_in, LPJ_GUESS_TYPES, LPJ_GUESS_CONST_INTS)
                    # and read!
                    if(current_stream_specs$single){
                      Gridcell[["Stand"]][[stnd_i]][[length(Gridcell[["Stand"]][[stnd_i]])]][[current_stream_type$name]][[pft_i]] <- readBin(con  = zz, 
                                                                                                                                             what = current_stream_specs$what, 
                                                                                                                                             n    = current_stream_specs$n, 
                                                                                                                                             size = current_stream_specs$size)
                    }else{
                      for(css.i in seq_along(current_stream_specs$what)){
                        Gridcell[[length(Gridcell)]][[current_stream_type$name]][[pft_i]][[current_stream_specs$names[css.i]]]<- readBin(con  = zz, 
                                                                                                                                         what = current_stream_specs$what[css.i], 
                                                                                                                                         n    = current_stream_specs$n[css.i], 
                                                                                                                                         size = current_stream_specs$size[css.i])
                      }
                    }
                  }
                } # streamed_vars-loop ends
              } # pft-loop ends
              
            }else{
              # NOT CLASS
              current_stream_specs <- find_stream_size(current_stream_type, guessh_in, LPJ_GUESS_TYPES, LPJ_GUESS_CONST_INTS)
              # and read!
              if(current_stream_specs$single){
                Gridcell[["Stand"]][[stnd_i]][[current_stream_type$name]] <- readBin(con  = zz, 
                                                                                     what = current_stream_specs$what, 
                                                                                     n    = current_stream_specs$n, 
                                                                                     size = current_stream_specs$size)
              }else{ # probably don't need this but let's keep
                for(css_i in seq_along(current_stream_specs$what)){
                  Gridcell[[length(Gridcell)]][[current_stream_specs$names[css_i]]] <- readBin(con  = zz, 
                                                                                               what = current_stream_specs$what[css_i], 
                                                                                               n    = current_stream_specs$n[css_i], 
                                                                                               size = current_stream_specs$size[css_i])
                }
              }
            }# end if-class within Stand
          } # end patch-if 
          
          
        }# end for-loop over the streamed stand vars (svs_i, L.165)
      }# end for-loop over the stands (stnd_i, L.164)
      
    }else{ #not reading in Stand variables
      
      # NOT STAND
      
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
        num_pft <- ifelse(grepl("pft", current_stream_type$name, fixed = TRUE), n_pft, 1)
        
        for(varname in streamed_vars){
          Gridcell[[length(Gridcell)]][[varname]] <- varname
          Gridcell[[length(Gridcell)]][[varname]] <- vector("list", num_pft) 
        }
        
        for(pft_i in seq_len(num_pft)){
          for(sv_i in seq_along(streamed_vars)){
            current_stream <- streamed_vars[sv_i] #it's OK to overwrite
            current_stream_type <- find_stream_type(class_name, current_stream, LPJ_GUESS_CLASSES, LPJ_GUESS_TYPES, guessh_in)
            
            current_stream_specs <- find_stream_size(current_stream_type, guessh_in, LPJ_GUESS_TYPES, LPJ_GUESS_CONST_INTS)
            # and read!
            if(current_stream_specs$single){
              Gridcell[[length(Gridcell)]][[current_stream_type$name]][[pft_i]] <- readBin(con  = zz, 
                                                                                           what = current_stream_specs$what, 
                                                                                           n    = current_stream_specs$n, 
                                                                                           size = current_stream_specs$size)
            }else if(current_stream_specs$name %in% c("hmtemp_20", "hmprec_20", "hmeet_20")){
              # these three are just too different, maybe extract their names in the beginning
              # be careful while writing back to the binary
              # Gridcell[[length(Gridcell)]][[current_stream_type$name]] <- readBin(con = zz, double(), 264, 8)
              Gridcell[[length(Gridcell)]][[current_stream_type$name]] <- vector("list", length(current_stream_specs) - 2)
              for(css.i in seq_len(length(current_stream_specs) - 2)){
                 Gridcell[[length(Gridcell)]][[current_stream_type$name]][[css.i]] <- readBin(con  = zz, 
                                                                 what = current_stream_specs[[css.i]]$what, 
                                                                 n    = current_stream_specs[[css.i]]$n, 
                                                                 size = current_stream_specs[[css.i]]$size)
              }
            }else{
              for(css.i in seq_along(current_stream_specs$what)){
                Gridcell[[length(Gridcell)]][[current_stream_type$name]][[pft_i]][[current_stream_specs$names[css.i]]]<- readBin(con  = zz, 
                                                                                                                                 what = current_stream_specs$what[css.i], 
                                                                                                                                 n    = current_stream_specs$n[css.i], 
                                                                                                                                 size = current_stream_specs$size[css.i])
              }
            }
            
          } # streamed_vars-loop ends
        } # pft-loop ends
        
      }else{
        # NOT CLASS
        current_stream_specs <- find_stream_size(current_stream_type, guessh_in, LPJ_GUESS_TYPES, LPJ_GUESS_CONST_INTS)
        # and read!
        if(current_stream_specs$single){
          Gridcell[[length(Gridcell)]][[current_stream_type$name]] <- readBin(con  = zz, 
                                                                              what = current_stream_specs$what, 
                                                                              n    = current_stream_specs$n, 
                                                                              size = current_stream_specs$size)
        }else{ # probably don't need this but let's keep
          for(css_i in seq_along(current_stream_specs$what)){
            Gridcell[[length(Gridcell)]][[current_stream_specs$names[css_i]]] <- readBin(con  = zz, 
                                                                                         what = current_stream_specs$what[css_i], 
                                                                                         n    = current_stream_specs$n[css_i], 
                                                                                         size = current_stream_specs$size[css_i])
          }
        }
      }# end if-class within Gridcell
      
    } # Stand if-else ends
  } # Gridcell-loop ends
  
  close(zz)
  
  Gridcell$meta_data <- meta_data
  
  return(Gridcell)
} # read_binary_LPJGUESS end






