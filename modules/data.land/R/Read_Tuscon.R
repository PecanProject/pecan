#' Clean_Tucson
#'
#' tree core QAQC
#'
#' @param file WinDendro output
#' @export
Clean_Tucson <- function(file) {
  lines <- scan(file, character(), sep = "\n")
  split <- strsplit(lines, " ")
  tags <- NULL
  decade <- NULL
  
  for (i in seq_along(split)) {
    tags[i]   <- split[[i]][1]
    decade[i] <- split[[i]][2]
  }
  utags <- unique(tags)
  newfile <- paste0(file, ".COR.txt")
  if (file.exists(newfile)) {
    file.remove(newfile)
  }
  
  for (tag in utags) {
    rows <- rev(which(tags == tag))
    keep <- 1
    for (i in seq_along(rows)) {
      if (rows[i] - rows[keep] >= -1) {
        keep <- i
      } else {
        break
      }
    }
    keep   <- min(keep, length(rows))
    rows   <- rev(rows[1:keep])
    append <- file.exists(newfile)
    write(lines[rows], newfile, append = append)
  }
  return(newfile)
} # Clean_Tucson

#' Read_Tucson
#'
#' wrapper around read.tucson that loads a whole directory of tree ring files 
#' and calls a 'clean' function that removes redundant records 
#' (WinDendro can sometimes create duplicate records when editing)
#'
#' @param folder path to read files from.
#'  Will read all files at this path matching "TXT", "rwl", or "rw"
#'
#' @export
Read_Tucson <- function(folder) {
  
  filenames <- dir(folder, pattern = "TXT", full.names = TRUE)
  filenames <- c(filenames, dir(folder, pattern = "rwl", full.names = TRUE))
  filenames <- c(filenames, dir(folder, pattern = "rw", full.names = TRUE))
  corrected <- grep(pattern = "COR.txt", x = filenames)
  if (length(corrected) > 0) {
    filenames <- filenames[-corrected]
  }
  filedata <- list()
  for (file in filenames) {
    file <- Clean_Tucson(file)
    filedata[[file]] <- dplR::read.tucson(file, header = FALSE)
  }
  
  return(filedata)
} # Read_Tucson

