## code that loops over a whole set of Tuscon files
Read_Tuscon <- function(folder){

  library(dplR)
  
  filenames <- dir(folder,pattern =  "TXT",full.names=TRUE)
  filenames <- c(filenames,dir(folder,pattern =  "rwl",full.names=TRUE))
  filedata <- list()
  for (file in filenames){
    filedata[[file]] <- read.tucson(file, header = FALSE)
  }

  return(filedata)
  
}
