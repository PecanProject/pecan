
Read_WinDendro <- function(folder){

  library(dplR)
  
  filenames <- dir(folder,pattern =  "TXT",full.names=TRUE)
  filenames <- c(filenames,dir(folder,pattern =  "rwl",full.names=TRUE))
  filedata <- list()
  for (file in filenames){
    filedata[[file]] <- read.rwl(file, format="auto")
  }

  dendro.data<<-filedata
  return(dendro.data)
}
