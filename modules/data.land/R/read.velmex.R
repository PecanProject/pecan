## read velmex non-tuscon files

read.velmex <- function(folder){

  filenames <- dir(folder,pattern =  ".txt$",full.names=TRUE)
  filedata <- list()
  for (file in filenames){
    dat <- read.fwf(file, c(1,12,3,128), header = FALSE)
    notes <- which(!is.na(dat[,4]))
    if(length(notes) == 0) notes = nrow(dat)+1
    filedata[[basename(file)]] <- c(0,dat[1:(notes[1]-1),2])    
  }
  #names(filedata) <- basename(filenames)
  
  return(filedata)

  
}
