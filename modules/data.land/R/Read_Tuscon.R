
## tree core QAQC
Clean_Tucson <- function(file){
  lines = scan(file,character(),sep="\n")
  split = strsplit(lines," ")
  tags   = NULL
  decade = NULL
  for(i in 1:length(split)){
    tags[i]   = split[[i]][1]
    decade[i] = split[[i]][2]
  }
  utags = unique(tags)
  newfile = paste(file,".COR.txt",sep="")
  if(file.exists(newfile)) file.remove(newfile)
  for(tag in utags){
    rows = rev(which(tags == tag))
    keep = 1
    for(i in 1:length(rows)){
      if(rows[i] - rows[keep] >= -1){
        keep = i
      } else {
        break
      }
    }
    keep = min(keep,length(rows))
    rows = rev(rows[1:keep])
    append = file.exists(newfile)
    write(lines[rows],newfile,append=append)
  }
  return(newfile)
}

Read_Tuscon <- function(folder){
  
  require(dplR)
  
  filenames <- dir(folder,pattern =  "TXT",full.names=TRUE)
  filenames <- c(filenames,dir(folder,pattern =  "rwl",full.names=TRUE))
  corrected = grep(pattern="COR.txt",x=filenames)
  if(length(corrected)>0){
    filenames = filenames[-corrected]
  }
  filedata <- list()
  for (file in filenames){
    file = Clean_Tucson(file)
    filedata[[file]] <- read.tucson(file, header = FALSE)
  }
  
  return(filedata)
}

