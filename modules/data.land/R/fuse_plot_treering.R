fuse_plot_treering <- function(plot.data,inc.data,inc.unit.conv = 0.1){

  plot.data <- as.data.frame(plot.data)
  
  ## separate veg data to lists by plot
   plot.id <- unique(plot.data$plot)
  diameters <- list()
  spp <- list()
  for(i in 1:length(plot.id)){

    mch = which(plot.data$plot == plot.id[i]) 
    diameters[[i]] <- plot.data$dbh[mch]
    names(diameters[[i]]) <- plot.data$tree[mch]
    spp[[i]] <- plot.data$spp[mch]
    
    
  }
  
  ## match increment data to plot and tree
  ## note: much of this parsing is currently specific to Sam's data
  increments <- list()
  inc.names <- sub('T','',sub('.txt','',names(inc.data),fixed=TRUE))
  inc.ID <- as.numeric(substr(inc.names,1,nchar(inc.names)-1))
  inc.rep <- substr(inc.names,nchar(inc.names),nchar(inc.names))
  inc.length <- sapply(inc.data,length)
  nyr <- max(inc.length,na.rm=TRUE)-1
  for(i in 1:length(plot.id)){ ## loop over plots
    ntree = length(diameters[[i]])
    increments[[i]] <- matrix(NA,ntree,nyr)
    for(j in 1:ntree){  ## loop over trees
      ## look for next tree in list
      tree.id <- as.numeric(names(diameters[[i]])[j])
      mch = which(inc.ID == tree.id)
      if(length(mch) > 0){
        if(length(mch) == 1){
          ## only one record, use it.
          growth = diff(inc.data[[mch]])
          increments[[i]][j,nyr:(nyr-length(growth)+1)] <- growth*inc.unit.conv
        } else{          
           ## create mean increment record (eventually shift this to BAI eliptoid)
           growth =  matrix(NA,length(mch),nyr)
           for(k in 1:length(mch)){
             growth[k,1:(inc.length[mch[k]]-1)] <- diff(inc.data[[mch[k]]])
           }
           growth <- apply(growth,2,mean,na.rm=TRUE)
           growth[is.nan(growth)] <- NA
           increments[[i]][j,] <- rev(growth)*inc.unit.conv

         }
      } ## end mch > 0
    }  ## end loop over trees
  } ## end loop over plots
  
  ## build diameter increment matrix
  
  ## expand diameter data matri
   for(i in 1:length(diameters)){
     dtmp <- matrix(NA,length(diameters[[i]]),nyr+1)
     dnames <- names(diameters[[i]])
     dtmp[,nyr+1] <- diameters[[i]]
     diameters[[i]] <- dtmp
     colnames(diameters[[i]]) <- 2012 - (nyr+1):1 + 1
     row.names(diameters[[i]])<- dnames
   }

  return(list(diameters=diameters,increments=increments,species = spp))
}
