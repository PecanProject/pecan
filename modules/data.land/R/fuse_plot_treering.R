#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
fuse_plot_treering <- function(plot.data,inc.data,tuscon.data,inc.unit.conv = 0.1){

  plot.data <- as.data.frame(plot.data)
  
  ## separate veg data to lists by plot
  plot.id <- unique(plot.data$plot)
  diameters <- list()
  spp <- list()
  depth <- list()
  for(i in 1:length(plot.id)){

    mch = which(plot.data$plot == plot.id[i]) 
    diameters[[i]] <- plot.data$dbh[mch]
    names(diameters[[i]]) <- plot.data$tree[mch]
    spp[[i]] <- plot.data$spp[mch]
        
  }
  mplot = length(plot.id)
    
  ## match increment data to plot and tree
  ## note: much of this parsing is currently specific to Sam's data
  increments <- list()
  inc.names <- sub('T','',sub('.txt','',names(inc.data),fixed=TRUE))
  inc.ID <- as.numeric(substr(inc.names,1,nchar(inc.names)-1))
  inc.rep <- substr(inc.names,nchar(inc.names),nchar(inc.names))
  inc.length <- sapply(inc.data,length)
  nyr <- max(inc.length,na.rm=TRUE)-1

  tuscon <- list()
  T.names <- sub('T','',sub('.TXT','',basename(names(tuscon.data)),fixed=TRUE))
  T.ID <- as.numeric(substr(T.names,1,nchar(T.names)-1))
  T.rep <- substr(T.names,nchar(T.names),nchar(T.names))
  T.length <- sapply(tuscon.data,nrow)
  nyr <- max(c(nyr,T.length),na.rm=TRUE)
  
  survival <- list()
  for(i in 1:mplot){
    if(length(diameters[[i]]) == 0) next
    survival[[i]] <- matrix(TRUE,length(diameters[[i]]),nyr+1)
  }
  
  for(i in 1:length(plot.id)){ ## loop over plots
    ntree = length(diameters[[i]])
    increments[[i]] <- matrix(NA,ntree,nyr)
    depth[[i]] <- rep(NA,ntree)
    for(j in 1:ntree){  ## loop over trees
      ## look for next tree in list
      tree.id <- as.numeric(names(diameters[[i]])[j])
      mch = which(inc.ID == tree.id)
      mchT = which(T.ID == tree.id)
      if(length(mch) > 0){  ## match to a velmex record
        if(length(mch) == 1){
          ## only one record, use it.
          y = inc.data[[mch]] 
          maxy = max(y)
          growth = diff(y)          
          increments[[i]][j,nyr:(nyr-length(growth)+1)] <- growth*inc.unit.conv
        } else{          
           ## create mean increment record (eventually shift this to BAI eliptoid)
           growth =  matrix(NA,length(mch),nyr)
           maxy = NULL
           for(k in 1:length(mch)){
             maxy = max(maxy,inc.data[[mch[k]]])
             growth[k,1:(inc.length[mch[k]]-1)] <- diff(inc.data[[mch[k]]])
           }
           growth <- apply(growth,2,mean,na.rm=TRUE)
           growth[is.nan(growth)] <- NA
           increments[[i]][j,] <- rev(growth)*inc.unit.conv
        }
      
        ## did core get to pith??
        radius = diameters[[i]][j]/2
        depth[[i]][j] = maxy*inc.unit.conv
        if((radius*0.95 - 2) < depth[[i]][j]){ ## if you're within 5% of the diameter, assume hit middle
          survival[[i]][j,which(is.na(increments[[i]][j,]))] <- FALSE   
        }
      
      } ## end mch > 0
      
      if(length(mchT)>0){

        if(length(mchT) == 1){
          ## only one record, use it.
          growth = t(tuscon.data[[mchT]])*0.1
          maxy = sum(growth)
          increments[[i]][j,(nyr-length(growth)+1):nyr] <- growth
        } else{          
          ## create mean increment record (eventually shift this to BAI eliptoid)
          growth =  matrix(NA,length(mchT),nyr)
          for(k in 1:length(mchT)){
            g <- t(tuscon.data[[mchT[k]]])*0.1
            growth[k,(nyr-length(g)+1):nyr] <- g
          }
          maxy = max(apply(growth,1,sum,na.rm=TRUE))
          growth <- apply(growth,2,mean,na.rm=TRUE)
          growth[is.nan(growth)] <- NA
          increments[[i]][j,] <- growth
        }
        
        ## did core get to pith??
        radius = diameters[[i]][j]/2
        depth[[i]][j] = maxy
        if((radius*0.95 - 2) < depth[[i]][j]){ ## if you're within 5% of the diameter, assume hit middle
          survival[[i]][j,which(is.na(increments[[i]][j,]))] <- FALSE   
        }
        
        
        
      }
      
    }  ## end loop over trees
  } ## end loop over plots
  
  ## build diameter increment matrix
  
  ## expand diameter data matrix
   for(i in 1:length(diameters)){
     dtmp <- matrix(NA,length(diameters[[i]]),nyr+1)
     dnames <- names(diameters[[i]])
     dtmp[,nyr+1] <- diameters[[i]]
     diameters[[i]] <- dtmp
     colnames(diameters[[i]]) <- 2012 - (nyr+1):1 + 1
     row.names(diameters[[i]])<- dnames
   }

  return(list(diameters=diameters,increments=increments,
              survival=survival,species = spp,depth=depth))
}
