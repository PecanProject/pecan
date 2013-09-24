fuse_plot_treering <- function(plot.data,tuscon.data){
   
#  plot.data <- as.data.frame(plot.data)

## prep veg data
sites <- unique(plot.data$site)
plot.ids <- unique(plot.data$plot)
diameters <- plot.data$dbh         
spp <- plot.data$spp
mplot = length(plot.ids)
  ntrees = length(spp)
  depth <- rep(NA,ntrees)
  
 #match increment data to census data
 #note: much of this parsing is currently specific to Sam's data
  
#inc.names <- sub('T','',sub('.txt','',names(inc.data),fixed=TRUE))
#inc.ID <- as.numeric(substr(inc.names,1,nchar(inc.names)-1))
#inc.rep <- substr(inc.names,nchar(inc.names),nchar(inc.names))
#inc.length <- sapply(inc.data,length)
#nyr <- max(inc.length,na.rm=TRUE)-1

tuscon <- list()
T.names <- lapply(tuscon.data,names)#sub('T','',sub('.TXT','',basename(names(tuscon.data)),fixed=TRUE))
#T.rep <- substr(T.names,nchar(T.names),nchar(T.names))
T.length <- sapply(tuscon.data,nrow)
T.trees <- sapply(tuscon.data,ncol)
T.minYr <- sapply(tuscon.data,function(x){min(as.numeric(rownames(x)))})
T.maxYr <- sapply(tuscon.data,function(x){max(as.numeric(rownames(x)))})
nyr = max(T.maxYr)-min(T.minYr)+1
#nyr <- max(T.length,na.rm=TRUE) # was: nyr <- max(c(nyr,T.length),na.rm=TRUE)
  
  increments <- as.data.frame(matrix(NA,sum(T.trees),nyr))
  colnames(increments) = min(T.minYr):max(T.maxYr)
  
  count = 0
  for(i in 1:length(tuscon.data)){
    inc = t(tuscon.data[[i]])*0.1  ##transpose and convert to cm
    rowsel = which(!(rownames(inc) %in% rownames(increments))) ## will drop any duplicate tags
    if(length(rowsel)==0) next
    #rowsel = duplicated((rownames(inc) %in% rownames(increments)))
    mch = which(colnames(increments) %in% colnames(inc)) 
    increments[1:length(rowsel)+count,mch] <- inc[rowsel,]
    rownames(increments)[1:length(rowsel)+count] <- rownames(inc)[rowsel]
    count = count + nrow(inc)
  }
  
  mch = match(plot.data$tree,rownames(increments))
  wmch = which(!is.na(mch))
  incrementMatch = as.data.frame(matrix(NA,ntrees,nyr))
  incrementMatch[wmch,] = increments[mch[wmch],]
#  row.names(incrementMatch) = as.character(plot.data$tree) # = rownames(increments)[mch[wmch]]
  colnames(incrementMatch) = colnames(increments)
  increments = incrementMatch
  
## this code will run into trouble with multi stem trees that were cored (will assign one core to both) or recores
## need to augment ID's with replicate info
  
#T.ID <- as.character(substr(T.names,1,nchar(T.names)-1)) #was as.numeric
  
  
  
#survival <- list()
#for(i in 1:mplot){
#  if(length(diameters[[i]]) == 0) next
#  survival[[i]] <- matrix(TRUE,length(diameters[[i]]),nyr+1)
#}
  survival <- matrix(TRUE,nrow(diameters),nyr+1)
  
## need to apply PLOT data to recent survival

## need to apply FLAGGED pith records

## APPROXIMATION: did core get to pith??
radius = apply(diameters,1,max,na.rm=TRUE)/2
depth = apply(increments,1,sum,na.rm=TRUE); depth[depth==0]=NA
for(j in which(!is.na(depth))){
  if((radius[j]*0.95 - 2) < depth[j]){ ## if you're within 5% of the diameter, assume hit middle
    survival[j,which(is.na(increments[j,]))] <- FALSE   ## need to only apply to EARLY period
  }
}

if(FALSE){

for(i in 1:length(plot.id)){ ## loop over plots
  ntree = length(diameters[[i]])
  increments[[i]] <- matrix(NA,ntree,nyr)
  depth[[i]] <- rep(NA,ntree)
  for(j in 1:ntree){  ## loop over trees
    ## look for next tree in list
    tree.id <- as.character(names(diameters[[i]])[j])
    #mch = which(inc.ID == tree.id)
    mchT = which(T.ID == tree.id)
    #if(length(mch) > 0){  ## match to a velmex record
     # if(length(mch) == 1){
      #  ## only one record, use it.
       # y = inc.data[[mch]] 
        #maxy = max(y)
        #growth = diff(y)          
        #increments[[i]][j,nyr:(nyr-length(growth)+1)] <- growth*inc.unit.conv
    #  } else{          
     #   create mean increment record (eventually shift this to BAI eliptoid)
      #  growth =  matrix(NA,length(mch),nyr)
       # maxy = NULL
      #  for(k in 1:length(mch)){
       #   maxy = max(maxy,inc.data[[mch[k]]])
        #  growth[k,1:(inc.length[mch[k]]-1)] <- diff(inc.data[[mch[k]]])
      #  }
       # growth <- apply(growth,2,mean,na.rm=TRUE)
      #  growth[is.nan(growth)] <- NA
       # increments[[i]][j,] <- rev(growth)*inc.unit.conv
      #}
      
      ## did core get to pith??
    #  radius = diameters[[i]][j]/2
     # depth[[i]][j] = maxy*inc.unit.conv
    #  if((radius*0.95 - 2) < depth[[i]][j]){ ## if you're within 5% of the diameter, assume hit middle
     #   survival[[i]][j,which(is.na(increments[[i]][j,]))] <- FALSE   
    #  }
      
  #  } ## end mch > 0
    
    if(length(mchT)>0){ ## match to a tuscon record
      
      ##this needs generalization if all trees not cored in same (most recent) year
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

}  ## end FALSE of old code

## build diameter increment matrix

## expand diameter data matrix
#for(i in 1:length(diameters)){
  dtmp <- matrix(NA,nrow(diameters),nyr+1)
  colnames(dtmp) = (min(T.minYr)-1):max(T.maxYr)
  dnames <- names(diameters)
  mch = match(colnames(dtmp),dnames)
  for(i in which(!is.na(mch))){
    dtmp[,i] <- diameters[,mch[i]]
  }
  diameters <- dtmp
#}

dat<<-list(diameters=diameters, increments = increments, survival = survival,
            species = spp,depth=depth) # before species was "survival = survival"
  #return(dat)
}
