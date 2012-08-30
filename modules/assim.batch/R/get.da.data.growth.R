## Carl Davidson code for dealing with height growth data for emulator-based DA
## ported by M. Dietze 08/30/12
## some of this is redundant with other parts of PEcAn and needs to be cleaned up 


#library(hdf5)
#source('./code/R/edview.base.R')
#source('./code/R/utils.R')
#source('./code/R/model.specific.R')

get.height <- function(growth, monthly, j, year.i0){
  monthly$HITE[year.i0][growth$plot[j]       == monthly$PATCH_NAME[year.i0] & 
                        growth$individual[j] == monthly$COHORT_NAME[year.i0] &
                        monthly$HITE[year.i0] > 0.01] 
                        #ignore new cohorts by matching against hgt_min 
}

#LIKELIHOOD
calculate.growth.L <- function(yearly, growth, error, years) { 
  
  year.names <- as.character(years)
  year.ranges <- c(0, cumsum(yearly$NCOHORTS_GLOBAL))
  years.i<-lapply(seq(years),
                 function(i) {year.ranges[i]:year.ranges[i+1]})
  names(years.i)<-year.names
  
  cumLogL <- 0
  squares <- c()
  
  for(i in seq(years)[-1]){
    year<-year.names[i]
    year.i1 <- years.i[[i]]
    year.i0 <- years.i[[i-1]]
    for(j in 1:nrow(growth)){
      observed.growth <- growth[j,year]
      #print(observed.growth)
      if(!is.na(observed.growth)){
        height1 <- get.height(growth, yearly, j, year.i1)[1]
        height0 <- get.height(growth, yearly, j, year.i0)[1]
        if(length(height0) != length(height1)) warnings('0 growth')
        modeled.growth <- height1 - height0
        
        if(length(modeled.growth) > 0 && !is.na(modeled.growth)){
          #print(growth$pft[j])
          #print(paste('diff ', (modeled.growth - observed.growth)))
          if(any(modeled.growth < 0)) warnings('negative growth')
          #squares <- c(squares, (modeled.growth - observed.growth)^2)
          logL <- dnorm((observed.growth), (modeled.growth), error, log=TRUE)
          if(any(is.infinite(logL))) stop('Infinite likelihood') #AKA really large value
          #else print(paste(growth$pft[j], 'logL', logL))
          cumLogL <- cumLogL - sum(logL, na.rm = TRUE)
          if(is.infinite(cumLogL)) stop('Infinite likelihood')
        }
      }
    }
  }
  #return(squares)
  print(paste('cumLogL', cumLogL))
  return(cumLogL)
}

get.da.data.growth <- function(){

  out.dir<-'./pecan/Toolik/growth/'

  buds <- read.csv('./toolik/validation/Survey/ToolikVegSurvey.csv', sep='\t')
  buds<-buds[!is.na(buds$length),]
  buds<-buds[buds$pft != 'graminoid',]
  heights<-buds[,c('length', paste('X', 2010:2003, sep=''))]
  heights<-as.matrix(heights[!is.na(heights$length),])/1000
  growth <-do.call(rbind, 
    lapply(1:nrow(heights), 
        function(i) -(diff(as.numeric(heights[i,])))))
  colnames(growth)<-2011:2004
  growth <-cbind(buds[,c('plot', 'individual', 'pft')], growth)

  ensemble.size <- 500
  load(paste(out.dir, 'samples.Rdata', sep=''))

  pfts<-names(ensemble.samples)
  pfts<-pfts[pfts != 'env']

  #ENSEMBLE
  omitted <- c(87)
  ensemble.run.ids <- get.run.id('ENS', left.pad.zeros((1:ensemble.size)[-omitted]))
  ensemble.x <- do.call(cbind, ensemble.samples[pfts])[(1:ensemble.size)[-omitted],]

  #SENSITIVITY ANALYSIS
  sa.x <- list()
  for(pft in pfts){
    MEDIAN <- '50'
  
    median.samples <- list()
    for(i in 1:length(sa.samples)){
      median.samples[[i]] <- sa.samples[[i]][MEDIAN,]
    }
    names(median.samples) = names(sa.samples)
    run.id <- get.run.id('SA', 'median')
    sa.x[[run.id]] <- do.call(cbind, trait.samples)
    ## loop over pfts
    for(i in seq(names(sa.samples))){
    
      traits <- colnames(sa.samples[[i]])
      quantiles.str <- rownames(sa.samples[[i]])
    
      ## loop over variables
      for (trait in traits) {
        for(quantile.str in quantiles.str) {
          if (quantile.str != MEDIAN) {
            quantile <- as.numeric(quantile.str)/100
            trait.samples <- median.samples
            trait.samples[[i]][trait] <- sa.samples[[i]][quantile.str, trait]
            run.id <- get.run.id('SA', round(quantile,3), trait=trait, pft.name=names(trait.samples)[i])
            sa.x[[run.id]] <- do.call(cbind, trait.samples)
          }
        }
      }
    }
  }
  sa.x <- do.call(rbind, sa.x)
  sa.run.ids <- rownames(sa.x)
  run.ids <- ensemble.run.ids  #c(ensemble.run.ids, sa.run.ids)
  x <- ensemble.x              #rbind(ensemble.x, sa.x)


  #run.ids<-ensemble.run.ids
  #x <- ensemble.x
  y <- t(as.data.frame(lapply(run.ids, 
              function(run.id){ 
                print(run.id)
                yearly <-paste(run.id, '-E-(((', 
                                paste(paste('(', 2003:2010, ')', sep=''), collapse='|'), ')-01)|(2011-07))', 
                                sep='')
                yearly <- read.output.type(out.dir, 
                    outname=yearly, pattern='-E-')
                if(length(yearly) <= 0){
                  return(NA)
                }
                squares <- calculate.growth.L(yearly, growth, 0.023, years=2003:2011)
                #error param was calculated from stddev, see below
                return(squares)
              })))
  "squares<-lapply(run.ids, 
      function(run.id){ 
        print(run.id)
        yearly <-paste(run.id, '-E-(((', 
            paste(paste('(', 2003:2010, ')', sep=''), collapse='|'), ')-01)|(2011-07))', 
            sep='')
        yearly <- read.output.type(out.dir, 
            outname=yearly, pattern='-E-')
        if(length(yearly) <= 0){
          return(NA)
        }
        squares <- calculate.growth.L(yearly, growth, 0.0278, years=2003:2011)
        #observation process 
        return(squares)
      })"
  #stddev <- sqrt(mean(unlist(squares)))
  #print(stddev)

  #filter out runs that have not completed
  #log likelihoods default to 0
  x<-x[y!=0,]
  y<-y[y!=0]
  print(y)
  save(x, y, file=paste(out.dir, 'L.nee.Rdata', sep=''))
  print('save sucessful')
  warnings()
}
