#install what we need
lapply(c('dplyr',
         'shinyWidgets',
         'CodeDepends',
         'visNetwork',
         'ggplot2',
         'XML',
         'shinydashboard'),function(pkg){
    if (!(pkg %in% installed.packages()[,1])){
          install.packages(pkg)
          }
           library(pkg,character.only = TRUE,quietly = TRUE)
    }
)

if (!('DependenciesGraphs' %in% installed.packages()[,1])) devtools::install_github("datastorm-open/DependenciesGraphs")


curentd2 <<- NULL
curentd1 <<- NULL
curentd3 <<- NULL
#------------- My dependen - I added group to this
my.allDepFunction<-function (envir, name.functions) 
{
  envir2 <- paste0("package:", envir)
  toutfonc <- linksForOne(envir2, name.functions)
  link <- toutfonc
  functions.list <- unique(as.character(unlist(c(toutfonc))))
  Visdata <- list()
  Nomfun <- functions.list
  #--- source env
  envs.found<-lapply(functions.list,find)
  #-taking out the my own all functions packge
  lapply(envs.found, function(ll){
    ll[which(!(ll%in%c("package:Pecan.functions")))][1]->tmp
    if (length(tmp)!=0){
      return((strsplit(tmp,":")[[1]])[2])
    }else{
      return("Pecan.functions")
    }
    
  })%>%unlist()->envs.fi
  Nomfun <- data.frame(cbind(id = 1:length(Nomfun), label = Nomfun,group=envs.fi))
  
  if (!is.null(link)) {
    fromto <- matrix(0, ncol = dim(link)[2], nrow = dim(link)[1])
    if (length(fromto) > 0) {
      for (i in 1:dim(link)[1]) {
        fromto[i, 1] <- which(as.character(link[i, 2]) == 
                                Nomfun[, 2])
        fromto[i, 2] <- which(as.character(link[i, 1]) == 
                                Nomfun[, 2])
        if (dim(link)[2] > 2) {
          fromto[i, 3:length(link[i, ])] <- link[i, 3:length(link[i, 
                                                                  ])]
        }
      }
    }
  }
  else {
    fromto <- cbind(0, 0)
  }
  fromto <- data.frame(fromto)
  names(fromto) <- c("from", "to")
  Visdata$Nomfun <- Nomfun
  Visdata$fromto <- fromto
  class(Visdata) <- "dependenciesGraphs"
  return(Visdata)
}

