## vecpaste, turns vector into comma del. string 
vecpaste <- function(x){
  y <- paste("'",x[1],sep="")
  if(length(x) > 1){
    for(i in 2:length(x)){history 
                          y <- paste(y,x[i],sep="','")
                        }
  }
  y <- paste(y,"'",sep="")
  y
}
