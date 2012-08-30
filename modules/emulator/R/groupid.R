`groupid` <-
function(x){

  if(is.null(ncol(x))){## | ncol(x) == 1){
    return(1:length(x))
  }
  
  n <- nrow(x)
  v <- rep(NA,n)
  j <- 1
  for(i in 1:n){
    if(is.na(v[i])){
      v[i] <- j
      if(i < n){
        for(k in (i+1):n){
          if(!any(x[i,] != x[k,])){
            v[k] <- j
          }
        }
      }
      j <- j+1
    }
  }
  v
}

