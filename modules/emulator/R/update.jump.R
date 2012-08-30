`update.jump` <-
function(jmp,chain){
  ##check for valid typing
  if(is.null(jmp)) stop("jump is NULL")
  
  ##update counter
  cnt <- attr(jmp,"count") + 1;
  attr(jmp,"count") <- cnt;
  clen <- attr(jmp,"clen")
  
  ##update jump parm
  if(cnt %% clen == 0){    
    a <- max(arate(chain[(cnt-clen+1):cnt,]),1.0/clen)
    l <- length(attr(jmp,"history"))
    j <- attr(jmp,"history")[l]
    attr(jmp,"history")[l+1] <- j*a/attr(jmp,"target")
    attr(jmp,"arate")[l+1] <- a
  }
  jmp
}

`update.mvjump` <-
    function(jmp,chain){
  ##check for valid typing
  if(is.null(jmp)) stop("jump is NULL")
  
  ##update counter
  cnt <- attr(jmp,"count") + 1;
  attr(jmp,"count") <- cnt;
  clen <- attr(jmp,"clen")
  
  ##update jump parm
  if(cnt %% clen == 0){
    hnew <- rep(NA,ncol(chain))
    l <- nrow(attr(jmp,"history"))
    for(i in 1:ncol(chain)){
      #print(c(i,cnt,clen))
      a <- arate(chain[(cnt-clen+1):cnt,i])
      j <- attr(jmp,"history")[l,i]
      hnew[i] <- j*a/attr(jmp,"target")
    }
    print(hnew)
    attr(jmp,"history")<- rbind(attr(jmp,"history"),hnew)
    attr(jmp,"arate")[l+1] <- a
  }
  jmp
}
