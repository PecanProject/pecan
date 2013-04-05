compare.summary <- function(ref,model,rname=c("ref"),mname=c("model"),normalize=FALSE,iflog=T,quantile=c(0.05,0.95),fold=c(7)){
  load(PEcAn.qaqc)
  ref <- na.omit(ref)
  model <- na.omit(model)
  len <- min(length(ref),length(model))
  quantil <- seq(1/(len+1),1-1/(len+1),by=1/(len+1))
  ref <- ref[round(quantil*length(ref),0)]
  model <- model[round(quantil*length(model),0)]
  if (is.list(ref)) 
    ref <- unlist(ref)
  if (is.list(model)) 
    model <- unlist(model)
  sd.r <- sd(na.omit(ref))
  sd.f <- sd(na.omit(model))
  if (normalize) {
    sd.f <- sd.f/sd.r
    sd.r <- 1
  }
  R <-sum((ref-mean(ref))*(model-mean(model)))/len/sd.f/sd.r
  E <-((sum((model-ref)^2))/len)^(1/2)
  S <- (2 * sqrt(1 + R))/(sd.f + (1/sd.f))^2
  measures <-c(sd.f,sd.r,R,E,S,overlap.cdf(ref,model,iflog,quantile),overlap.cdf.uncert(ref,model,iflog,quantile,fold))
  result <-as.data.frame(measures,row.names=c("sd.f","sd.r","R","E","S","overlap","sd.overlap"))
  taylor.diagram(ref,model,normalize=normalize)
  RR <- cor(ref, model, use = "pairwise")
  text(sd.r, 0, labels=rname,pos=3)
  text(sd.f * RR, sd.f * sin(acos(RR)), labels=mname,pos=3)
  return(result)
}