

modelid <- c("SIPNET","ED")
runnum <- c(2,2)
yrid <- c(2002,2003,2004,2005)
var <- "NPP"  # specify the var that is interested in
compare.model <- function(var,modelid,runnum,yrid,siteid,){
  # read in the needed data
  testdata <- NULL
  for (yr in yrid)
  {SIPNET1 <- read.output(run.id=1, outdir='../output/PEcAn_1/out/1',
                        start.year=yr, end.year=yr,
                        variables=var,
                        model="SIPNET")
   SIPNET2 <- list(data2=data1[[1]]+rnorm(length(data1[[1]]),0,sd(data1[[1]])/rnorm(1,5,1)))
   ED1 <- list(data2=data1[[1]]+rnorm(length(data1[[1]]),0,sd(data1[[1]])/rnorm(1,5,1)))
   ED2 <- list(data2=data1[[1]]+rnorm(length(data1[[1]]),0,sd(data1[[1]])/rnorm(1,5,1)))
   obst <- list(obs=data1[[1]]+rnorm(length(data1[[1]]),0,sd(data1[[1]])/rnorm(1,5,1)))
   testdatat <- data.frame(site=rep(1,length(data1[[1]])),
                           date=rep(yr,length(data1[[1]])),
                           obs=obst[[1]],
                           SIPNET1=data1[[1]],
                           SIPNET2=data2[[1]],
                           ED1=ED1[[1]],
                           ED2=ED2[[1]])
   testdata <- rbind(testdata,testdatat)
  }

}

