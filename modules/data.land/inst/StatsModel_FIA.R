#This code reads in output of CanopyCover2NLCD.r and using the thresholds of NLCD legens to convert them to NLCD classes. 
library(rjags)

#Reading in files
PSS <- read.csv(file="/sitefiles.radius 0.075.lat 29.8848 lon -83.3542.pss.csv", head=TRUE, sep=",")

#Using NLCD thresholds to change PSS file to NLCD landcover.
#Since all MANDIFORE points have at least 50% forest cover, I do not check for minimum vegetation cover
landcover <- data.frame (matrix(ncol=4, nrow=length(PSS$X)))
colnames(landcover) = c("Deciduous_Forest", "Evergreen_Forest", "Mixed_Forest", "Woody_Wetlands")
rownames(landcover) = c(PSS$X)

 
for (i in 1:nrow(PSS)){    

  if (sum (PSS$X9[i], PSS$X10[i], PSS$X11[i], PSS$X18[i]) > 75){
    landcover$"Deciduous_Forest"[i] = 1
    landcover$"Evergreen_Forest"[i] = 0
    landcover$"Mixed_Forest"[i] = 0
    landcover$"Woody_Wetlands"[i] = 0    
  }
  if (sum (PSS$X6[i], PSS$X7[i], PSS$X8[i], PSS$X20[i]) > 75){
    landcover$"Deciduous_Forest"[i] = 0
    landcover$"Evergreen_Forest"[i] = 1
    landcover$"Mixed_Forest"[i] = 0
    landcover$"Woody_Wetlands"[i] = 0    
  }
  if ((sum (PSS$X9[i], PSS$X10[i], PSS$X11[i], PSS$X18[i]) < 75) & (sum (PSS$X6[i], PSS$X7[i], PSS$X8[i], PSS$X20[i]) < 75)){
    landcover$"Deciduous_Forest"[i] = 0
    landcover$"Evergreen_Forest"[i] = 0
    landcover$"Mixed_Forest"[i] = 1
    landcover$"Woody_Wetlands"[i] = 0 
  }      
  if (PSS$X19[i] > 20){
    landcover$"Deciduous_Forest"[i] = 0
    landcover$"Evergreen_Forest"[i] = 0
    landcover$"Mixed_Forest"[i] = 0
    landcover$"Woody_Wetlands"[i] = 1 
  }
}

#Counting each cover class inside the whole grid
landcover["Total",] <- colSums(landcover)
X1 <- landcover[nrow(landcover),"Deciduous_Forest"]
X2 <- landcover[nrow(landcover),"Evergreen_Forest"]
X3 <- landcover[nrow(landcover),"Mixed_Forest"]
X4 <- landcover[nrow(landcover),"Woody_Wetlands"]
X <- c(X1, X2, X3, X4)
n <- sum(X)
a <- rep (1, ncol(landcover))
loop <- (nrow(landcover)-1) #Could be deleted since it is not in the data model.
class <- (ncol(landcover)) #Could be deleted since it is not in the data model.


#Data model (multinomial)

FIAmodel = "
model{
  P ~ ddirch(a) #prior
  X ~ dmulti(P, n)
#  for(i in 1:loop){
#    X[i, 1:class] ~ dmulti(P, n)
#  }  
}
"

#JAGS stuff

#data = list(n=n, class=class, loop=loop, a=a, X=X)
data = list(n=n, a=a, X=X)
init = NULL
j.model   <- jags.model (file = textConnection(FIAmodel), data = data, inits = init, n.chains = 3)
update(j.model, n.iter=1000)
j.out   <- coda.samples (model = j.model,variable.names= c("P"), n.iter = 10000)
summary(j.out)

output <- as.data.frame(as.matrix(j.out))
colnames(output) <- colnames(landcover)
par(mfrow = c(2,1))
x <- seq(0,1,length = 100)
for(i in 1:ncol(output)){
  plot(output[,i], type="l", main = colnames(output)[i])
  plot(x,dbeta(x,a[i],sum(a)-a[i]), type="l", col="red", ylim = c(0,5))
  lines(density(output[,i]))
}
  

 