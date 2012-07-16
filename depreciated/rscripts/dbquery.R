#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
### PEcAn code from February 2010
### created before version control implemented
### This file should be retained for archival purposes
### Author: David LeBauer

###libraries
library("RMySQL"   , lib = "/home/dlebauer/lib/R")
library("R2WinBUGS", lib = "/home/dlebauer/lib/R")
library("coda")
library("XML")
##library("ed21", lib.loc = "/home/dlebauer/lib/R")

###
##DEFINE MISC VARIABLES
###

###database
dvr <- dbDriver ("MySQL")#Specifies DB driver being used
con <- dbConnect(dvr, group  = "biofuel" )
                     #group is defined in $HOME/.my.cnf in brackets []
                     #used to specify database name, username, and  password

###vars for bugs
bugs.iter <- 100 #number of iterations for WinBUGS
              # set to 100 for testing, then increase
bugs.chains <- 2

###paths required for bugs()
bugdir <- "/home/dlebauer/.wine/drive_c/Program Files/WinBUGS14/"
setwd ("/home/dlebauer/Research/Database/WinBUGS/")          
bugs.wk.dir = "/home/dlebauer/Research/Database/WinBUGS/"
###paths for wine within bugs()
winedir = "/usr/bin/wine"
WINEPATH <- "/usr/bin/winepath" 

###initialize vars and matrices
i<-1 ; j<-1 ; k<-1 ; ii<-1
###Table to accept post. parameters
postparams <- as.data.frame(matrix(NA,length(traits[,1]),3))
###List of WinBUGS output / mcmc objects for each trait
mcmco <- list()

### paths for ED output files
outPath <- "/home/dlebauer/output/"
###!!set species, ecotype, etc. for query and etc

### Determine the traits of interest (e.g. those for which we have data and priors)
traits <- fetch (dbSendQuery(con, "SELECT DISTINCT TraitName 
				    FROM Priors" ), n=-1) 
		      ##?! will need to be able to select by PriorClade Column
		      ##?! how to choose which traits to use? for now (10/09) only using 3

 
###
##Define WinBUGS model
###
model <- function(){
	mu ~ PRIORDIST ( PRIORPARAMA, PRIORPARAMB)##winbugs needs prec, not variance
	tau ~ dgamma(0.1,0.1) 
	acrossSD <- 1/sqrt(tau)
	prec ~ dgamma(0.1,0.1)
	var <- 1/prec
	stdev <- sqrt(var) 
	for (i in 1:LENGTH)
		{
		theta[i] ~ dnorm(mu,tau) 
		u1[i] <- n[i]/2 
		u2[i] <- (var*n[i])/2
		z[i]<-prec*n[i]
		obs_prec[i] ~ dgamma(u1[i],u2[i]) 
		Y[i] ~ dnorm(theta[i],z[i])
		}
	}

###
##Function write.model.loop
## inserts prior distribution name and parameter values (adapted from R2WinBUGS write.model() )
###
write.model.loop <- function (model, con = "model.bug", pr.dist, pr.param.a, pr.param.b,length)                                         
{                                                                            
    if (is.R()) {                                                            
        model.text <- attr(model, "source")                                  
        model.text <- sub("^\\s*function\\s*\\(\\s*\\)", "model",            
            model.text)                                                      
    }                                                                        
    else {                                                                   
        model.text <- as.character(model)
        model.text <- paste("model", model.text)
    }
    model.text <- gsub("%_%", "", model.text)
    model.text <- gsub("PRIORDIST",paste("d",pr.dist,sep=""),model.text)
    model.text <- gsub("PRIORPARAMA",pr.param.a,model.text)
    model.text <- gsub("PRIORPARAMB",pr.param.b,model.text)
    model.text <- gsub("LENGTH",length,model.text)
    if (!is.R()) {
        model.text <- replaceScientificNotation(model.text)
        model.text <- gsub("invisible[ ]*\\([ ]*\\)", "", model.text)
    }
    writeLines(model.text, con = con)
}





for (i in 1:length(traits[,1])){
    ##Set trait name; if c2n_leaf: change c2n_leaf trait to leafN
    name <- ifelse(traits[i,] == "c2n_leaf","leafN",traits[i,]) 

    ##Query DB and assign to data.frame "data"
    qd <- paste("SELECT TraitMean, TraitSE, TraitN FROM TraitData WHERE TraitID ='",name,"';", sep = "")
    q      <- dbSendQuery(con, qd)
    data   <- fetch ( q, n = -1 )
    colnames(data) <- c("Y", "se", "n")
    data$stdev <- NA   # sd will be calcluated next
    data$obs_prec <- NA #this is the obs. prec for model
    
    ## Where trait is c2n_leaf and data is leaf %N,
    ##  calculate the sd of 48/leafN = c2n_leaf
    ##  assume leaf is 48% C
    ##  sampling from random, but if sd/mean > 0.25 might want to use explicit variable transformation

    if(name == "leafN"){ 
        for (j in 1:length(data$Y)){
          if (!is.na(data$se[j])){
            a <-rnorm(100000, data$Y[j],sqrt(data$n[j])*data$se[j])
            b <- a[which(a>0.2)] #exclude all samples with leaf % N < 0.2%
            data$Y[j]  <- mean ( 48/b )
            data$stdev[j] <-   sd ( 48/b )
           } else data$Y[j] <- 48 / data$Y[j]
        }
    }  else  data$stdev<-sqrt(data$n)*data$se # sd for all other traits
    data$obs_prec <- data$stdev^-2
      ##write data as bugs data to data.txt
      bugs.data( data[,c(1,3,5)],digits = 5, data.file=paste( name,"data.txt",sep=""))

    ##Query DB, make dataframe "prior" 
    qp  <-  paste("SELECT PriorDistn, PriorParamA, PriorParamB, PriorN FROM Priors WHERE TraitName = '", traits[i,],"' ;", sep = "")
    p <- dbSendQuery(con, qp)
    prior <- fetch ( p, n= -1 ) ##1.3 Query DB and assign to data.frame "priors"
    colnames (prior) <- c("distn", "a", "b", "n")

    ##use write.model loop function to specify prior in bugs model
    write.model.loop (model, paste( name, "_model.txt",sep=""), prior$distn, prior$a, prior$b, length(data$Y))

 n.iter = bugs.iter
    
    ##run bugs and output "post" in mcmc object format
post <- bugs (data = paste(name,"data.txt",sep=""),
      inits=NULL,#list(list(mu=mean(data$Y/10),obs_prec=mean(data$Y/10), Y = mean(data$Y)/10 ),list(mu=mean(data$Y*10),obs_prec=mean(data$Y/10), Y = mean(data$Y/10))), #gen inits
      parameters.to.save = cbind("mu", "stdev", "Y"),   #Parameters in model to record
      model.file =  paste(name,"_model.txt",sep=""),#WINBUGS model
      n.chains = bugs.chains,
      n.iter = 1000,
      codaPkg=TRUE,
      n.burnin = n.iter/2,
      bugs.directory = bugdir,
      working.directory = bugs.wk.dir,
      #debug=TRUE,
      clearWD = FALSE,
      useWINE = TRUE,
      newWINE = TRUE,
      WINE = winedir,
      WINEPATH=WINEPATH
      )

#mcmco[[i]] <- read.bugs (post)
    
#add gelman-rubin statistic, if TRUE else  double n.iter else at some length send error
#if (gelman.diag (mcmco[[i]])$mpsrf < 1.2 ) 


pdf ( "posterior_parameters.pdf")
par(mfrow = c(4,2))
plot(mcmco[[i]])
plot(0,0,main = paste (name, "Gelman-Rubin PRSF: ",gelman.diag(mcmco[[i]])$mpsrf),cex=4)
    dev.off()

#output figures w/ std naming trait_sp_
#as a pdf report


  }




###
## Plotting Prior and Posterior Distributions
###
qp     <-  paste("SELECT TraitName, PriorDistn, PriorParamA, PriorParamB, PriorN FROM Priors ;", sep = "")
#qp     <-  paste("SELECT *  FROM Priors ;", sep = "")
p <- dbSendQuery(con, qp)
priors <- fetch ( p, n= -1 ) ##1.3 Query DB and assign to data.frame "priors"
colnames (priors) <- c("TraitName", "distn", "a", "b", "n")

pdf("figure.pdf", width = 11, height = 4)
par (mfrow = c(1,3), cex = 2)
xseq <- seq(0.1,40,length=500)
priorY <-  dlnorm (xseq, priors$a[1], priors$b[1])
SLAmu <- c ( SLAmcmc[[1]][,2],SLAmcmc[[2]][,2])
postY <- density (SLAmu) 
plot (xseq,priorY, ylim = c(0,1),
      type = "l",
      lty = "longdash",
      lwd = 2,
      xlab = "SLA ( m^2 / kg )",
      ylab = "probability density")
lines (postY,lty="solid",lwd = 2)

priorY <-  dnorm (xseq, priors$a[2], priors$b[2])
c2nmu <- c ( c2nmcmc[[1]][,2],c2nmcmc[[2]][,2])
postY <- density (c2nmu) 
plot (xseq,priorY, ylim = c(0,1),
      type = "l",
      lty = "longdash",
      lwd = 2,
      xlab = "Leaf C:N ratio",
      ylab = "")

lines (postY,lty="solid",lwd = 2)


priorY <-  dunif (xseq, priors$a[3], priors$b[3])
leafWmu <- c ( leafWmcmc[[1]][,2],leafWmcmc[[2]][,2])
postY <- density (leafWmu) 
plot (xseq,priorY, ylim = c(0,1),
      type = "l",
      lty = "longdash",
      lwd = 2,
      xlab = "Leaf Width (mm)",
      ylab = "")

lines (postY,lty="solid",lwd = 2)
dev.off()




### n: Number of runs in ensemble
n<-100

###Table of posterior params
postparams <- as.data.frame(matrix(NA,length(traits[,1]),3))


postparams<- read.csv("postparams.csv",header=TRUE)[,2:4]
means.i <- as.data.frame(matrix(NA,n,3))
colnames(means.i) <- postparams$trait

for (i in 1:length(traits[,1])){
  means.i[,i]<-rnorm(n,as.numeric(postparams$mean[i]),as.numeric(postparams$sd[i]))
}

#### 
##3. EXPORT POSTERIOR MEAN TO config.xml
####

for (m in 1:n){
config <- list (
                misc = list(
                  output_filepath = outPath,
                  history_out_filepath = outPath),
                pft = list (
                  num=15,
                  c2n_leaf = means.i[n,1],
                  leafW = means.i[n,2],
                  SLA = means.i[n,3]
                  #for (k in 1:length(traits[,1])) paste ( traits[k,1],"=",means.i[m,k]",")
                  )
                )
abc <- list2XML (config)
writeXML ( abc , paste ("config",m,".xml",sep=""), dtd = "ed.dtd" )

              }


###Move everything to ebi-cluster
edpath <- "/home/dlebauer/EDBRAMS/ED/run/"

system (
for (ii in 1:n){

system ( paste ( "(cd ",edpath ,")" ))
  
newED2IN <- paste("ED2IN",ii,".txt", sep="")
replaceconfig <- paste( "(sed 's/PANVAR.xml/config",ii,".xml/g' ED2IN_rockspringPANVAR > ",newED2IN,")", sep = "")
 system ( replaceconfig )

cmdline <- paste ( "cd", edpath, "; ./mrun 1 ED2IN_rockspringPANVAR oct27log",ii, ".txt", sep = "")
 system ( cmdline )
}






###TESTDATA
foo<- as.data.frame(matrix(NA,4,3))
foo[,1] <- c(8,9,1,7)
foo[,2]<- c(2,2,2,2)
foo[,3]<- c(1,1,1,1)
colnames(foo)<-c("Y","sd","n")
#bugs.data(data[1:4,c(1,4,3)],data.file="foo.txt")
bugs.data(foo,data.file="foo.txt")
write.model.loop(model,"modelfoo.txt","norm", 1,0.5,4)
post <- bugs (data ="foo.txt",
      inits=NULL, #gen inits
      parameters.to.save = "mu",     #Parameters in model to record
      model.file = "modelfoo.txt",#WINBUGS model
      n.chains = 2,
      n.iter = 100,
      n.burnin = n.iter/10,
      bugs.directory = bugdir, # "/usr/local/share/WinBUGS14/",
      working.directory = "/home/dlebauer/Research/Database/WinBUGS/",#"/tmp",
      debug=TRUE,
      clearWD = FALSE,
      useWINE = TRUE,
      newWINE = TRUE,
      WINE = "/usr/bin/wine",
      WINEPATH=WINEPATH
      )
