####################################################################################################
#
#            Estimate Vcmax and Jmax using Farquhar equation optimization
#
#           --- Uses DEoptim global parameter optimization
#            --- Outputs Rd, Vcmax, and Jmax (when a full curve is availible) estimated parameters 
#               in a single output .csv file with assocaited information
#           --- Output A-Ci fit diagnostic figures and DEoptim parameter trace plots
#
#		--- Last updated:  8.22.2013 BY SPS
####################################################################################################


#---------------- Close all devices and delete all variables. -------------------------------------#
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#---------------- *User defined settings.* --------------------------------------------------------#
### Location of R scripts.  Needed for Farquhar model optimization. Contains functions.
r.functions <- '/Users/serbin/Data/Dropbox/Soybean_Aphid_Project/R_Scripts/'

### Input LI6400 dataset.  First define location of file (i.e. directory). 
in.dir <- '/Users/serbin/Data/Dropbox/Soybean_Aphid_Project/Data/LI6400_Data/'
dataset <- 'Compiled_data_2012_ACi_only.v2.csv'

### Define input file to be processed
ge.data <- read.table(paste(in.dir,"/",dataset,sep=""), header=T,sep=",")
summary(ge.data)          ## Summary of dataset

### Main output directory 
out.dir <- ('/Users/serbin/Data/Dropbox/Soybean_Aphid_Project/Data/Processed_LI6400_Data/Processed_2012_Data/')
if (! file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)


# *********************************** QA/QC Options ***********************************
### Vcmax to Jmax Ci cutoff.
Vcmax.cutoff <- 220 #ppm  Options: E.g. 180, 200, 220, 250
Jmax.cutoff  <- 450 #ppm  Options: E.g. 400, 450, 500

### Sample QC checks
Cond.cutoff <- 0.08       ## Throw out observations with Cond < cutoff. E.g. 0.08
Ci.cutoff <- c(0,2000)    ## Throw out observations with Ci out of bounds
Tleaf.cutoff <- 1.6       ## How much Tleaf variation to accept in curve data. E.g. 1 
# would allow variation of 1 degree around the mean Tleaf
# *********************************** QA/QC Options ***********************************


# ********************************** DEoptim Options **********************************
### DEoptim options. Controls the parameter search space for each DEoptim iteration.
lower.bound <- c(5,-5,5)     ## Lower bound of Vcmax, Rd, & Jmax
upper.bound <- c(800,50,800)      ## Upper bound of Vcmax, Rd, & Jmax
max.iters <- 1000                 ## Max iterations
RMSE.min <- 0.1                   ## Min value of RMSE to be reached (VTR) during 
NP <- 100                          ## Number of population members. For many problems it is best to set 
                                  ## NP to be at least 10 times the length of the parameter vector.
DWF <- 0.8                        ## Differential weighting factor from interval [0,2]. Default to 0.8.
CR <- 0.6                         ## Crossover probability from interval [0,1]. Default to 0.5.
# DEoptim optimization. DEoptim will stop when either 
# 1) RMSE=RMSE.min or 2) max iters are reached.
# Should be sufficiently small, e.g. 0.5. Set to 
# a low number to ensure convergance (e.g. 0.0001)
# or if unsure. This option can speed up convergence
#see ?DEoptim
# ********************************** DEoptim Options **********************************


# -------------------------------- Temperature adjustment settings ---------------------------------
# Taken from Bernacchi et al. 2001. Plant, Cell and Environment. 24, 253-259
#
# & Long, S. P. and C. J. Bernacchi. 2003. Gas exchange measurements, what can 
# they tell us about the underlying limitations to photosynthesis? Procedures 
# and sources of error. Journal of Experimental Botany 54:2393-2401.
#
# & Medlyn et al., 2002. Plant, Cell and Environment. 25, 1167-1179
#
# & Bernacchi et al., 2013. Plant, Cell and Environment.
#
#
#R       <- 0.008314472               ## Ideal gas constant
R       <- 8.314                     ## Ideal gas constant. J mol-1 K-1
Oxygen  <- 210                       ## Oxygen value (ubar)
#Oxygen  <- 21
Kc25    <- 404.9                     ## umol m-1
#Ekc     <- 79.430                    ## kJ mol-1
Ekc     <- 79430
Ko25    <- 278.4                     ## mmol m-1
#Eko     <- 36.380                    ## kJ mol-1
Eko     <- 36380
Gstar25 <- 42.75                     ## umol m-1
#EGstar  <- 37.830                    ## kJ mol-1
EGstar  <- 37830
mm.constants <- list(R=R,Oxygen=Oxygen,Kc25=Kc25,Ekc=Ekc,Ko25=Ko25,Eko=Eko,Gstar25=Gstar25,
                     EGstar=EGstar)
rm(R,Oxygen,Kc25,Ekc,Ko25,Eko,Gstar25,EGstar)
#--------------------------------------------------------------------------------------------------#


#==================================================================================================#
# End of user defined options. Start of program.
#==================================================================================================#


###################################### START SCRIPT ################################################
print('**************************************************')
print('**************** STARTING SCRIPT *****************')
print('**************************************************')
print(' ')
####################################################################################################


#---------------- Load required libraries ---------------------------------------------------------#
# Info: Loads required R libraries and warns if package is not availible.
ok = require(DEoptim) ; if (! ok) 
  stop("*** Package DEoptim is not available.  This is needed for model optimization ***")
rm(ok)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# Load utils and Farquhar functions for data QA/QC and optimization
source(paste(r.functions,'/','photo.processing.functions.R',sep=""))
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Setup output dirs
dlm = .Platform$file.sep # <--- What is the platform specific delimiter?
aci.fig.dir = paste(out.dir,dlm,"ACi_Diagnostics",sep="")
if (! file.exists(aci.fig.dir)) dir.create(aci.fig.dir)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# Apply data QA/QC functions
#data <- ge.data  #For debugging
ge.data.qc <- data.qc(data=ge.data,out.dir=out.dir,Cond.cutoff=Cond.cutoff,Ci.cutoff=Ci.cutoff,
                      Tleaf.cutoff=Tleaf.cutoff)

out.qc.ge.data <- data.frame(ge.data.qc$Sample.Info,ge.data.qc$GE.data)
write.csv(out.qc.ge.data,file=paste(out.dir,"/","QC_GE_Data.csv",sep=""),row.names=FALSE)
rm(out.qc.ge.data,ge.data)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Apply temperature adjustments to MM constants
loc <- match(c("CI","PRESS"),toupper(names(ge.data.qc$GE.data)))
ge.data.qc$GE.data$Ci_Pcorr = ge.data.qc$GE.data[,loc[1]]*ge.data.qc$GE.data[,loc[2]]*0.01  ## Pressure Adj. of Ci
loc <- match("TLEAF",toupper(names(ge.data.qc$GE.data)))
TleafK  <- ge.data.qc$GE.data[,loc[1]]+273.15                                ## Leaf temperature
ge.data.qc$GE.data$Kc <- mm.constants$Kc25*exp(mm.constants$Ekc*(TleafK-298.15)/
                                                (298.15*mm.constants$R*TleafK))       ## Kc - based on Medlyn et al
ge.data.qc$GE.data$Ko <- mm.constants$Ko25*exp(mm.constants$Eko*(TleafK-298.15)/
                                                (298.15*mm.constants$R*TleafK))       ## Ko - based on Medlyn et al
ge.data.qc$GE.data$Gstar <- mm.constants$Gstar25*exp(mm.constants$EGstar*(TleafK-298.15)/
                                                      (298.15*mm.constants$R*TleafK)) ## Gamma star. Medlyn et al
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Get sample info and summary stats
samples <- unique(ge.data.qc$Sample.Info)

### Get data names
data.names <- names(ge.data.qc$GE.data)
remove.nms <- match(c("PRESS","PARI","CO2REF","PHOTO","CI","CI_PCORR","KC","KO","GSTAR"),toupper(data.names))
data.names <- data.names[-remove.nms]
data.names # What to summarize

index <- within(ge.data.qc$GE.data, indx <- as.numeric(interaction(ge.data.qc$Sample.Info, 
                                                                    drop=TRUE,lex.order=TRUE)))
samples <- data.frame(samples,Index=unique(index$indx))
samples <- samples[order(samples$Index),]
row.names(samples) <- seq(len=nrow(samples))
samples <- samples[,-match("Index",names(samples))]

### Obs stats
means <- aggregate(.~index$indx,data=index,mean)
means <- means[data.names]
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Fit Farquhar model to data
data <- data.frame(ge.data.qc$Sample.Info,ge.data.qc$GE.data)

# Setup outputs
Rd <- array(data=NA,dim=dim(samples)[1])
Vcmax <- array(data=NA,dim=dim(samples)[1])
Jmax <- array(data=NA,dim=dim(samples)[1])
RMSE.DEoptim <- array(data=NA,dim=dim(samples)[1])
RMSE.photo <- array(data=NA,dim=dim(samples)[1])

# i = 122
# i = 44
# i=2
# Main outer loop
system.time(for (i in 1:dim(samples)[1]) {
#system.time(for (i in 1:10) {
  sub.data <- merge(data,samples[i,],by=names(samples[i,]))
  sub.data = sub.data[order(sub.data$Ci_Pcorr),]
  print("--- Processing Sample: ")
  print(paste(as.vector(unlist(samples[i,])),collapse=" "))
  
  # Determine what level of GE processing
  chk2 <- length(which(sub.data$Ci_Pcorr<Vcmax.cutoff))
  chk3 <- length(which(sub.data$Ci_Pcorr>Jmax.cutoff))
  
  # Check if there are enough obs. Remove curves with to few Vcmax/Jmax samples
  if (dim(sub.data)[1]<3 | chk2<3 & chk3<3){
    Rd[i] <- -9999
    Vcmax[i] <- -9999
    Jmax[i] <- -9999
    RMSE.DEoptim[i] <- -9999
    RMSE.photo[i] <- -9999
    
  } else {
    Ci <- sub.data$Ci_Pcorr
    Oxy <- mm.constants$Oxygen
    Kc <- sub.data$Kc
    Ko <- sub.data$Ko
    Gstar <- sub.data$Gstar
    Km <- Kc*(1+(Oxy/Ko))
    Photo <- sub.data[,match("PHOTO",toupper(names(sub.data)))] # Find photo data
    f.model <- NA # Type variable for defining plots
    
    # Vcmax and Jmax
    if (chk2>=3 & chk3>=3){
      f.model <- 1
      fit <- DEoptim(ACi.full,lower=lower.bound, upper=upper.bound, DEoptim.control(NP=NP,
                     F=DWF,CR=CR, itermax=max.iters, VTR=RMSE.min, strategy=2, trace=FALSE,
                     storepopfrom = 1,storepopfreq = 5))
      tempout <- data.frame(fit$optim)
      Vcmax[i] <- tempout[1,1]
      Rd[i] <- tempout[2,1]
      Jmax[i] <- tempout[3,1]
      RMSE.DEoptim[i] <- fit$optim$bestval
      fitted_photo = pmin(((Vcmax[i]*(Ci-Gstar))/(Ci+Km))-Rd[i],
                          ((Jmax[i]*(Ci-Gstar))/((4.5*Ci)+(10.5*Gstar)))-Rd[i])
      residuals = fitted_photo-Photo
      RMSE.photo[i] = sqrt(mean((residuals)^2))
      
      ### Display info to console 
      print(paste("Vcmax: ",round(Vcmax[i],2)))
      print(paste("Jmax: ",round(Jmax[i],2)))
      print(paste("Rd: ",round(Rd[i],4)))  
      print(paste("DEoptim RMSE: ",round(RMSE.DEoptim[i],2)))
      print(paste("Photo RMSE: ",round(RMSE.photo[i],2)))
      print("-----------------------------------------------------------")
      flush.console()
      
      # Vcmax only
    } else if (chk2>=3 & chk3<3) {
      f.model <- 2
      fit <- DEoptim(ACi.rubisco,lower=lower.bound[1:2], upper=upper.bound[1:2], DEoptim.control(NP=NP,
                     F=DWF,CR=CR, itermax=max.iters, VTR=RMSE.min, strategy=2, trace=FALSE, 
                     storepopfrom = 1,storepopfreq = 5))
      tempout <- data.frame(fit$optim)
      Vcmax[i] <- tempout[1,1]
      Rd[i] <- tempout[2,1]
      Jmax[i] <- -9999
      RMSE.DEoptim[i] <- fit$optim$bestval
      fitted_photo = ((Vcmax[i]*(Ci-Gstar))/(Ci+Km))-Rd[i]
      residuals = fitted_photo-Photo
      RMSE.photo[i] = sqrt(mean((residuals)^2))
      
      ### Display info to console 
      print(paste("Vcmax: ",round(Vcmax[i],2)))
      print(paste("Jmax: ",round(Jmax[i],2)))
      print(paste("Rd: ",round(Rd[i],4)))  
      print(paste("DEoptim RMSE: ",round(RMSE.DEoptim[i],2)))
      print(paste("Photo RMSE: ",round(RMSE.photo[i],2)))
      print("-----------------------------------------------------------")
      flush.console()
      
      # Jmax only
    } else if (chk2<3 & chk3>=3){
      f.model <- 3
      fit <- DEoptim(ACi.rubp,lower=lower.bound[3:2], upper=upper.bound[3:2], DEoptim.control(NP=NP,
                     F=DWF,CR=CR, itermax=max.iters, VTR=RMSE.min, strategy=2, trace=FALSE, 
                     storepopfrom = 1,storepopfreq = 5))
      tempout <- data.frame(fit$optim)
      Vcmax[i] <- -9999
      Jmax[i] <- tempout[1,1]
      Rd[i] <- tempout[2,1]
      RMSE.DEoptim[i] <- fit$optim$bestval
      fitted_photo = ((Jmax[i]*(Ci-Gstar))/((4.5*Ci)+(10.5*Gstar)))-Rd[i]
      residuals = fitted_photo-Photo
      RMSE.photo[i] = sqrt(mean((residuals)^2))
      
      ### Display info to console 
      print(paste("Vcmax: ",round(Vcmax[i],2)))
      print(paste("Jmax: ",round(Jmax[i],2)))
      print(paste("Rd: ",round(Rd[i],4)))  
      print(paste("DEoptim RMSE: ",round(RMSE.DEoptim[i],2)))
      print(paste("Photo RMSE: ",round(RMSE.photo[i],2)))
      print("-----------------------------------------------------------")
      flush.console()
      
      } # End processing if/else
    
    ### Generate fit diagnostics
    #if (length(which(sub.data$Ci_Pcorr<Vcmax.cutoff))>=3){
    if (chk2 >=3 | chk3 >=3){
      sample.name <- paste(unlist(samples[i,]),collapse="_")
      sample.name <- gsub(pattern="/","-",sample.name)
      params <- data.frame(Vcmax=Vcmax[i],Jmax=Jmax[i],Rd=Rd[i],RMSE.photo[i])
      plot.ge.fit(type="A-Ci",sub.data,fit,params,aci.fig.dir,sample.name,f.model)
      rm(f.model)
    }
    
  } # End total observations if/else check

  rm(chk2,chk3)
}) #End of outer main loop
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Generate output
output.data <- data.frame(samples,means,Vcmax=Vcmax,Jmax=Jmax,Rd=Rd,RMSE.DEoptim=RMSE.DEoptim,
                          RMSE.photo=RMSE.photo)
output.dataset <- paste(strsplit(dataset,".csv"),".processed.csv",sep="")
write.csv(output.data,file=paste(out.dir,"/",output.dataset,sep=""),row.names=FALSE)
#rm(list=ls(all=TRUE))   # clear workspace
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### EOF
