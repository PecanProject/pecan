library(PECAn)
##input variables
pft <- 'ebifarm.c4crop'

## 1. get species list based on pft
spp <- query.bety.pft_species(pft)
spstr <- spp$spstr

## 2. get priors available for pft
priors <- query.bety.priors(pft)
print(priors)

prvec <- rownames(priors) # vector of traits with prior distributions for pft 
prstr <- vecpaste(prvec)  # string of " " " " used to query priors

trvec <- gsub('Vm0', 'Vcmax', prvec)  


## now it is time to query the data
trait.data <- query.bety.traits(spstr,trvec) 
## returns list 'trait.data' with one dataframe per variable 

## run the meta-analysis
trait.mcmc <- pecan.ma(trait.data, priors, j.iter = 1000)


## sample values for ensemble
trait.samps <- pecan.samps(trait.mcmc, priors)
prior.dists <- trait.samps[['priors']]
post.samps <- trait.samps[['post.samps']]     
prior.samps <- trait.samps[['prior.samps']] #matrix of samples from all priors


post.dtheta.q <- pecan.dtheta(samps = post.samps)
prior.dtheta.q <- pecan.dtheta(samps = prior.samps)

## generate config files
write.configs(M=100, pft, prior.samps, post.samps)

## print out some statistical summaries and figures from meta-analysis
pecan.ma.summary(trait.mcmc, pft)








################################
## This is where I am testing the validity of the stat. conversions in query.bety.traits() 
###############################
## testing SE = f(P)

#write.csv(data,'query.bety.traits.test.csv') 
data <- read.csv('query.bety.traits.test.csv')

#data <-  data.frame(site = c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,5), mean = c(1,1,1,1.1,1,2,3,4,1,1,1,10,10000,0,0,0,5,5,5,5,5), statname = c(rep('SD',8),rep('none',4), rep('SD',4),'MSE','95%CI','LSD','HSD','MSD'), stat = c(1:16,rep(1,5)), n = rep(4,21))

data <- transform(data,
                  stat = as.numeric(stat),
                  n    = as.numeric(n))
          
