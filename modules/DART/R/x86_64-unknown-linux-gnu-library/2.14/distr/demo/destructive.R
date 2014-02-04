##########################################################
## Demo: Instructive destructive example
##########################################################
require(distr)
options("newDevice"=TRUE)

## package "distr" encourages 
## consistency but does not 
## enforce it---so in general  
## d o   n o t   m o d i f y
## slots d,p,q,r!

N <- Norm()
B <- Binom()
N@d <- B@d
plot(N) 
### consequence: the slots of N
## are no longer consistent!!
