# .First.lib <- function(which.lib.loc, package){

## define a class for automatically tuning jump distributions
setClass("jump",
         representation(history="numeric",count="numeric",target="numeric",clen="numeric",arate="numeric"),
         prototype=list(history=vector("numeric",0),count=0,target=0.4,clen=100,arate=vector("numeric",0))
         )
## chain = mcmc chain
## history = jump parm history
## count = counter to update jump parm
## target = target acceptance rate
## clen = update period (recompute when count > clen)
setIs("jump","list")

setClass("mvjump",
         representation(history="matrix",count="numeric",target="numeric",clen="numeric",arate="numeric",mydim="numeric"),
         prototype=list(history=matrix(NA,0,0),count=0,target=0.4,clen=100,arate=vector("numeric",0),mydim=1)
         )
## history = jump parm history
## count = counter to update jump parm
## target = target acceptance rate
## clen = update period (recompute when count > clen)
setIs("mvjump","list")

  
# }
