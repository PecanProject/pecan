
pda.emulator <- function(input.dir,prior.dir,prior.file,out.dir){

require(emulator)
require(kernlab)
rm(list=ls())

#move post.distns.Rdata over from forecast
#input.dir <- './pecan/Toolik/growth/'
#prior.dir <- './pecan/Toolik/growth/'
#prior.file<-'/post.distns.Rdata'
#out.dir <- './pecan/Toolik/growth/'

#Move prior.distns.Rdata files from last run over to forecast before running ensemble
#input.dir <- './pecan/BarrowDA5param/'
#prior.dir <- './pecan/Toolik/growth/'
#prior.file<-'/da.post.distns.Rdata'
#out.dir <- './pecan/BarrowDA5param/'

run.id<-commandArgs(trailingOnly = TRUE)
print(run.id)

ddist<- function(x, prior){
  if(prior$distn=='exp') {return(dexp(x, prior$parama))}
  eval(parse(text=paste('d', prior$distn, sep='')))(x, prior$parama, prior$paramb)
}
rdist<- function(x, prior){
  if(prior$distn=='exp') {return(rexp(x, prior$parama))}
  eval(parse(text=paste('r', prior$distn, sep='')))(x, prior$parama, prior$paramb)
}

load(paste(input.dir, 'samples.Rdata', sep=''))
load(paste(input.dir, 'L.nee.Rdata', sep=''))

sel <- !is.na(y)


df <- data.frame(L.nee = y, x)
kernlab.gp <- gausspr(L.nee ~., data=df[y<quantile(y, 0.995),])
#predictions <-predict(kernlab.gp, x); plot(y, predictions)

best.y <- order(y)
print(y[best.y])

#PRIORS
pfts<-names(ensemble.samples)
pfts<-pfts[pfts != 'env']
priors<-do.call(rbind, lapply(pfts, function(pft){
      load(paste(prior.dir, pft, prior.file, sep = ''))
      traits<-names(ensemble.samples[[pft]])
      return(post.distns[traits,])
    }))

p.rng <- do.call(rbind, lapply(pfts, function(pft){
          t(sa.samples[[pft]][c(1,nrow(sa.samples[[pft]])),])
        }))

start.time <- proc.time()
#browser()
m<-lapply(1, function(chain){
      default.x <- lapply(1:ncol(x), function(i) { print(priors[i,]); rdist(1, priors[i,])})
      names(default.x) <- names(x)
      mcmc.GP(kernlab.gp, ## emulator
          default.x,  ## initial conditions
          nmcmc = 8000,      ## number of reps
          p.rng,        ## range of covs
          "lin",      ## "lin"ear vs "log" of LogLikelihood 
          mix = "each", ## jump "each" dimension independently or update them "joint"ly
          #jmp0 = c(0.008408595, 0.196528177, 0.000988355,  0.194925416,0.002382323,0.009091557,
          #         0.203131470, 0.002248734, 0.007984979),
          priors=priors
      )$mcmc
    })

print(priors)
warnings()
finish.time <- proc.time()-start.time
print(paste(finish.time, 'seconds'))

save(m, file=paste(out.dir, './mcmc', run.id, '.Rdata', sep=''))

}








