################

##### NOTE THIS FILE USES FOLLOWING FILE
##path.func <- '/hsan2/khk/ghg/2012/soil/functions/'
##source(paste(path.func,"biomass.core.r", sep=""))


#  This piece of code defines biomass- and stem volume functions
#  16.12.2008 by Aleksi Lehtonen (originally by Mikko + Anett)
# 
################################################################################
## defining functions
## stem volume [m3] on tree level  =============================== FUNCTIONS
stem.vol.pine <- function(d13rm, ht) {                                 #[dm3 =l]
  b0 <- 0.036089
  b1 <- 2.01395
  b2 <- 2.07025
  b3 <- -1.07209
  b4 <- 0.99676
  return(b0* (d13rm^b1)* (b4^d13rm)* (ht^b2)* ((ht-1.3)^b3))
}
stem.vol.spruce <- function(d13rm, ht) {        
  b0 <- 0.022927
  b1 <- 1.91505
  b2 <- 2.82541
  b3 <- -1.53547
  b4 <- 0.99146
  return(b0* (d13rm^b1)* (b4^d13rm)* (ht^b2)* ((ht-1.3)^b3))
}
stem.vol.birch <- function(d13rm, ht) {        
  b0 <- 0.011197
  b1 <- 2.10253
  b2 <- 3.98519
  b3 <- -2.65900
  b4 <- 0.98600
  return(b0* (d13rm^b1)* (b4^d13rm)* (ht^b2)* ((ht-1.3)^b3))
}
#################here functions for stem volume, including dbh, d6 & h


stem.vol.pine.comp <- function(d13rm, ht, d6) {              #[dm3 =l]
  b0 <- 0.268621
  b1 <- -0.0145543
  b2 <- -0.0000478628
  b3 <- 0.000334101
  b4 <- 0.0973148
  b5 <- 0.0440716
 
  return(b0*d13rm^2+b1*d13rm^2*ht+b2*d13rm^3*ht+b3*d13rm^2*ht^2+b4*(d13rm^2+d13rm*d6+d6^2)+b5*d6^2*(ht-6))
 
}
stem.vol.spruce.comp <- function(d13rm, ht, d6) {           #[dm3 =l]
  b0 <-  0.208043000
  b1 <- -0.014956700
  b2 <- -0.000114406
  b3 <-  0.000436781
  b4 <- 0.133947000
  b5 <- 0.037459900

 return(b0*d13rm^2+b1*d13rm^2*ht+b2*d13rm^3*ht+b3*d13rm^2*ht^2+b4*(d13rm^2+d13rm*d6+d6^2)+b5*d6^2*(ht-6))
 
}
 
stem.vol.birch.comp <- function(d13rm, ht, d6) {            #[dm3 =l]
  b0 <- 0.226547000
  b1 <- -0.010469100
  b2 <- -0.000122258
  b3 <- 0.000438033
  b4 <- 0.099162000
  b5 <- 0.033483600
 
 return(b0*d13rm^2+b1*d13rm^2*ht+b2*d13rm^3*ht+b3*d13rm^2*ht^2+b4*(d13rm^2+d13rm*d6+d6^2)+b5*d6^2*(ht-6))
}



## stem wood density [kg*m-3] on tree level      ================= FUNCTIONS
repola.pine.dens <- function(d13rm,t13,lamsum) {                          # Pine
  b0 <- 378.39
  b1 <- -78.829
  b2 <- 0.039
  return(b0 + b1*d13rm/t13 + b2*lamsum)
}
repola.spruce.dens <- function(d13rm,t13) {                             # Spruce
  b0 <- 442.03
  b1 <- -0.904
  b2 <- -82.695
  dk <- 2+1.25*d13rm
  return(b0 + b1*dk + b2*d13rm/t13)
}
repola.birch.dens <- function(d13rm,t13) {                               # Birch
  b0 <- 431.43
  b1 <- 28.054
  b2 <- -52.203
  dk <- 2+1.25*d13rm
  return(b0 + b1*log(dk) + b2*d13rm/t13)
}

## stem biomass [kg] on tree level      =================  SIMPLE FUNCTIONS
# Functions based only on dbh & h
repola.pine.stem.simp <- function(d13rm, ht) {         
  dk <- 2+1.25*d13rm
  return(ma.ru.m1(dk,ht))
}

repola.spruce.stem.simp <- function(d13rm, ht) {         
  dk <- 2+1.25*d13rm
  return(ku.ru.m1(dk,ht))
}

repola.birch.stem.simp <- function(d13rm, ht) {         
  dk <- 2+1.25*d13rm
  return(ko.ru.m1(dk,ht))
 
}


## stem bark biomass [kg] on tree level      =================  SIMPLE FUNCTIONS
# Functions based only on dbh & h
repola.pine.stembark.simp <- function(d13rm, ht) {         
  dk <- 2+1.25*d13rm
 return(ma.kuo.m2(dk,ht))
 
}

repola.spruce.stembark.simp <- function(d13rm, ht) {         
  dk <- 2+1.25*d13rm
 return(ku.kuo.m2(dk,ht))
}

repola.birch.stembark.simp <- function(d13rm, ht) {         
  dk <- 2+1.25*d13rm
 return(ko.kuo.m2(dk,ht))
}


##  weight of foliage [kg] on tree level    ======================= FUNCTIONS
# 1) REPOLA                                                                 [kg]
repola.pine.needles <- function(d13rm, ht, hlc) {         
  cl <- ht- hlc
  dk <- 2+1.25*d13rm
 return(ma.neul.m2(dk,ht,cl))

}
        
repola.spruce.needles <- function(d13rm, ht, hlc) {     
  cl <- ht- hlc
  dk <- 2+1.25*d13rm
 return(ku.neul.m2(dk,ht,cl))
}
repola.birch.foliage <- function(d13rm, ht, hlc) {      
  cl <- ht- hlc
  cr <- cl/ht
  dk <- 2+1.25*d13rm
 return(ko.lehdet.m2(dk,cr))
}
#  MARKLUND                                                               [kg]
marklund.pine.needles <-  function(d13rm, ht) {                  
  return(exp(-3.4781 + 12.1095*d13rm/(d13rm+7) -1.565*log(ht) +0.0413*ht ) )
}
marklund.spruce.needles <-  function(d13rm, ht) {          
  return(exp(-1.8551 + 9.7809*d13rm/(d13rm+12)-0.4873*log(ht)))
}
marklund.spruce.needles.comp <-  function(d13rm, ht, hlc) {          
  cl <- ht- hlc
  return(exp(-1.5732 + 8.4127*d13rm/(d13rm+12) -1.5628*log(ht) + 1.4032*log(cl)))
}
marklund.birch.livbranch <- function(d13rm) {      
  return(exp(-3.3633+ 10.2806*d13rm/(d13rm+ 10) ))
}

# Functions based only on dbh & h

repola.pine.needles.simp <- function(d13rm, ht) {         
  dk <- 2+1.25*d13rm
 return(ma.neul.m1(dk,ht))
}
repola.spruce.needles.simp <- function(d13rm, ht) {         
  dk <- 2+1.25*d13rm
 return(ku.neul.m1(dk,ht))
}

repola.birch.foliage.simp <- function(d13rm) {         
  dk <- 2+1.25*d13rm
 return(ko.lehdet.m1(dk))
}

## weight of living branches [kg] on tree level    =============== FUNCTIONS
repola.pine.livbranch <- function(d13rm, ht, hlc) {         
  cl <- ht- hlc
  cr <- cl/ht
  dk <- 2+1.25*d13rm
 return(ma.ran.m2(dk,ht,cl))
}

repola.spruce.livbranch <- function(d13rm, ht, hlc) {         
  cl <- ht- hlc
  cr <- cl/ht
  dk <- 2+1.25*d13rm
 return(ku.ran.m2(dk,ht,cl))
}

repola.birch.livbranch <- function(d13rm, ht, hlc) {         
  cl <- ht- hlc
  cr <- cl/ht
  dk <- 2+1.25*d13rm
 return(ko.ran.m2(dk,ht,cl))
}

# Functions based only on dbh & h

repola.pine.livbranch.simp <- function(d13rm, ht) {         
  dk <- 2+1.25*d13rm
 return(ma.ran.m1(dk,ht))
}
repola.spruce.livbranch.simp <- function(d13rm, ht) {         
 dk <- 2+1.25*d13rm
 return(ku.ran.m1(dk,ht))
}

repola.birch.livbranch.simp <- function(d13rm, ht) {         
 dk <- 2+1.25*d13rm
 return(ko.ran.m1(dk,ht))
}

##  aboveground biomass [kg] on tree level   ====================== FUNCTIONS
repola.pine.above <- function(d13rm, ht) {
 dk <- 2+1.25*d13rm
 return(ma.tot.m1(dk,ht))
}
repola.spruce.above <- function(d13rm, ht) {
 dk <- 2+1.25*d13rm
 return(ku.tot.m1(dk,ht))
}
repola.birch.above <- function(d13rm, ht) {
 dk <- 2+1.25*d13rm
 return(ko.tot.m1(dk,ht))
}
##  belowground biomass [kg] on tree level    ===================== FUNCTIONS
# 1) REPOLA
repola.pine.stump <- function(d13rm) {                                 # Pine
 dk <- 2+1.25*d13rm
 return(ma.kanto.m1(dk))
}
repola.pine.roots <- function(d13rm) {
 dk <- 2+1.25*d13rm
 return(ma.juuret.m1(dk))
}                   
repola.spruce.stump <- function(d13rm) {                                # Spruce
 dk <- 2+1.25*d13rm
 return(ku.kanto.m1(dk))
}
repola.spruce.roots <- function(d13rm) {
 dk <- 2+1.25*d13rm
 #return(ma.juuret.m1(dk)) korjattu 2.10.2012 AL
 return(ku.juuret.m1(dk))
}
repola.birch.stump <- function(d13rm) {                                 # Birch
 dk <- 2+1.25*d13rm
 return(ko.kanto.m1(dk))
}

repola.birch.roots <- function(d13rm, ht) {
 dk <- 2+1.25*d13rm
 return(ko.juuret.m1(dk, ht))
}

# 2) PETERSSON  
petersson.pine.below <-  function(d13rm) {
  return(exp(3.39014 + 11.06822*(d13rm*10)/((d13rm*10)+113) + ((0.35702)^2)/2))
}
petersson.spruce.below <-  function(d13rm) {
  return(exp(4.52965 + 10.57571*(d13rm*10)/((d13rm*10)+142) + ((0.31487)^2)/2))
}
petersson.birch.below <-  function(d13rm) {
  return(exp(4.90864 + 9.91194*(d13rm*10)/((d13rm*10)+138) + ((0.44180)^2)/2))
}
##  weight of fine roots [kg] on tree level    ======================= FUNCTIONS


fineroots <- function(Mf,spec) {
  if (spec==1) {
    return(Mf*0.676)}
  if (spec==2) {
    return(Mf*0.256)}
  if (spec==3) {
    return(Mf*0.5)}
}

finerootsbytype <- function(Mf,spec,type) {
  if (spec==1 & type<3) {
    return(Mf*0.2)}
  if (spec==2 & type<3) {
    return(Mf*0.18)}
  if (spec==3 & type<3) {
    return(Mf*1)}
 
 if (spec==1 & type==3) {
    return(Mf*0.36)}
  if (spec==2 & type==3) {
    return(Mf*0.3)}
  if (spec==3 & type==3) {
    return(Mf*1.5)}

  if (spec==1 & type==4) {
    return(Mf*0.51)}
  if (spec==2 & type==4) {
    return(Mf*0.42)}
  if (spec==3 & type==4) {
    return(Mf*2)}
 
    if (spec==1 & type>4) {
    return(Mf*0.7)}
  if (spec==2 & type>4) {
    return(Mf*0.54)}
  if (spec==3 & type>4) {
    return(Mf*2.5)}
}
### above cofiding is from Sanna Härkönen biomassa.r

# includes all fineroots, also understorey
# tsum from 1961 - 1990
# Helmisaari et al. 2007
# tonnia biomassaa
fineroot.biomass.tsum <- function(tsum) {
  return((-0.396*tsum+771.4)/100)
}


##########################################
### Understorey vegetation, based data from M.Salemaa
### predictions are kg per ha
##########################################

# dwarf shrubs

dwarfshrub <- function(cov, comp, reg) {
    if (comp=='abv' & reg==1){
    return(cov*19.195)}
    if (comp=='abv' & reg==2){
    return(cov*30.232)}
    if (comp=='bel' & reg==1){
    return(54.531*cov^0.7327)}
    if (comp=='bel' & reg==2){
    return(324.08*cov^0.06074)}
  }

 # Huom! etelän kerroin molemmille
herb <- function(cov, comp, reg) {
    if (comp=='abv' & reg==1){
    return(cov*2.5845)}
    if (comp=='abv' & reg==2){
    return(cov*2.5845)}
    if (comp=='bel' & reg==1){
    return(6.9353*cov)}
    if (comp=='bel' & reg==2){
    return(6.9353*cov)}
  }

grass <- function(cov, comp, reg) {
    if (comp=='abv' & reg==1){
    return(cov*14.128)}
    if (comp=='abv' & reg==2){
    return(cov*13.787)}
    if (comp=='bel' & reg==1){
    return(7.7022*cov)}
    if (comp=='bel' & reg==2){
    return(21.452*cov)}
  }

bryof <- function(cov, reg) {
    if (reg==1){
    return(126.78*exp(cov*0.0229))}
    if (reg==2){
    return(80.079*exp(cov*0.0272))}
  }

lich <- function(cov) {
      return(60.087*cov^0.7015)}
 
 

