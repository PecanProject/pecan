##################


#  This piece of code defines litter production rates
#  29.01.2009 by Aleksi Lehtonen
# 
################################################################################



foliage.litter <- function(Mf, spec, reg, min) {
  if (spec==1 & reg==1 & min==1) {
    return(Mf*0.245)}
  if (spec==1 & reg==2 & min==1) {
    return(Mf*0.154) }
  if (spec==1 & min==0) {
    return(Mf*0.33) }
 
  if (spec==2) {
    if (reg==1) {
          return(Mf*0.1)}
    if (reg==2) {
          return(Mf*0.05)}
  }
 
    if (spec==3) {
           return(Mf*0.79)}
}


branch.litter <- function(Mb, spec) {
  if (spec==1) {
          return(Mb*0.02)}
  if (spec==2) {
          return(Mb*0.0125)} 
  if (spec==3) {
          return(Mb*0.0135)}
}
# the bark of the stump
stump.litter <- function(Mst, spec) {
  if (spec==1) {
          return(Mst*0.0029)}
  if (spec==2) {
          return(0)} 
  if (spec==3) {
          return(Mst*0.0001)}
}

root.litter <- function(Mr, spec) {
  if (spec==1) {
          return(Mr*0.0184)}
  if (spec==2) {
          return(Mr*0.0125)} 
  if (spec==3) {
          return(Mr*0.0135)}
}

bark.litter <- function(Ms, spec) {
  if (spec==1) {
          return(Ms*0.0052)}
  if (spec==2) {
          return(Ms*0.0027)} 
  if (spec==3) {
          return(Ms*0.0029)}
}

fineroot.litter <- function(Mf) {
  return(Mf*0.85)
}
fineroot.litter.reg <- function(Mf,reg) {
  if (reg==1) {
          return(Mf*0.85)}
  if (reg==2) {
          return(Mf*0.5)}
}
#
# Based on assumptions
#

fineroot.litter.tsum <- function(Mfr, tsum) {
  if (tsum>=1200) {
          return(Mfr*0.85)}
  if (tsum<=700) {
          return(Mfr*0.5)}
  if (tsum<1200 && tsum>700){
    return(Mfr*(0.0007*tsum+0.001))
}
}



#########################
### here function for carbon!!!

carbon <- function(M) {
  return(M*0.5)
}

###############################################3
####################### HERE is a piece to calculate AWEN (acid, water, ethanol & non-solubles) for Yasso input
###########################

# Note for Birch Betula pubenscens and brown leaves is used
foliage.AWEN <- function(Lf, spec) {

  fol.AWEN <- matrix(0,nrow=length(Lf), ncol=4)
  ma <- (1:length(Lf))[spec==1]
  ku <- (1:length(Lf))[spec==2]
  ko <- (1:length(Lf))[spec==3]
 
fol.AWEN[,1][ma] <- 0.518*Lf[ma]
fol.AWEN[,1][ku] <- 0.4826*Lf[ku]
fol.AWEN[,1][ko] <- 0.4079*Lf[ko]

fol.AWEN[,2][ma] <- 0.1773*Lf[ma]
fol.AWEN[,2][ku] <- 0.1317*Lf[ku]
fol.AWEN[,2][ko] <- 0.198*Lf[ko]

fol.AWEN[,3][ma] <- 0.0887*Lf[ma]
fol.AWEN[,3][ku] <- 0.0658*Lf[ku]
fol.AWEN[,3][ko] <- 0.099*Lf[ko]

fol.AWEN[,4][ma] <- 0.216*Lf[ma]
fol.AWEN[,4][ku] <- 0.3199*Lf[ku]
fol.AWEN[,4][ko] <- 0.2951*Lf[ko]

  return(fol.AWEN)       
}

fineroot.AWEN <- function(Lfr, spec) {

  fr.AWEN <- matrix(0,nrow=length(Lfr), ncol=4)
  ma <- (1:length(Lfr))[spec==1]
  ku <- (1:length(Lfr))[spec==2]
  ko <- (1:length(Lfr))[spec==3]
 
fr.AWEN[,1][ma] <- 0.5791*Lfr[ma]
fr.AWEN[,1][ku] <- 0.5508*Lfr[ku]
fr.AWEN[,1][ko] <- 0.4079*Lfr[ko]

fr.AWEN[,2][ma] <- 0.1286*Lfr[ma]
fr.AWEN[,2][ku] <- 0.1331*Lfr[ku]
fr.AWEN[,2][ko] <- 0.198*Lfr[ko]
 
fr.AWEN[,3][ma] <- 0.0643*Lfr[ma]
fr.AWEN[,3][ku] <- 0.0665*Lfr[ku]
fr.AWEN[,3][ko] <- 0.099*Lfr[ko]

fr.AWEN[,4][ma] <- 0.228*Lfr[ma]
fr.AWEN[,4][ku] <- 0.2496*Lfr[ku]
fr.AWEN[,4][ko] <- 0.2951*Lfr[ko]

  return(fr.AWEN)
}
## Branches are here
# It seems that there is only valiues for pine (these are applied for others as well)

branches.AWEN <- function(Lb) {
   fb.AWEN <- matrix(0,nrow=length(Lb), ncol=4)
  
a <- c(0.4763,0.4933,0.4289,0.5068,0.4607,0.5047,0.4642,0.5307,0.5256,0.4661,0.5060,
0.4941,0.4848,0.4158,0.4605,0.4423,0.4811,0.4434,0.5141,0.4312,0.4867,0.3997,0.4758,0.4741,0.4996)

w <- c(0.0196,0.0105,0.0197,0.0120,0.0107,0.0106,0.0130,0.0126,0.0116,0.0195,0.0180,
0.0257,0.0219,0.0295,0.0242,0.0198,0.0242,0.0263,0.0188,0.0218,0.0207,0.0234,0.0176,0.0248,0.0188)

e <- c(0.0870,0.0659,0.1309,0.0506,0.0874,0.0519,0.0840,0.0382,0.0394,0.0996,0.0647,
0.0905,0.0633,0.1131,0.0874,0.1101,0.0681,0.1108,0.0561,0.1128,0.0452,0.1161,0.0678,0.0698,0.0470)

n <- c(0.4170,0.4303,0.4205,0.4306,0.4412,0.4328,0.4388,0.4186,0.4234,0.4148,0.4112,
0.4456,0.4300,0.4416,0.4279,0.4278,0.4266,0.4195,0.4110,0.4341,0.4474,0.4608,0.4388,0.4313,0.4346)

fb.AWEN[,1] <- mean(a)*Lb
fb.AWEN[,2] <- mean(w)*Lb
fb.AWEN[,3] <- mean(e)*Lb
fb.AWEN[,4] <- mean(n)*Lb
  
return(fb.AWEN)

 }

stem.AWEN <- function(Lst, spec) {

  st.AWEN <- matrix(0,nrow=length(Lst), ncol=4)
  ma <- (1:length(Lst))[spec==1]
  ku <- (1:length(Lst))[spec==2]
  ko <- (1:length(Lst))[spec==3]
 
st.AWEN[,1][ma] <- 0.5*(0.66+0.68)*Lst[ma]
st.AWEN[,1][ku] <- 0.5*(0.63+0.7)*Lst[ku]
st.AWEN[,1][ko] <- 0.5*(0.65+0.78)*Lst[ko]

st.AWEN[,2][ma] <- 0.5*(0.03+0.015)*Lst[ma]
st.AWEN[,2][ku] <- 0.5*(0.03+0.005)*Lst[ku]
st.AWEN[,2][ko] <- 0.5*(0.03+0)*Lst[ko]

st.AWEN[,3][ma] <- 0.5*(0+0.015)*Lst[ma]
st.AWEN[,3][ku] <- 0.5*(0+0.005)*Lst[ku]
st.AWEN[,3][ko] <- 0

st.AWEN[,4][ma] <- 0.5*(0.28+0.29)*Lst[ma]
st.AWEN[,4][ku] <- 0.5*(0.33+0.28)*Lst[ku]
st.AWEN[,4][ko] <- 0.5*(0.22+0.33)*Lst[ko]

  return(st.AWEN)
}

grass.AWEN <- function(Lg, above) {
 grass.AWEN <- matrix(0,nrow=length(Lg), ncol=4)
  a <- (1:length(Lg))[above==1]
  b <- (1:length(Lg))[above==0]

grass.AWEN[,1][a] <- 0.273*Lg[a]
grass.AWEN[,2][a] <- 0.427518*Lg[a]
grass.AWEN[,3][a] <- 0.274482*Lg[a]
grass.AWEN[,4][a] <- 0.025*Lg[a]

grass.AWEN[,1][b] <- 0.273*Lg[b]
grass.AWEN[,2][b] <- 0.506844*Lg[b]
grass.AWEN[,3][b] <- 0.195156*Lg[b]
grass.AWEN[,4][b] <- 0.025*Lg[b]

 return(grass.AWEN)
}


### Note this is twig (varpu)
twig.AWEN <- function(Lt) {
 twig.AWEN <- matrix(0,nrow=length(Lt), ncol=4)
 
twig.AWEN[,1] <- 0.557*Lt
twig.AWEN[,2] <- 0.225264*Lt
twig.AWEN[,3] <- 0.086736*Lt
twig.AWEN[,4] <- 0.131*Lt


 return(twig.AWEN)
}

### Note this is lichen (jäkälä)
lichen.AWEN <- function(Ll) {
 lichen.AWEN <- matrix(0,nrow=length(Ll), ncol=4)
 
lichen.AWEN[,1] <- 0.836*Ll
lichen.AWEN[,2] <- 0.080864*Ll
lichen.AWEN[,3] <- 0.031136*Ll
lichen.AWEN[,4] <- 0.052*Ll
return(lichen.AWEN)
}

### Note this is moss (sammal)
moss.AWEN <- function(Lm) {
 moss.AWEN <- matrix(0,nrow=length(Lm), ncol=4)
 
moss.AWEN[,1] <- 0.736*Lm
moss.AWEN[,2] <- 0.096026*Lm
moss.AWEN[,3] <- 0.036974*Lm
moss.AWEN[,4] <- 0.131*Lm
return(moss.AWEN)
}


wheat.AWEN <- function(Lwheat) {
  wh.AWEN <- matrix(0,nrow=length(Lwheat), ncol=4)

wh.AWEN[,1] <- 0.712956*Lwheat
wh.AWEN[,2] <- 0.102822*Lwheat
wh.AWEN[,3] <- 0.052463*Lwheat
wh.AWEN[,4] <- 0.131759*Lwheat

  return(wh.AWEN)
}


barley.AWEN <- function(Lbarley) {
  bar.AWEN <- matrix(0,nrow=length(Lbarley), ncol=4)

bar.AWEN[,1] <- 0.728503*Lbarley
bar.AWEN[,2] <- 0.133199*Lbarley
bar.AWEN[,3] <- 0.062327*Lbarley
bar.AWEN[,4] <- 0.075972*Lbarley

  return(bar.AWEN)
}


shit.AWEN <- function(Lshit) {
  shi.AWEN <- matrix(0,nrow=length(Lshit), ncol=4)

shi.AWEN[,1] <- 0.701348*Lshit
shi.AWEN[,2] <- 0.102378*Lshit
shi.AWEN[,3] <- 0.067075*Lshit
shi.AWEN[,4] <- 0.129199*Lshit

  return(shi.AWEN)
}
###################
