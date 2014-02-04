require(distrMod)
options("newDevice"=TRUE)

## generation of distribution with density ~ e^{-|x|^3
myD <- AbscontDistribution(d = function(x) exp(-abs(x)^3),
                           withS = TRUE, Symmetry=SphericalSymmetry(0))
## generating some data from this distribution
## in a location scale model
scl.true <- 2; loc.true <- 3
x <- r(myD)(40) * scl.true + loc.true
## definition of the corresponding model
myDF <- L2LocationScaleFamily(centraldistr = myD)
plot(myDF)

## ML-Estimation
(mledistrMod <- MLEstimator(x,myDF))
par(mfrow=c(1,2))
plot(profile(mledistrMod))
confint(mledistrMod)

## MD-Estimation
(mde.kolm <- MDEstimator(x,myDF))
(mde.CvM  <- MDEstimator(x,myDF, distance = CvMDist))
