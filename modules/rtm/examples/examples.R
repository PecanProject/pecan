library(PEcAn.rtm)
#################### Some examples for testing #####################

## Load test data
data(poplar)
data(clover)
data(beech)

spectra = clover
plot(spectra[,1],spectra[,2],type="l",lwd=2.5,ylim=c(0,1))
lines(spectra[,1],1-spectra[,3],lwd=2.5,col="dark grey")
box(lwd=2.2)

## Test PROSPECT inversion
waves <- spectra[,1]
refl <- spectra[,2]
tran <- spectra[,3]

## PROSPECT-4
inv = invprospect(refl,tran,model=4,method="DEoptim",strategy=2,threshold=0.01,cpus=4)
plot.prospect.inv(inv,outdir='/Users/serbin/DATA/',file='test_prospect.inv3')

## PROSPECT-5
inv = invprospect(refl,tran,model=5,"DEoptim",strategy=2,threshold=0.01,cpus=4)
plot.prospect.inv(inv,outdir='/Users/serbin/DATA/',file='test_prospect.inv4')


