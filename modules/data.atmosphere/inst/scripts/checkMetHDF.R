## code to load and check HDF met files

library(hdf5)

fname <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/snowcorHDF/ED_OL_2002JUN.h5"
met <- hdf5load(fname,load=FALSE)

par(mfrow=c(3,4))
for(i in 1:12){
  plot(met[[i]],main=names(met)[i],type='l')
}
