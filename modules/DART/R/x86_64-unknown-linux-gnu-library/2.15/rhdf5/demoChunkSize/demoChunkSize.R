

library(rhdf5)

Dim = 10000000
Data = 1:10000000L
Data2 = 1:10000000L

chunksize = 10L^(0:6)
level = c(0,3,6,9)

WT = matrix(-1,nr=7,nc=4)
RT = matrix(-1,nr=7,nc=4)
S = matrix(-1,nr=7,nc=4)

for (i in 1:7) {
  for (j in 1:4) {
    h5createFile("test.h5")
    h5createDataset("test.h5","A",dims=Dim, storage.mode = "integer", chunk = chunksize[i], level = level[j])

    t1 = Sys.time()
    for (k in 1:100) {
      h5write(Data[1:10000+(k-1)*10000], "test.h5","A",index=list(1:10000+(k-1)*10000))
    }
    t2 = Sys.time()
    dt1 = as.double(t2-t1)
    WT[i,j] = dt1
    
    t1 = Sys.time()
    for (k in 1:100) {
      Data2[1:10000+(k-1)*10000] = h5read("test.h5","A",index=list(1:10000+(k-1)*10000))
    }
    t2 = Sys.time()
    dt2 = as.double(t2-t1)
    RT[i,j] = dt2

    s = file.info("test.h5")$size
    S[i,j] = s
    file.remove("test.h5")
    
    cat("i = ",i,"; j = ",j,"; t1 = ",dt1,"; t1 = ",dt2, " size=",s,"\n")
  }
}

pdf(file="chunksize.pdf")
par(mfrow=c(3,1))
ylim = range(c(WT))
col = rainbow(4)
plot(-10000,xlim=c(1,7),ylim=ylim,xlab="chunksize",ylab="time (sec)",main="writing time",xaxt="n")
axis(1,1:7,chunksize)
for (i in 1:4) {
  lines(WT[,i],col=col[i], pch=20,lwd=3,type="b")
}
legend("topright",sprintf("level = ",c(0,3,6,9)),fill=col,inset=0.01)

ylim = range(c(RT))
col = rainbow(4)
plot(-10000,xlim=c(1,7),ylim=ylim,xlab="chunksize",ylab="sec",main="reading time",xaxt="n")
axis(1,1:7,chunksize)
for (i in 1:4) {
  lines(RT[,i],col=col[i], pch=20,lwd=3,type="b")
}
legend("topright",sprintf("level = %d",c(0,3,6,9)),fill=col,inset=0.01)

ylim = range(c(log10(S)))
col = rainbow(4)
plot(-10000,xlim=c(1,7),ylim=ylim,xlab="chunksize",ylab="byte [log10]",main="file size",xaxt="n")
axis(1,1:7,chunksize)
for (i in 1:4) {
  lines(log10(S[,i]),col=col[i], pch=20,lwd=3,type="b")
}
legend("topright",sprintf("level = %d",c(0,3,6,9)),fill=col,inset=0.01)
dev.off()
