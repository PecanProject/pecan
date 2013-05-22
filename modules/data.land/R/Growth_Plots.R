#Friday plots
par(mfrow=c(1,4),oma=c(4,4,0,0),mar=c(1,1,1,1))

#Bartlett Growth 2012-2002
growth2012=na.exclude(cbind(spp,incrementMatch[,198:189]))
mean2012=rowMeans(growth2012[2:6])
growth2012=cbind(growth2012,mean2012)
boxplot(growth2012[,2]~growth2012[,1],horizontal=TRUE,las=1,
        xlab="2012-2002 Avg. Increment",ylim=c(0,4.3))

#Bartlett Growth 2002-1992
growth2001=na.exclude(cbind(spp,incrementMatch[,189:180]))
mean2001=rowMeans(growth2001[2:11])
growth2001=cbind(growth2001,mean2001)
boxplot(growth2001[,2]~growth2001[,1],horizontal=TRUE,las=1,
        xlab="2002-1992 Avg. Increment",yaxt='n',ylim=c(0,4.3))

#Bartlett growth 1992-1982
growth1990=na.exclude(cbind(spp,incrementMatch[,180:171]))
mean1990=rowMeans(growth1990[2:11])
growth1990=cbind(growth1990,mean1990)
boxplot(growth1990[,2]~growth1990[,1],horizontal=TRUE,las=1,
        xlab="1992-1982 Avg. Increment",yaxt='n',ylim=c(0,4.3))

#Bartlett growth 1982-1972
growth1980=na.exclude(cbind(spp,incrementMatch[,171:162]))
mean1980=rowMeans(growth1980[2:11])
growth1980=cbind(growth1980,mean1980)
boxplot(growth1980[,2]~growth1980[,1],horizontal=TRUE,las=1,
        xlab="1982-1972 Avg. Increment",yaxt='n',ylim=c(0,4.3))

mtext("Bartlett, NH 10yr Average Growth 2012-1972", side = 1, outer = TRUE,
      line = 2)

par(mfrow=c(1,1))
#growth.all = (cbind(spp,incrementMatch[,]))
growth12 = na.exclude(cbind(dbh[,2],incrementMatch[,198]))
reg1=lm(growth12[,2] ~ growth12[,1])
plot(growth12[,1],growth12[,2],xlab="DBH12",ylab="Growth Increment 2012")
abline(reg1,lwd = 3, col = 3)
mtext("DBH 2012", side = 1, outer = TRUE, line = 2)
mtext("Growth Increment 2012", side = 2, outer = TRUE, line = 2)
summary(reg1)

plot12 = na.exclude(cbind(plot.data$plot,incrementMatch[,198:189]))
boxplot(plot12[,2]~plot12[,1])
mtext("Plot", side = 1, outer = TRUE, line = 2)
mtext("Growth Increment 2012-2002", side = 2, outer = TRUE, line = 2)
