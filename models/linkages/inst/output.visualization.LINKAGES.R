if(FALSE){
### set working directory to output file.
link = as.matrix(read.csv("OUT.csv",head=FALSE))

x=seq(0,1150,50) #change to match output interval in OUT.csv
ecol_proc=link[1:(nrow(link)-2)/4,]
colnames(ecol_proc) = c("year","num stems","ag biomass","leaf litter","leaf litter N","ag npp","avail n","humus C:N","soil co2-c","soil OM","aet")
ecol_proc_cis = link[((nrow(link)-2)/4)+1:2*(nrow(link)-2)/4,]

par(mfrow=c(3,3))
for(i in 2:10){
  plot(x,ecol_proc[,i],typ="l",ylim=c(min(ecol_proc[,i]-ecol_proc_cis[,i]),max(ecol_proc[,i]+ecol_proc_cis[,i])),main=colnames(ecol_proc)[i],ylab=NA,xlab="Year")
  lines(x,ecol_proc[,i]-ecol_proc_cis[,i],lty=3,col="blue")
  lines(x,ecol_proc[,i]+ecol_proc_cis[,i],lty=3,col="blue")
  #abline(v=ipolat_nums,lty=4)
}

tree_choices = as.matrix(read.csv("/Users/paleolab/pecan/models/linkages/tests/testthat/LINKAGES_tree_choices.csv",header=FALSE))
tree_names = tree_choices[link[(2*(nrow(link)-2)/4)+2,2:11],1]
biomass=link[((2*(nrow(link)-2)/4)+3):((3*(nrow(link)-2)/4)+2),]
colnames(biomass) = c("Year",tree_names)
biomass_cis = link[((3*(nrow(link)-2)/4)+3):nrow(link),]

par(mfrow=c(1,2))
plot(x,biomass[,2],type="l",lwd=4,main=NA,xlab="Years",ylab="Average Biomass",ylim=c(0,max(test_biomass[,2:11])))
lines(x,biomass[,3],col="red",lwd=4)
lines(x,biomass[,4],col="yellow",lwd=4)
lines(x,biomass[,5],col="blue",lwd=4)
lines(x,biomass[,6],col="green",lwd=4)
lines(x,biomass[,7],col="purple",lwd=4)
lines(x,biomass[,8],col="gray",lwd=4)
lines(x,biomass[,9],col="orange",lwd=4)
lines(x,biomass[,10],col="lightblue",lwd=4)
lines(x,biomass[,11],col="pink",lwd=4)
plot.new()
legend("center",c(colnames(biomass[,2:11])),lwd=rep(4,9),lty=rep(1,9),col=c("black","red","yellow","blue","green","purple","gray","orange","lightblue","pink"),xpd=TRUE)



}