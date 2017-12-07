#####set up working directory####
setwd("/Users/klesse/Dropbox/PIPO AZ FIA crossdated") # where all the PIPO cores are in 
mydir<-"/Users/klesse/Dropbox/PIPO AZ FIA crossdated"
mydata<-list.files(mydir)
mydata


AZ_FIA_plotcoordinates<-read.table("/Users/klesse/Documents/LTRR/plots/PSME/AZ_FIA_plotcoordinates.txt",header=T,sep=";")
colnames(AZ_FIA_plotcoordinates)



####subset the dataset####
#substr(mydata,1,6) gets you the county+plot number that is saved in AZ_FIA_plotcoordinates$plotnew
subset_AZ_FIA_plotcoordinates<-AZ_FIA_plotcoordinates[which(AZ_FIA_plotcoordinates$plotnew %in% substr(mydata,1,6)),]
unique(substr(mydata,1,6))
nrow(subset_AZ_FIA_plotcoordinates)
#subset_AZ_FIA_plotcoordinates<-subset_AZ_FIA_plotcoordinates[match(unique(subset_AZ_FIA_plotcoordinates$ID2),subset_AZ_FIA_plotcoordinates$ID2),]
#change column order for ClimateNA

silly<-data.frame(ID1=rep("FIA_AZ_PIPO",nrow(subset_AZ_FIA_plotcoordinates)),
                  ID2=subset_AZ_FIA_plotcoordinates$plotnew,
                  lat=subset_AZ_FIA_plotcoordinates$LAT,
                  lon=subset_AZ_FIA_plotcoordinates$LON,
                  el=subset_AZ_FIA_plotcoordinates$el)
silly$el<-round(silly$el*0.3048,0)
setwd("/Users/klesse/Documents/LTRR/Arizona FIA data")
write.csv(silly,"az_fia_PIPO_coords.csv", quote=F, row.names=F)



####
####now: go and extract the climate data from climateNA!
####

#####reimport the data from climateNA####
climateAZFIA <- read.csv("~/Documents/LTRR/PSME data/az_fia_coords_1901-2015AMT.csv", sep=",")
View(climateAZFIA)
####defining the sitecodes####
Sitecode<-unique(climateAZFIA$ID2)
climate_AZFIA_coords<-subset(climateAZFIA[1:length(Sitecode),],select=c(ID2,Latitude,Longitude,Elevation))

####which climate parameters do you want to extract? make a selection here: 
clim_parameter<-c("Tmax","Tmin","Tave","PPT","CMD","PAS","RH","Eref")
####if you want to extract only CMD the code could probably be much simpler. ;-)
climateAZFIA_site<-NULL
for (i in 1:length(Sitecode)){
  climateAZFIA_site[[i]]<-subset(climateAZFIA,c(ID2==Sitecode[i]))
  names(climateAZFIA_site)[[i]]<-Sitecode[[i]]
  for(j in 1:length(clim_parameter)){
    climateAZFIA_site[[i]][[j]]<-subset(climateAZFIA_site[[i]],select=grep(clim_parameter[j],colnames(climateAZFIA)))
    names(climateAZFIA_site[[i]])<-clim_parameter
  }
}
names(climateAZFIA_site)<-Sitecode

#####doesn't look too nice, but what you get is a list with the length according to the number of different sites. Each row is now a year and the columns are the different monthly climate parameters 
####climateAZFIA_site$`010049`$CMD calls CMD monthly parameters of plot 010049, $PPT: precip, and so on.

####to extract all CMD values of all sites use:
CMDofallsites<-sapply(climateAZFIA_site,function(x)as.numeric(unlist(x$CMD)))

###columns: sites; rows: first 115 CMD of January (year 1901:2015),116:230 February, and son on. 
####now collapse everything into strings for the Bayesian Model####

CMD_matrix_new_transposed_string<-matrix(ncol=12,nrow=ncol(CMDofallsites))

for(i in 1:ncol(CMDofallsites)){
  for(j in 1:12){
    CMD_matrix_new_transposed_string[i,j]<-c(paste0(CMDofallsites[c(1:115)+(j-1)*115,i],collapse=","))
  }
}



############part for importing the rwl files######

ringwidthdata<-NULL
for (i in 1: length(mydata)){
ringwidthdata[[i]]<-read.rwl(mydata[i])
ringwidthdata[[i]]<-ts(ringwidthdata[[i]],start=as.numeric(rownames(ringwidthdata[[i]])[1]))
}
ringwidthdata.ts<-do.call(ts.union,ringwidthdata)


ringwidthdata_matrix_new_transposed_string<-matrix(ncol=ncol(ringwidthdata),nrow=nrow(ringwidthdata))

for(i in 1:ncol(ringwidthdata)){
  for(j in 1:12){
    ringwidthdata_matrix_new_transposed_string[i,j]<-c(paste0(ringwidthdata[,i],collapse=","))
  }
}

tsp(ringwidthdata)[1] #first year of time-series
tsp(ringwidthdata)[2] #last year of time-series

######