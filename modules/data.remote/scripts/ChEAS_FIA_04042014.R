##Author Brady S. Hardiman 11/12/2013

##Read in fuzzed FIA coordinates supplied by Andy Finley (MSU), extract palsar backscatter values, fit curves, save figures and extracted values (with coordinates)


## To work from stored, previously extracted values, run everything up to line 48, then skip to line ~284

################################
## Load Required Packages
################################
# require(sp)
# require(rgdal)
# require(raster)
# require(chron)
# require(RgoogleMaps)
# require(maptools)
# require(ggplot2)
# require(car)
# require(spatstat)
# require(fields)
# require(reshape)
# require(rjags)
# require(R2HTML)

################################
## OPTIONS
################################
kml=0 #1 = generate and save kml files of extraction coordinates; 0 = do not generate new kml
fia=0 #1 = use FIA coordinates, 0 = use WLEF/Park Falls Tower coordinates
leaf.off=0 #1=include PALSAR scenes acquired duing leaf off period of the year, 0=exclude leaf off scene dates
plot_ext=1 #Generate figures showing palsar scn extent with overlay of plot coords? (1=yes, 0=no)
machine=1 #1=Brady's Mac; 1=Brady's Linux
# buff=c(48) #vector of buffer sizes (in meters) to extract

coord.set<-c("WLEF", "FIA")

if(machine==2){ #Brady's Linux paths
  metadata<- read.csv("/home/bhardima/pecan/modules/data.remote/output/metadata/output_metadata.csv", sep="\t", header=T) ##for Brady's Linux
  palsar_inpath <- file.path("/home/bhardima/Desktop/cheas/geo_corrected_single_sigma") ##location of PALSAR raw files
  calib_inpath <-"/home/bhardima/pecan/modules/data.remote/biometry/Link_to_Forest_Biomass_Calibration_Coords" ##location of file containing (FIA) plot coords and biomass values for calibrating PALSAR backscatter 
  outpath <- file.path("/home/bhardima/pecan/modules/data.remote/output/data") ##For saving
} 
if(machine==1){ #Brady's Mac paths
  metadata<- read.csv("/Users/hardimanb/Desktop/data.remote(Andys_Copy)/output/metadata/output_metadata.csv", sep="\t", header=T) ##location of PALSAR metadata table
  palsar_inpath <- file.path("/Users/hardimanb/Desktop/data.remote(Andys_Copy)/palsar_scenes/geo_corrected_single_sigma") ##location of PALSAR raw files
  calib_inpath <-"/Users/hardimanb/Desktop/data.remote(Andys_Copy)/biometry" ##location of file containing (FIA) plot coords and biomass values for calibrating PALSAR backscatter 
  outpath <- file.path("/Users/hardimanb/Dropbox/PALSAR_Biomass_Study/data") ##For saving  
}

#Extract palsar data from calibration coordinates
palsar.extractor(kml,fia,leaf.off,plot_ext)

## Create working copy of data (so that I don't need to re-extract if I screw up the data)
## NOTE: Here I remove the NAs from coords that don't fall with in the scene and 
##       the zeros that are an artifact of the mismatch between palsar bbox dim and palsar raster dim (due to tilted orbital path)
dat48<-data.frame(na.exclude(extracted_48m))
colnames(dat48)<-c("scnid","scndate", "plot", "UTM.lat", "UTM.lon", "biomass","HH.sigma.48", "HV.sigma.48","HHse_data_48m","HVse_data_48m")


## NOTE: Converting to dataframe changes all values to factor, so here I reformat the data and save it
dat48$scnid<-as.character(dat48$scnid)
dat48$scndate<-as.Date(dat48$scndate,"%Y-%m-%d")
dat48$plot<-as.character(dat48$plot)
dat48$UTM.lat<- as.numeric(as.character(dat48$UTM.lat))
dat48$UTM.lon<- as.numeric(as.character(dat48$UTM.lon))
dat48$biomass<- as.numeric(as.character(dat48$biomass))
dat48$HH.sigma.48<- as.numeric(as.character(dat48$HH.sigma.48))
dat48$HV.sigma.48<- as.numeric(as.character(dat48$HV.sigma.48))
dat48$year<-as.numeric(format(dat48$scndate,"%Y"))
dat48$month<-as.numeric(format(dat48$scndate,"%m"))
dat48$HHse_data_48m<- as.numeric(as.character(dat48$HHse_data_48m))
dat48$HVse_data_48m<- as.numeric(as.character(dat48$HVse_data_48m))

#This will exclude scenes from the leaf off period (Nov-April)
if(leaf.off==1){ #include leaf off data
  dat48<-dat48
}else{ #exclude leaf off data
  dat48<-dat48[as.numeric(format(dat48$scndate,"%m"))>=05 & as.numeric(format(dat48$scndate,"%m"))<=10,]
}

#This will exclude plots which returned zeros for both HH and HV
#NOTE: this circumstance corresponds to a subset of the plots falling outside of the palsar scene 
#Not sure why these show up as zeros rather than NAs, but they definitely corresponds to non-overlapping scenes/plots
#      This can be verified by examining WLEF_SceneExtent_with_plot_overlay.pdf in data.remote/output/data
dat48<-dat48[dat48$HH.sigma.48 !=0 & dat48$HV.sigma.48 !=0,]


#Generate column of DOYs
dats<-as.character(dat48$scndate) #reformat as character
data.doy.07<-chron(dates = as.character(dat48$scndate[grep("2007",dats)]), format = (dates = "Y-m-d"), origin = c(day = 1, month = 0, year = 2007))
data.doy.07<-as.numeric(data.doy.07)
data.year.07<-substr(dat48$scndate[grep("2007",dates)],1,4)
data.dates.07<-cbind(data.year.07,data.doy.07)

data.doy.08<-chron(dates = as.character(dat48$scndate[grep("2008",dats)]), format = (dates = "Y-m-d"), origin = c(day = 1, month = 0, year = 2008))
data.doy.08<-as.numeric(data.doy.08)
data.year.08<-substr(dat48$scndate[grep("2008",dates)],1,4)
data.dates.08<-cbind(data.year.08,data.doy.08)

data.doy.09<-chron(dates = as.character(dat48$scndate[grep("2009",dats)]), format = (dates = "Y-m-d"), origin = c(day = 1, month = 0, year = 2009))
data.doy.09<-as.numeric(data.doy.09)
data.year.09<-substr(dat48$scndate[grep("2009",dates)],1,4)
data.dates.09<-cbind(data.year.09,data.doy.09)

data.doy.10<-chron(dates = as.character(dat48$scndate[grep("2010",dats)]), format = (dates = "Y-m-d"), origin = c(day = 1, month = 0, year = 2010))
data.doy.10<-as.numeric(data.doy.10)
data.year.10<-substr(dat48$scndate[grep("2010",dates)],1,4)
data.dates.10<-cbind(data.year.10,data.doy.10)

dat48$doy<-c(data.dates.07,data.dates.08,data.dates.09,data.dates.10)

#Save extracted data
write.table(dat48,file=paste(outpath,"/",coord.set[fia+1],"_dat48.csv",sep=""),sep=",",quote=FALSE,col.names = TRUE, row.names=F)

#Switch to working from saved data 
dat48<-read.csv(paste(outpath,"/",coord.set[fia+1],"_dat48.csv",sep=""),header = TRUE)

#Correctly format data (again...sigh...)
dat48$scnid<-as.character(dat48$scnid)
dat48$scndate<-as.Date(dat48$scndate,"%Y-%m-%d")
dat48$plot<-as.numeric(dat48$plot)
dat48$UTM.lat<- as.numeric(as.character(dat48$UTM.lat))
dat48$UTM.lon<- as.numeric(as.character(dat48$UTM.lon))
dat48$biomass<- as.numeric(as.character(dat48$biomass))
dat48$HH.sigma.48<- as.numeric(as.character(dat48$HH.sigma.48))
dat48$HV.sigma.48<- as.numeric(as.character(dat48$HV.sigma.48))
dat48$year<-as.numeric(format(dat48$scndate,"%Y"))
dat48$month<-as.numeric(format(dat48$scndate,"%m"))
dat48$HHse_data_48m<- as.numeric(as.character(dat48$HHse_data_48m))
dat48$HVse_data_48m<- as.numeric(as.character(dat48$HVse_data_48m))

#########################################################
## Run plotting function
#########################################################
palsar.plotter(outpath,coord.set,fia)

#This generates a figure showing the HH values for 2 WLEF plots which are anomalously high
# if(fia==0){
#   plot(dat48$biomass, dat48$HH.sigma.48,xlab="Biomass",ylab="HH (sigma)",main="Anomalous Plots")
#   points(dat48$biomass[dat48$plot=="W47"],dat48$HH.sigma.48[dat48$plot=="W47"],pch=19,col="red")
#   points(dat48$biomass[dat48$plot=="W52"],dat48$HH.sigma.48[dat48$plot=="W52"],pch=19,col="blue")
#   legend("topright",legend=c("W47","W52"),pch=19,col=c("red","blue"),bty="n")
#   
#   dat48<-dat48[dat48$plot !="W47" & dat48$plot !="W52",] #Excludes returns from WLEF plots W47 and W52
# }

#Plot HH values for each year-month combo  
png(file=file.path(outpath,"All_HH_by_scndate.png"),bg="transparent")
par(mfrow=c(3,6),mar=c(3,4,1,0.5))
for(y in unique(dat48$year)){
  for(m in unique(dat48$month)){
    if(length(dat48$biomass[dat48$month==m  & dat48$year==y])<1){ #Skips Year-month combos with no data
      next
    }else{ 
      scatter.smooth(dat48$biomass[dat48$month==m  & dat48$year==y],dat48$HH.sigma.48[dat48$month==m & dat48$year==y],
           xlim=c(min(dat48$biomass),max(dat48$biomass)),ylim=c(min(dat48$HH.sigma.48),max(dat48$HH.sigma.48)),
           xlab="biomass",ylab='HH',main=paste(month.abb[m],y,sep=" "),pch="." )
    }#if
  }#for m
}#for y
dev.off()

#Plot HV values for each year-month combo
png(file=file.path(outpath,"All_HV_by_scndate.png"),bg="transparent")
par(mfrow=c(3,6),mar=c(3,4,1,0.5))
for(y in unique(dat48$year)){
  for(m in unique(dat48$month)){
    if(length(dat48$biomass[dat48$month==m  & dat48$year==y])<1){ #Skips Year-month combos with no data
      next
    }else{ 
      scatter.smooth(dat48$biomass[dat48$month==m  & dat48$year==y],dat48$HV.sigma.48[dat48$month==m & dat48$year==y],
                     xlim=c(min(dat48$biomass),max(dat48$biomass)),ylim=c(min(dat48$HV.sigma.48),max(dat48$HV.sigma.48)),
                     xlab="biomass",ylab='HV',main=paste(month.abb[m],y,sep=" "),pch="." )
    }#if
  }#for m
}#for y
dev.off()




#########################################################
## Run curve fitting function
#########################################################

n.reps<- 1000 #sets value for n.adapt and n.iter
n.chain<-3 #number of MCMC chains to run
bayes.curve.fit(outpath,coord.set,fia,n.reps,n.chain)






#########################################################
## HERE THERE BE DRAGONS! (old/obselete/deprecated)
#########################################################
# ##Plot timeseries of backscatter values from each set of coords
# par(mfrow=c(2,2))
# par(new=T)
# plot(sort.dat48$scndate,sort.dat48$HH.sigma.48,ylim=c(0,1),type='n',xlab="scndate",ylab="HH",main="48m")
# for(i in unique(dat48$plot)){
#   par(new=T)
# lines(sort.dat48$scndate[sort.dat48$plot==i],sort.dat48$HH.sigma.48[sort.dat48$plot==i],type='b',col=i)
# }
# plot(sort.dat48$scndate,sort.dat48$HV.sigma.48,ylim=c(0,0.11),type='n',xlab="scndate",ylab="HV",main="48m")
# for(i in unique(dat48$plot)){
#   par(new=T)
#   lines(sort.dat48$scndate[sort.dat48$plot==i],sort.dat48$HV.sigma.48[sort.dat48$plot==i],type='b',col=i)
# }
# plot(sort.dat60$scndate,sort.dat60$HH.sigma.60,ylim=c(0,1),type='n',xlab="scndate",ylab="HH",main="60m")
# for(i in unique(dat60$plot)){
#   par(new=T)
#   lines(sort.dat60$scndate[sort.dat60$plot==i],sort.dat60$HH.sigma.60[sort.dat60$plot==i],type='b',col=i)
# }
# plot(sort.dat60$scndate,sort.dat60$HV.sigma.60,ylim=c(0,0.11),type='n',xlab="scndate",ylab="HV",main="60m")
# for(i in unique(dat60$plot)){
#   par(new=T)
#   lines(sort.dat60$scndate[sort.dat60$plot==i],sort.dat60$HV.sigma.60[sort.dat60$plot==i],type='b',col=i)
# }

# ##Average palsar values for scenes acquired within the same month?
# months(sort.dat48$scndate[i])
# format(sort.dat48$scndate[i], "%Y")
# tapply()
# plot(sort.dat48$scndate,sort.dat48$HH.sigma.48,ylim=c(0,1),type='n',xlab="scndate",ylab="HH",main="48m")
