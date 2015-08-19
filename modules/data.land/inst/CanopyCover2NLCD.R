##' CanopyCover2NLCD
##'
##' It uses css file which is made by fia2ED module and provides the canopy cover area.
##' During development of cohort and patch level information from FIA database, 
##' we started to compare the extracted data to RS data for same plots in order to see the differences 
##' and also adjust the patch level information for ED input. We calculated the height based on DBH using 
##' allometry. We know the area of each plot which is sum of 4 sub plot and plant density (from FIA). 
##' We calculated tree height, based on DBH using allometry and then sorted them from tallest to shortest. 
##' We then calculated the canopy area base on allometry and following that calculated crown to plot ratio. 
##' Therefore we have canopy area of each tree, height and total area of each plot. 
##' We sorted them from tallest to shortest and start adding canopy areas together to reach 100% land cover, 
##' if adding the last tree causes to go beyond 100%, we dropped that. 
##'
##' @title CanopyCover2NLCD
##' @author Afshin Pourmokhtarian & Michael Dietze


library (data.table)
cls <- c("numeric", "character", rep("numeric", 8))
filelist = list.files("/home/apourmok/My scripts/",pattern = "sitefiles.radius 0.075.*.css", full.names = TRUE)
#filelist = list.files(".",pattern = "sitefiles.radius 0.075.*.css", full.names = TRUE)

#loop over all files
#filelist = list.files("/home/apourmok/pecan/modules/data.land/R/~/",pattern = "sitefiles.radius 0.075.*.css", full.names = TRUE)
#for (k in 1:20){
  #mydata = fread(filelist[i], header=TRUE, colClasses = cls)
#}

mydata = fread(filelist, header=TRUE, colClasses = cls)


# Canopy Area allometry from Dietze and Clark (2008) ==> Allometry.f90
#  !---------------------------------------------------------------------------------------!
#  !     DBH-crown allometry (Dietze and Clark (2008))                                     !
#  !---------------------------------------------------------------------------------------!
#  !----- Intercept. ----------------------------------------------------------------------!
#  b1Ca(5:13)  = 2.490154
#  !----- Slope.  -------------------------------------------------------------------------!
#  b2Ca(5:13)  = 0.8068806
dbh2ca = 2.490154 * mydata$dbh ** 0.8068806
#writing calculated crown size based on allometry back to the file
mydata [,c("Crown") := dbh2ca]
#Temperate PFT allometry
#dbh2h = hgt_ref(ipft) + b1Ht(ipft) * (1.0 - exp(b2Ht(ipft) * dbh))

if (mydata$pft == 7) {
  dbh2h = 0.3 + 27.14 * (1.0 - exp((-0.03884) * mydata$dbh))
}else if (mydata$pft == 8){
  dbh2h = 0.3 + 22.79 * (1.0 - exp((-0.04445) * mydata$dbh))
}else if (mydata$pft == 9){
  dbh2h = 0.3 + 22.6799 * (1.0 - exp((-0.06534) * mydata$dbh))
}else if (mydata$pft == 10){
  dbh2h = 0.3 + 25.18 * (1.0 - exp((-0.04964) * mydata$dbh))
}else if (mydata$pft == 11){
  dbh2h = 0.3 + 23.3874 * (1.0 - exp((-0.05404) * mydata$dbh))
}else if (mydata$pft == 18){
  dbh2h = 0.3 + 25.18 * (1.0 - exp((-0.04964) * mydata$dbh))
}else if (mydata$pft == 19){
  dbh2h = 0.3 + 25.18 * (1.0 - exp((-0.04964) * mydata$dbh))
}else {
  dbh2h = 0.3 + 25.18 * (1.0 - exp((-0.04964) * mydata$dbh))
}
#writing calculated height based on allometry back to the file
mydata [,c("Height") := dbh2h]

#Each subplot radius is 7.32m, area is 168.33 m2, 4 subplot in each plot, therefore total area is 673.32 m2
crown_plot_ratio = dbh2ca*mydata$n*100
mydata [,c("Area") := crown_plot_ratio]

#sorting trees from tallest to shortest starting for each patch
md = mydata[order(patch,-rank(Height),decreasing=FALSE)]

#summing up the crown area of trees from tallest to shortest
patches = unique(mydata$patch)
pfts    = unique(mydata$pft)
LandCover = matrix(numeric(1),length(patches),max(pfts))

Canopy = numeric(nrow(md))
md = cbind(md,Canopy)
grass = numeric(length(patches))
for(i in seq_along(patches)){
  sel = which(md$patch == patches[i])
  if(length(sel)>0){
    CAsum = cumsum(md$Area[sel])
    md$Canopy[sel] = CAsum < 100    
    patchCov = tapply(md$Area[sel]*md$Canopy[sel],md$pft[sel],sum)
    LandCover[i,as.numeric(names(patchCov))]=patchCov
    if(CAsum[length(CAsum)]<100){grass[i]=100-CAsum[length(CAsum)]}
  } 
}

LandCover = cbind(LandCover,grass)
LandCover <- data.table(LandCover)
row.names(LandCover) = patches
LandCover
#write out the output
setnames(LandCover,c(1:20,"grass"))
write.csv(LandCover,row.names=TRUE,file =paste0(filelist,".csv"))


apply(LandCover,2,mean)
#patchnumbers<-mydata[, count := (unique(patch)]

#writing out the crown and height back to the same file
write.table(mydata, filelist, row.names=FALSE, quote=FALSE)


#b1Ht(7)     = 27.14
#b1Ht(8)     = 22.79
#b1Ht(9)     = 22.6799
#b1Ht(10)    = 25.18
#b1Ht(11)    = 23.3874

#b2Ht(7)     = -0.03884
#b2Ht(8)     = -0.04445 
#b2Ht(9)     = -0.06534
#b2Ht(10)    = -0.04964
#b2Ht(11)    = -0.05404

#hgt_ref(6:11)  = 0.3