##Generate Figures for PALSAR manuscript
##Author Brady S. Hardiman
##January 24, 2014

require(maps)

##FIGURE 1: Map of Wisc and MI-UP with FIA plots (points) and PALSAR scenes (bboxes)
cheascoords.spatial <- as.data.frame(cheas.coords)
colnames(cheascoords.spatial)[1] <- "x"
colnames(cheascoords.spatial)[2] <- "y"

coordinates(cheascoords.spatial)=~x+y
proj4string(cheascoords.spatial)=CRS("+proj=longlat +datum=WGS84") #define: WGS-84 lon,lat projection
map(database = "state", regions = list("wisconsin","michigan:north"))
points(cheascoords.spatial,pch=16,cex=0.25,col='grey')

# spp.albers <- spTransform(cheascoords.spatial,CRS("+init=epsg:3175")) #convert to: NAD83/Great Lakes and St Lawrence Albers projection
# points(spp.albers,pch=16,cex=0.25,col='grey')
# points(spp.albers,pch=16,cex=0.25,col='grey')
for(i in 1:numfiles){
  scn.bbox<-rbind(c(metadata$scn_nwlon[i],metadata$scn_nwlat[i]),
      c(metadata$scn_nelon[i],metadata$scn_nelat[i]),
      c(metadata$scn_selon[i],metadata$scn_selat[i]),
      c(metadata$scn_swlon[i],metadata$scn_swlat[i]),
      c(metadata$scn_nwlon[i],metadata$scn_nwlat[i]))
  scn.bbox<- Polygon(scn.bbox) #spatial polygon from cheas-palsar extent
  Srs1<- Polygons(list(scn.bbox),"ChEAS_PLASAR_extent") #spatial polygons (plural)
  scn.bbox<-SpatialPolygons(list(Srs1),proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
  plot(scn.bbox,add=TRUE)
}
map.axes()
title(main="Figure 1. Location of study area", xlab=parse(text=paste("Longitude ","^o","*W",sep="")),ylab=parse(text=paste("Latitude ","^o","*N",sep="")))
legend("bottomright", c("FIA plot", "PALSAR scene"), col=c("grey", "black"), pch = c(16,0))

##FIGURE 2:

