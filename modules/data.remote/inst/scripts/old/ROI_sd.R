ROI_sd<-function(radius,coords,k){
  

    buffext<-as.vector(disc(radius=radius, centre=coords[k,])) ##10m is the smallest buffer size that overlaps >1 cell
    ext<-extent(c(buffext[[2]],buffext[[3]])) 
    cellnums<-cellsFromExtent(rast,ext)
#     xyFromCell(rast, cellnums, spatial=T)
    
    sd_ROI<-suppressMessages(sd(extract(rast,cellnums)))

  
  return(sd_ROI)
}