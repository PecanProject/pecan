##' Plot map of world with model output
##'
##' This is just a first draft
##' @title PEcAn worldmap
##' @param df.in data.frame with columns lat, lon, and (variable) 
##' @param outfile png file  
##' @return NULL plot as side effect
##' @author David LeBauer
##' @export
##' @examples
##' miscanthusyield <- read.csv(system.file("extdata/miscanthusyield.csv",
##'                                      package = "PEcAn.visualization"))
##' pecan.worldmap(df.in = miscanthusyield,
##'                outfile = file.path(tempdir(), 'worldmap.png'))
pecan.worldmap <- function(df.in, outfile = "worldmap.png", xlim = c(-130,-30), ylim = c(-40,60)){

  ### map of yields
  world <- map_data("world")
  states <- map_data("state")
  rawdf <- df.in
    var <-   colnames(rawdf)[!colnames(rawdf) %in% c("X", "lat", "lon")]
    colnames(rawdf)[!colnames(rawdf) %in% c("X", "lat", "lon")] <- "var"    
  
  ## from http://stackoverflow.com/a/15351169/513006
  spdf <- SpatialPointsDataFrame(data.frame(x = rawdf$lon, y = rawdf$lat),
                                 data = data.frame(z = rawdf$var))
  
  
  e <- extent(spdf)  
  # Determine ratio between x and y dimensions
  ratio <- (e@xmax - e@xmin) / (e@ymax - e@ymin)
  
  # Create template raster to sample to
  r <- raster( nrows = 56 , ncols = floor( 56 * ratio ) , ext = extent(spdf) )
  rf <- rasterize( spdf , r , field = "z", fun = mean )
#   if(!var ==  "aet" ){
#     rf <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 
#            +units=m +nadgrids=@null +wktext  +no_defs")    
#   }
  
  
  rdf <- data.frame( rasterToPoints( rf ) )    
  
  if(!var == "aet"){
    p <-ggplot() + 
      #    geom_polygon(data = world, 
      #                 aes(x=long, y=lat, group = group), 
      #                 colour="grey", fill="white") +
      geom_raster(data = rdf, aes(x, y, fill = layer)) + xlim(xlim) + ylim(ylim) +
      #    geom_point(aes(x = c(-180, -180, 180, 180), y = c(-90, 90, -90, 90), size = 0.1)) +
      #    xlab("Longitude") + ylab("Latitude") + ggtitle(var) +
      theme_nothing() +
      scale_fill_gradientn(colours = colorRampPalette(c("darkblue", "wheat", "darkred"))(20))     
  } else {
    p <-ggplot() + 
      #    geom_polygon(data = world, 
      #                 aes(x=long, y=lat, group = group), 
      #                 colour="grey", fill="white") +
      geom_point(data = df.in, aes(lat, lon, color = aet)) + xlim(-130,-30) + ylim(-40,60) +
      #    geom_point(aes(x = c(-180, -180, 180, 180), y = c(-90, 90, -90, 90), size = 0.1)) +
      #    xlab("Longitude") + ylab("Latitude") + ggtitle(var) +
      theme_nothing() +
      scale_fill_gradientn(colours = colorRampPalette(c("darkblue", "wheat", "darkred"))(20)) 
    
    
  }
  
  if(!is.null(outfile)){
  ggsave(filename = outfile, plot = p, 
         width = 44*diff(xlim)/360, height = 34*diff(ylim)/180, 
         units="in", dpi = 100, bg="transparent")
  
  require(grid)

  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
  }
  

  l <- ggplot(data = rdf, aes(x, y, fill = layer)) + 
    geom_point(aes(0,0)) + 
    theme(legend.position = "right") +
    scale_fill_gradientn(name = "Yield (Mg/ha)", colours = colorRampPalette(c("darkblue", "wheat", "darkred"))(20))
  l2 <- g_legend(l)  
  png(paste0(outfile, "-legend.png") ,units="px",bg = "transparent")
  grid.draw(l2)
  dev.off()
  } else {
    p + theme_bw()
  }
}

