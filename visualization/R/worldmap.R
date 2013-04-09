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
pecan.worldmap <- function(df.in, outfile = "worldmap.png"){

  ### map of yields
  world <- map_data("world")
  states <- map_data("state")
  rawdf <- df.in
  var <-   colnames(rawdf)[!colnames(df) %in% c("X", "lat", "lon")]
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
  
  # We can then plot this using `geom_tile()` or `geom_raster()`
  rdf <- data.frame( rasterToPoints( rf ) )    
  
  p <-ggplot() + 
#    geom_polygon(data = world, 
#                 aes(x=long, y=lat, group = group), 
#                 colour="grey", fill="white") +
    geom_raster(data = rdf, aes(x, y, fill = layer)) + xlim(-180,180) + ylim(-90, 90) +
#    geom_point(aes(x = c(-180, -180, 180, 180), y = c(-90, 90, -90, 90), size = 0.1)) +
#    xlab("Longitude") + ylab("Latitude") + ggtitle(var) +
    theme_nothing() +
    scale_fill_gradientn(colours = colorRampPalette(c("darkblue", "wheat", "darkred"))(20)) 
  
  
  ggsave(filename = outfile, plot = p, 
         width = 44, height = 34, 
         units="in", dpi = 100, bg="transparent")
  
}

