##' Plot map of world with model output
##'
##' This is just a first draft
##' @title PEcAn worldmap
##' @param infile csv file with columns lat, lon, and (variable) 
##' @param outfile png file  
##' @return NULL plot as side effect
##' @author David LeBauer
##' @export
##' @examples
##' pecan.worldmap(infile = "https://www.betydb.org/miscanthusyield.csv",
##'                outfile = file.path(tempdir(), 'worldmap.png'))
pecan.worldmap <- function(infile, outfile = "worldmap.png"){
  require(RCurl)
  require(ggmap)
  ### map of yields
  library(scales)
  world <- map_data("world")
  if(grepl('https', infile)){
    file.url <- getURL(infile, ssl.verifypeer = FALSE)
    infile <- textConnection(file.url)
  }
  df <- read.csv(infile)
  var <-   colnames(df)[!colnames(df) %in% c("X", "lat", "lon")]
  colnames(df)[!colnames(df) %in% c("X", "lat", "lon")] <- "var"
  p <- ggplot(df) +
    geom_point(aes(x = lon, y = lat, color = var), size = 2, shape = 15) + 
    coord_equal(ratio=1/cos(mean(df$lat)*pi/180)) +
    scale_colour_gradientn(colours = colorRampPalette(c("darkblue", "wheat", "darkred"))(20)) + 
    geom_polygon(data=world[world$region != "USA",], aes(x=long, y=lat, group = group), 
                 colour="grey", fill="white") +
                   xlab("Longitude") + ylab("Latitude") + ggtitle(var)
  ggsave(filename = outfile, plot = p, width = 44, height = 34, units="in", dpi = 100)
  
}
