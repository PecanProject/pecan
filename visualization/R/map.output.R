#' Map Output
#'
#' @param table data.table or data.frame with columns lat, lon, followed by variable names 
#' @param variable name of variable to be mapped
#' @return plot
#' @export
#' @author David LeBauer
map.output <- function(table, variable){
  require(ggplot2)
  if(any(table$lat <0) | any(table$lon > 0)){
    world <- data.table(map_data("world"))
  } else {
    world <- data.table(map_data("usa"))
  }
  map <- ggplot() +
    geom_polygon(data = world, aes(x=long, y = lat, group = group),
                 fill = "white", color = "darkgrey") +
    geom_point(data = table,
               aes(x = lon, y = lat, color = table[, variable]), size = 5) +
    scale_color_gradientn(colours = c("red","orange", "yellow", "green", "blue", "violet")) +
    theme_bw() + xlim(range(pretty(table$lon))) + ylim(range(pretty(table$lat)))
  return(map)
}
