#' Map Output
#'
#' @param table data.table or data.frame with columns lat, lon, followed by variable names 
#' @param variable name of variable to be mapped
#' @return plot
#' @export
#' @author David LeBauer
map.output <- function(table, variable) {
  if (any(table$lat < 0) | any(table$lon > 0)) {
    world <- data.table::data.table(ggplot2::map_data("world"))
  } else {
    world <- data.table::data.table(ggplot2::map_data("usa"))
  }
  map <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = world,
      ggplot2::aes(x = long, y = lat, group = group),
      fill = "white",
      color = "darkgrey") +
    ggplot2::geom_point(
      data = table,
      ggplot2::aes(x = lon, y = lat, color = table[, variable]),
      size = 5) +
    ggplot2::scale_color_gradientn(
      colours = c("red", "orange", "yellow", "green", "blue", "violet")) +
    ggplot2::theme_bw() +
    ggplot2::xlim(range(pretty(table$lon))) +
    ggplot2::ylim(range(pretty(table$lat)))
  return(map)
}  # map.output
