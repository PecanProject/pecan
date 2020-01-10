##' @name add_icon
##' @title add_icon
##' @param x x-coordinate of logo
##' @param y y-coordinate of logo
##' @param id additional plot identificaition (URL, database ID, etc)
##' @export 
##' @author Mike Dietze
##' 
add_icon <- function(id = NULL, x = 0, y = 0) {

  # png and grid are both in Suggests; need to check if available before using
  if (!requireNamespace("png", quietly = TRUE)
      || !requireNamespace("grid", quietly = TRUE)) {
    PEcAn.logger::logger.error(
      "PEcAn.visualization::add_icon needs packages 'png' and 'grid'")
    return(NULL)
  }

  icon <- png::readPNG(
    system.file("favicon.png", package = "PEcAn.visualization"))
  dims <- dim(icon)
  logo <- grid::rasterGrob(icon, grid::unit(x, "npc"),
                     grid::unit(y, "npc"),
                     grid::unit(dims[1], "points"),
                     grid::unit(dims[2], "points"),
                     just = c("left", "bottom"))
  grid::grid.draw(logo)

  lab <- grid::textGrob(label = paste("PEcAn", id),
                  x = grid::unit(x, "npc") + grid::unit(dims[1], "points"),
                  y = grid::unit(y, "npc"), just = c("left", "bottom"))
  grid::grid.draw(lab)
}  # add_icon
