##' @name add_icon
##' @param x x-coordinate of logo
##' @param y y-coordinate of logo
##' @export 
##' @author Mike Dietze
##' 
add_icon <- function(id=NULL,x=0,y=0){
  require(png)
  require(grid)
  icon <- readPNG(system.file("favicon.png",package = "PEcAn.visualization"))
  dims = dim(icon)
  logo = rasterGrob(icon,unit(x,"npc"),unit(y,"npc"),unit(dims[1],"points"),unit(dims[2],"points"),just=c("left","bottom"))
  grid.draw(logo)

  lab = textGrob(label = paste("PEcAn",id), x = unit(x, "npc") + unit(dims[1],"points"), y = unit(y, "npc"),just = c("left", "bottom"))
  grid.draw(lab)
  
}