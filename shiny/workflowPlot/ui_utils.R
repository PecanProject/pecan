source_ui <- function(...) {
  source(
    file.path("ui_files", ...),
    local = TRUE
  )$value
}

load_anim_div <- function(plot_div) {
  plot_var <- plot_div
  source_ui("load_animation_div.R")
}