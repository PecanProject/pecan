source_ui <- function(path, ...) {
  source(
    file.path(path = "ui_files", ...),
    local = TRUE
  )$value
}

