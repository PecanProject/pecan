source_ui <- function(...) {
  source(
    file.path("ui_files", ...),
    local = TRUE
  )$value
}

