library(shiny)

shinyApp(ui = source("ui.R")$value, server = source("server.R")$value, options = list(includeCSS("styles.css")))