library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinydashboard)
library(gganimate)
library(plotly)
library(tidyverse)
library(rpart)
library(rattle)
library(leaflet)
library(htmltools)

ui <- dashboardPage( skin = "black",
                     dashboardHeader(title = "Flux Dashboard"),

                     dashboardSidebar(
                       selectInput("site_id", h3("Select Site:"), choices = unique(data$site_id)),
                       selectInput(inputId = "start_date", label="Forecast Horizon Date:", choices = unique(data$reference_datetime)),
                       sidebarMenu(
                         menuItem("NEE Forecast", tabName = "nee_ft", icon = icon("chart-area")),
                         menuItem("NEE Scatter", tabName = "nee_sct", icon = icon("tree")),
                         menuItem("NEE Error", tabName = "nee_err", icon = icon("tree")),
                         menuItem("LE Forecast", tabName = "le_ft", icon = icon("chart-area")),
                         menuItem("LE Scatter", tabName = "le_sct", icon = icon("tree")),
                         menuItem("LE Error", tabName = "le_err", icon = icon("tree"))
                       )
                     ),

                     dashboardBody(
                       tabItems(
                         #Map Tab
                         tabItem(tabName = "nee_ft",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("nee_ft_plot")
                                   )
                                 ))
                         ),
                         tabItem(tabName = "nee_sct",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("nee_sct_plot")
                                   )
                                 ))
                         ),
                         tabItem(tabName = "nee_err",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("nee_err_plot")
                                   )
                                 ))
                         ),
                         tabItem(tabName = "le_ft",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("le_ft_plot")
                                   )
                                 ))
                         ),
                         tabItem(tabName = "le_sct",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("le_sct_plot")
                                   )
                                 ))
                         ),
                         tabItem(tabName = "le_err",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("le_err_plot")
                                   )
                                 ))
                         )
                       )
                     )
)
