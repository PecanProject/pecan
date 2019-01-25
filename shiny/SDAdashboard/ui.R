library(shiny)
library(shinythemes)
library(bsplus)
library(shinyWidgets)
library(shinycssloaders)
suppressPackageStartupMessages(require(shinyjs))
suppressPackageStartupMessages(require(V8))
source(file.path('Utilities','uihelp.R'),local = T)$value

#cosmo,flatly,sandstone, yeti
fluidPage( tags$head(HTML('<title>SDA Dashboard</title>')),
           navbarPage(h4("SDA Dashboard"),theme =shinytheme("cosmo"),id = "inTabset",
                      
                      tabPanel(h4("Home"),
                               uiOutput('mainui'),
                               tags$head(tags$script(src="scripts.js")),
                               
                               tags$head(
                                 tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                               ),
                               tags$head(
                                 tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Lato:400,700,900")
                               ),
                               useShinyjs(),
                               use_bs_tooltip(),
                               use_bs_popover(),
                               shinytoastr::useToastr()
                                   )#,
                      # tabPanel(h4("Signals"),
                      #          fluidRow(
                      #            column(12)
                      #          
                      # )
           ),

# 
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$div(id="loadmessage",
                                                HTML(paste0("<div class=\"span4\"><img src=\"loader.gif\" height=\"256\" width=\"256\"></div>
                                                 "))
                                                 ))
                               )