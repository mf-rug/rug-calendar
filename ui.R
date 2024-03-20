library(shiny)
library(DT)
library(calendar)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(shinyWidgets)
library(rvest)
library(openxlsx)
library(webshot)

hrs <- hr(style = "margin-top: 2px; margin-bottom: 15px;")
hrc <- HTML('<hr style="height:2px;border-width:0;background-color:#08313e;margin-top: 0px; margin-bottom: 0px;">')
ui <- fluidPage(
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico", type = "image/x-icon")),
  tags$head(
    tags$style(HTML("
      #sidebar {
        overflow-y: scroll;
        min-height:88vh;
        max-height: 88vh;
      }
    ")),
    tags$style(HTML("
      .shiny-input-radiogroup .radio { margin-top: 2px; margin-bottom:4px; }
    "))
  ),
  tags$head(tags$style(
    HTML('
         #sidebar {
            color: #eff6f7;
            background-color: #08313e;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
  )),
  setBackgroundColor(
    color = c("#f6f6f6",'#e7edef'),
    gradient = "linear",
    direction = "bottom",
    shinydashboard = FALSE
  ),
  shinyjs::useShinyjs(),
  div(style = "display: flex; justify-content: space-between;",
      div(
        div(style = "margin-top: 5px; margin-bottom: 0px; color: black; font-size:25px", 
            HTML('University of Groningen - RoostR'),
        ),
        div(style="margin-top: -4px; margin-bottom: 4px; color:grey; font-size:12px",
            HTML('&nbspA customizable alternative to <a href ="https://rooster.rug.nl/">https://rooster.rug.nl/</a>'),
            )
      ),
      div(style = "margin-top: 15px; margin-bottom: 5px; color: grey; font-size:11px", 
          div('Developed by Max Fürst'),
          div(HTML('<a href ="https://www.fuerstlab.com/">www.fuerstlab.com</a>'))
      )
  ),hrc,
  sidebarLayout(
    sidebarPanel(width = 3,id="sidebar",
      fluidRow(
       column(12,
              HTML('<strong>Select output</strong>'), hrs,
              radioButtons('output', label = NULL, selected = 'Courses', choices = c('Courses', 'Programs'), width='100%', inline = TRUE)
       )
      ), 
      fluidRow(
        column(12,
               HTML('<strong>Select academic year</strong>'), hrs,
               shinyjs::disabled(selectInput('years', NULL, choices = NULL, width='100%'))
        )
      ), br(),
      fluidRow(
        column(12,
               HTML('<strong>Select course(s)</strong>'), hrs,
               shinyjs::disabled(selectizeInput('courses', NULL, NULL, multiple = TRUE, width='100%'))
        )
      ), br(),
      shinyjs::hidden(
        div(id = 'hideme', 
            fluidRow(
              column(12,
                   HTML('<strong>Customize appearance</strong>'), 
                   hrs)
              ),
            fluidRow(
              column(5,checkboxInput('colorby', 'Color rows by', TRUE)),
              column(4, selectInput('colorselection', NULL, NULL)),
              column(3, radioButtons('gradient', NULL, c('gradient', 'shuffle'))),
            ),
            fluidRow(
              column(5,checkboxInput('highlightby', 'Additionally highlight', TRUE)),
              column(4, selectInput('fcolorselection', NULL, NULL)),
              column(3, radioButtons('fgradient', NULL, c('gradient', 'shuffle'))),
            ),
            fluidRow(
              column(5, checkboxInput('seprows', 'Separate', width = '100%', value = TRUE)),
              column(4, selectInput('sepsel', NULL, c('days', 'weeks', 'months'), selected = 'weeks'))
            ),
            fluidRow(
              column(12,
                     virtualSelectInput(
                        inputId = "col_selection",
                        label = "Add/remove columns :",
                        choices = NULL,
                        multiple = TRUE,
                        width = "100%",
                        dropboxWrapper = "body"
                      ))
              ), 
            fluidRow(
              # column(12, checkboxInput('hl_past', 'Grey shade past events', width = '100%'))
              column(12, radioButtons('hl_past', label = 'Past events', selected = 'grey', choices = c('show', 'grey', 'hide'), width='100%', inline = TRUE))
            ), 
            fluidRow(
              column(12, checkboxInput('unique', 'Group events if all but location identical', value=TRUE, width = '100%'))
            ), 
            fluidRow(
              column(12, checkboxInput('unique2', 'Group events if course is identical', width = '100%'))
            ), 
            fluidRow(
              column(12,
                     HTML('<strong>Export</strong>'), 
                     hrs)
            ),
            fluidRow(
              column(6, downloadButton("downloadXL", "Download as Excel")),
              column(6, downloadButton("downloadImage", "Download as Image"))
            )
          )
      )
    ),
    mainPanel(
      br(),
      div(style='max-width:90vw', 
        DT::DTOutput('table')
      )
    )
  )
)