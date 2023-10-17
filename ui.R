library(shiny)
library(DT)
library(calendar)
library(tidyverse)
library(lubridate)

ui <- fluidPage(
  shiny::fileInput('file', 'Select .ical file', multiple = FALSE, accept = '.ics'),
  DT::DTOutput('table')
)
