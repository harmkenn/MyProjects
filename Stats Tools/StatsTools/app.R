library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Stat Tools by Ken Harmon", windowTitle = "Stat Tools"),
  tabsetPanel(
    tabPanel("Descriptive",
      sidebarLayout(
        sidebarPanel(
        ), #end of the sidebarPanel
        mainPanel(
        ) #end of the mainPanal 3
      ) #end sidebarLayout
    ) #end of tabpanel "Descriptive"
  ) #end of the tabset panel
) #end of the ui

server <- function(input, output) {

} #end of the server

# Run the application 
shinyApp(ui = ui, server = server)
