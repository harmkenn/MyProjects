#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(openintro)
#library(gridExtra)
#library(BHH2)
library(tidyverse)
library(psych)
library(pander)
library(car)
NBAplayers <- read.csv("NBAplayers.csv")

# UI
ui <- fluidPage(
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      
      # Select variables to study
      checkboxGroupInput(inputId = "selected_var",
                         label = "Select variables:",
                         choices = names(NBAplayers),
                         selected = c("height"))
      
    ),
    
    
    # Output(s)
    mainPanel(
      HTML("Select a variables, then download and/or view the data."),
      br(), br(),
      downloadButton(outputId = "download_data", label = "Download data"),
      br(), br(),
      DT::dataTableOutput(outputId = "NBAstats")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Create reactive data frame
  stats_selected <- reactive({
    req(input$selected_var)               # ensure input$selected_var is available
    NBAplayers %>% select(input$selected_var) # select columns of NBAstats
  })
  
  # Create data table
  output$NBAstats <- DT::renderDataTable({
    DT::datatable(data = stats_selected(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  # Download file
  output$download_data <- downloadHandler(
    filename = function() {
      "nbastats.csv"
    },
    content = function(file) { 
      write_csv(stats_selected(), path = file) 
    })
}
# Create a Shiny app object
shinyApp(ui = ui, server = server)