library(shinydashboard)
library(tidyverse)
library(rhandsontable)

data.discr <- data.frame(matrix(NA_real_, nrow = 500, ncol = 1))
colnames(data.discr) <- "A"

# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title = "Stat Tools"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Descriptive Stats", tabName = "ds"),
      menuItem("One Sample t-Test", tabName = "tTest")
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 3,
             box(
               title = "Data Input", width = NULL, status = "primary",
               actionButton("clear","Clear"),
               rHandsontableOutput("dt")
             )
      ),
      
      column(width = 5,
             box(
               status = "warning", width = NULL,
               plotOutput("hist")
             ),
      ),
      
      column(width = 4,
             box(
               title = "Title 2", width = NULL, solidHeader = TRUE,
               "Box content"
             ),
             box(
               title = "Title 6", width = NULL, background = "maroon",
               "A box with a solid maroon background"
             )
      )
    )
  )
)

#Start of the Server
server <- function(input, output) {
  data.in <- reactiveValues(values = data.discr)
  output$dt <- renderRHandsontable({
    rhandsontable(data.in$values)
  })
  
  observeEvent(eventExpr = input$dt, {
    data.in$values <- hot_to_r(input$dt)
    hist.x <- data.in$values[,1]
    output$hist <- renderPlot({
      if(!is.null(tryCatch(hist(hist.x), error = function(e){}))){
        count <- sum(!is.na(hist.x))
        bins <- ceiling(1+3.322*log10(count))
        hist(hist.x, breaks = bins, col = 'darkgray', border = 'white')
      }
    })
  })
} #end of the server

# Run the application 
shinyApp(ui = ui, server = server)