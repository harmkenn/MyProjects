

library(shiny)

load("northdes.rda")
columns <- names(northdes)

# Define UI for application that draws a histogram
ui <- fluidPage(
    selectInput("xaxis","X-Axis",columns),
    selectInput("yaxis","Y-Axis",columns)
    
        
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatterPlot <- renderPlotly({

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
