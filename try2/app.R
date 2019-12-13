library(shiny)
library(rhandsontable)
library(data.table)

X <- c("", "", "")
Y <- c("", "", "")

daten <- data.table(X, Y)

ui <- fluidPage(
    rHandsontableOutput(outputId = "tabelle"),
    plotOutput(outputId = "grafik")
)

server <- function(input, output){
    data.in <- reactiveValues(values = daten)
    output$tabelle <- renderRHandsontable({
        rhandsontable(data.in$values)
    })
    
    observeEvent(eventExpr = input$tabelle, {
        data.in$values <- hot_to_r(input$tabelle)
        output$grafik <- renderPlot({
            if(!is.null(tryCatch(plot(data.in$values), error = function(e){})))
            {plot(data.in$values)}
        })
    })
}

shinyApp(ui, server)