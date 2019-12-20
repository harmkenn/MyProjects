library(shiny)
library(plotly)

x1<- as.Date(c("2017-01-01", "2017-02-01", "2017-03-01", "2017-04-01", "2017-05-01", "2017-06-01"))
y1 <- c(1,2,3,4,5,6)

#bar graph
graph1 <-function(){ 
     plot_ly(x = x1,y = y1,
        type = 'bar')

}
# scatter lines
graph2 <- function(){
     plot_ly(x=x1, y=y1,
            type = 'scatter',
            mode = 'lines')
}



ui <- fluidPage(
titlePanel("Title"),
sidebarPanel("side panel",
checkboxInput("barGraph", "Bar Graph"),
verbatimTextOutput("conditionalInput"),
uiOutput('conditionlInput')
),
mainPanel("main panel",
plotlyOutput("AhtPlot")
)
)

server <- function(input, output,session){


output$AhtPlot<-renderPlotly({
if(input$barGraph == TRUE)
{graph1()} else {
graph2()    
}
})


output$conditionalInput <- renderText({input$barGraph})

}


shinyApp(ui, server)