library(shiny)
library(shinydashboard)
library(RWeather)

header <- dashboardHeader(title =  'Current weather')
sidebar <- dashboardSidebar()
boxCity <-
    box(selectInput(
        'station', 'City:', choices = c(
            'Atlanta' = 'KATL',  'Chicago' = 'KORD', 'Fairbanks' = 'PAFA', 'New York' = 'KJFK', 'Phoenix' =
                'KPHX'
        ), selected = 'KATL'
    ))
boxCondition <-
    box(title = 'Current conditions: ', textOutput('condition'), background = 'blue')
boxTime <-  box(textOutput('time'))
row1 <-  fluidRow(boxCity)
row2 <-  fluidRow(boxCondition, boxTime)
row3 <-  fluidRow(valueBoxOutput("vboxC"), valueBoxOutput("vboxF"))
body <- dashboardBody(row1,row2,row3)

ui <- dashboardPage(header,sidebar,body)

server <- function(input, output) {
    output$condition <-
        renderText({
            getWeatherFromNOAA(station_id = input$station, message = FALSE)$condition
        })
    output$time <-
        renderText({
            getWeatherFromNOAA(station_id = input$station, message = FALSE)$observation_time
        })
    output$vboxC <- renderValueBox({
        t <-
            as.numeric(getWeatherFromNOAA(station_id = input$station, message = FALSE)$temp_c)
        if (t  > 30)
        {
            valueBox(t, width = 3, subtitle = 'C', color = 'red')
        }
        else if (t < 10)
        {
            valueBox(t, width = 3, subtitle = 'C', color = 'blue')
        }
        else {
            valueBox(t, width = 3, subtitle = 'C', color = 'yellow')
        }
    })
    output$vboxF <- renderValueBox({
        t <-
            as.numeric(getWeatherFromNOAA(station_id = input$station, message = FALSE)$temp_f)
        if (t  > 86)
        {
            valueBox(t, width = 3, subtitle = 'F', color = 'red')
        }
        else if (t < 50)
        {
            valueBox(t, width = 3, subtitle = 'F', color = 'blue')
        }
        else {
            valueBox(t, width = 3, subtitle = 'F', color = 'yellow')
        }
    })
}

shinyApp(ui, server)