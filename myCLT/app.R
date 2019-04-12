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
NBAplayers <- read.csv("NBAplayers.csv")

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  headerPanel("Central Limit Theorem for NBAstats"),
  sidebarPanel(
    radioButtons(inputId = "stat", label = "NBA Stat:", choices = names(NBAplayers)),width=2
    ),
  mainPanel(
    sliderInput(inputId = "bins", label = "Number of bins:", min = 1, max = 50, value = 20),
    plotOutput(outputId = "distPlot"),
    plotOutput(outputId = "qqPlot"),
    verbatimTextOutput("summary"),
    sliderInput("n", "Sample size:", value = 1, min = 1, max = 100),
    sliderInput(inputId = "dsmbins", label = "Number of bins:", min = 1, max = 50, value = 20),
    plotOutput(outputId = "dsm"),
    verbatimTextOutput("summaryxbars")
  )
)

# Define server function required to create the scatterplot-
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    x <- NBAplayers %>% select(input$stat) %>% filter_all(any_vars(. != 0)) %>% pull() %>% na.omit() %>% as.numeric() # select columns of NBA
    
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white", xlab = input$stat,  main = "")
    
  })
  output$qqPlot <- renderPlot({
    
    x <- NBAplayers %>% select(input$stat) %>% filter_all(any_vars(. != 0)) %>% pull() %>% na.omit() %>% as.numeric() # select columns of NBA

    qqnorm(x)
    
  })
  output$summary <- renderPrint({
    x <- NBAplayers %>% select(input$stat) %>% filter_all(any_vars(. != 0)) %>% pull() %>% na.omit() %>% as.numeric()
    
    pander(describe(x))
  })
  
  # Distribution of Sample Means   
  
  output$dsm <- renderPlot({
    
    x <- NBAplayers %>% select(input$stat) %>% filter_all(any_vars(. != 0)) %>% pull() %>% na.omit() %>% as.numeric() # select columns of NBA
    samplexs <- sample(x,10000*input$n,replace = TRUE)
    msx <- as.data.frame(matrix(samplexs,10000,byrow = TRUE))
    xbars <- apply(msx,1,mean)
    
    bins <- seq(min(xbars), max(xbars), length.out = input$dsmbins + 1)
    bw <- max(xbars) - min(xbars)
    #
    #ggplot(as.data.frame(xbars), aes(xbars)) + geom_dotplot(stackgroups = TRUE,binwidth = bw/100,dotsize = .2)
    #d <- density(xbars)                                  
    #plot(d, main="Kernel Density of Miles Per Gallon")       
    #polygon(d, col="red", border="blue")  
    hist(xbars, breaks = bins, col = "#75AADB", border = "white", xlab = input$stat,  main = "")
    
  })
  output$summaryxbars <- renderPrint({
    x <- NBAplayers %>% select(input$stat) %>% filter_all(any_vars(. != 0)) %>% pull() %>% na.omit() %>% as.numeric()
    samplexs <- sample(x,10000*input$n,replace = TRUE)
    msx <- as.data.frame(matrix(samplexs,10000,byrow = TRUE))
    xbars <- apply(msx,1,mean)
    pander(describe(xbars))
  })
}


# Create a Shiny app object
shinyApp(ui = ui, server = server)