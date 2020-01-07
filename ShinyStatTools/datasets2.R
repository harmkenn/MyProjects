#library(xlsx)
#file.list <- list.files(pattern='*.*')
#df.list <- lapply(file.list, read_excel)
#library(plyr)
#df.list <- lapply(file.list, function(x) read.xlsx(file=x, sheetIndex=1,
#                  colIndex=1:4,as.data.frame=TRUE, header=FALSE, FILENAMEVAR=x))
#final.df <- rbind.fill(df.list)

#save(list = ls(.GlobalEnv), file = "BYUIdata.Rdata")

load("BYUIdata.Rdata")

dslist <- ls()[sapply(ls(), function(x) any(is.data.frame(get(x))))]

library(ggpmisc)
library(janitor)
library(shiny)
library(shinyjs)
library(shinydashboard)
box <- shinydashboard::box
library(tidyverse)
library(rhandsontable) 
library(ggplot2)
library(qqplotr) 
library(cowplot)
library(grid)
library(gridExtra)
theme_set(theme_bw())

ui=fluidPage(
  selectInput("data_input",label="Select data",
              choices=dslist),
  rHandsontableOutput("table_output")
)

server=function(input,output) {

#  getdata <- reactive({ get(input$data_input, .GlobalEnv) })
#  a <- as.data.frame(getdata())
#  output$table_output <- renderTable({getdata()})
  
  observeEvent(input$data_input,{
    getdata <- get(input$data_input, .GlobalEnv)
    output$table_output <- renderRHandsontable({rhandsontable(getdata)})
  })
  
}

    shinyApp(ui, server)