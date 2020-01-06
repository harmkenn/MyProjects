library(xlsx)
#file.list <- list.files(pattern='*.*')
#df.list <- lapply(file.list, read_excel)
library(plyr)
df.list <- lapply(file.list, function(x) read.xlsx(file=x, sheetIndex=1,
                  colIndex=1:4,as.data.frame=TRUE, header=FALSE, FILENAMEVAR=x))
final.df <- rbind.fill(df.list)

#save(list = ls(.GlobalEnv), file = "data.Rdata")

load("/cloud/project/ShinyStatTools/datasetsrda/data.Rdata")

dslist <- ls()[grepl('data.frame', sapply(ls(), function(x) class(get(x))))]



library(shiny)

ui=fluidPage(
  selectInput("data_input",label="Select data",
              choices=dslist),
  tableOutput("table_output")
)

server=function(input,output) {

  getdata <- reactive({ get(input$data_input, .GlobalEnv) })
  output$table_output <- renderTable({head(getdata())})
}

    shinyApp(ui, server)