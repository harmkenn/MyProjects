library(shiny)
library(shinydashboard)
box <- shinydashboard::box
library(tidyverse)
library(rhandsontable)
library(ggplot2)
library(pander)
theme_set(theme_bw())

data.discr <- data.frame(matrix(NA_real_, nrow = 500, ncol = 1))
colnames(data.discr) <- "A"
binwidth <- 1

# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title = "Stat Tools"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Descriptive Stats", tabName = "ds"),
      menuItem("z-Test", tabName = "zTest"),
      menuItem("One Sample t-Test", 
        menuSubItem("Data", tabName = "tTestData"),
        menuSubItem("Stats", tabName = "tTestStats")),
      menuItem("Paired t-Test", tabName = "Pairedt"),
      menuItem("Two Sample t-Test", 
        menuSubItem("Data", tabName = "2tTestData"),
        menuSubItem("Stats", tabName = "2tTestStats")),
      menuItem("ANOVA", tabName = "ANOVA"),
      menuItem("One Prop z-Test", tabName = "1pzt"),
      menuItem("Two Prop z-Test", tabName = "2pzt"),
      menuItem("Chi-Square", tabName = "Chi"),
      menuItem("Linear Regression", tabName = "LR"),
      menuItem("Discrete", tabName = "Disc")
    ) #End sidebarMenu
  ), #End dashboardSidebar
  dashboardBody(
    tabItems(
      tabItem("ds",
        fluidRow(
          column(width = 3,
            box(
              title = "Data Input", width = NULL, status = "primary",
              actionButton("clear","Clear"),
              rHandsontableOutput("dt")
            ) #Ebox
          ), #Ecolumn
          column(width = 5,
            box(
              title = "Histogram", width = NULL,
              plotOutput("hist")
            ), #Ebox
          ), #Ecolumn
          column(width = 4,
            box(
              title = "Summary Statistics", width = NULL, solidHeader = TRUE,
              tableOutput("dss")
            ), #Ebox
            box(
              title = "Title 6", width = NULL, background = "maroon",
              "A box with a solid maroon background"
            ) #Ebox
          ) #Ecolumn
        ) #EfluidRow
      ), #EtabItem ds
      tabItem("zTest","zTest goes Here"), #EtabItem zTest
      tabItem("tTestData","tTestData goes Here"), #EtabItem tTestData
      tabItem("tTestStats","tTestStats goes Here"), #EtabItem tTestData
      tabItem("Pairedt","Pairedt goes Here"), #EtabItem Pairedt
      tabItem("2tTestData","2tTestData goes Here"), #EtabItem 2tTestData
      tabItem("2tTestStats","2tTestStats goes Here"), #EtabItem 2tTestStats
      tabItem("ANOVA","ANOVA goes Here"), #EtabItem ANOVA
      tabItem("1pzt","1pzt goes Here"), #EtabItem 1pzt
      tabItem("2pzt","2pzt goes Here"), #EtabItem 2pzt
      tabItem("Chi","Chi goes Here"), #EtabItem Chi
      tabItem("LR","LR goes Here"), #EtabItem LR
      tabItem("Disc","Disc goes Here") #EtabItem Disc
    ) #End tabItems
  ) #EdashboardBody
) #EdashboardPage

#Start of the Server
server <- function(input, output) {
  data.in <- reactiveValues(values = data.discr)
  output$dt <- renderRHandsontable({
    rhandsontable(data.in$values)
  })
  observeEvent(eventExpr = input$dt, {
    data.in$values <- hot_to_r(input$dt)
    if(sum(!is.na(data.in$values[,1]))>1){
    hist.x <- data.in$values[!is.na(data.in$values)]
    count <- length(hist.x)
    bins <- ceiling(1+3.322*log10(count))
      if (count > 2) {binwidth <- (max(hist.x)-min(hist.x)+2)/(bins-2)}
      hist.df <- data.frame(hist.x)
      output$hist <- renderPlot({
        if(!is.null(tryCatch(ggplot(hist.df), error = function(e){}))){
          hist.df %>%
            ggplot(aes(x=hist.df[,1])) + 
            geom_histogram(color="darkblue", fill="lightblue",binwidth=binwidth)
        } #Eif
      }) #Eoutput$hist
      dss <- matrix(c(length(hist.x),mean(hist.x)),ncol=1,nrow=2)
      rownames(dss) <- c("Count","Mean")
      output$dss <- renderTable({dss},rownames = TRUE,colnames=FALSE)
    }#Eif
  }) #EobserveEvent
  observeEvent(eventExpr = input$clear, {
    data.discr <- data.frame(matrix(NA_real_, nrow = 500, ncol = 1))
    colnames(data.discr) <- "A"
    data.in <- reactiveValues(values = data.discr)
    output$dt <- renderRHandsontable({rhandsontable(data.in$values)})
  }) #EobserveEvent
} #end of the server

# Run the application 
shinyApp(ui = ui, server = server)