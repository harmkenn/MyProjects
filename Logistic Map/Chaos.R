library(shiny)
library(shinydashboard)
library(plotly) 
box <- shinydashboard::box
library(tidyverse)
theme_set(theme_bw())

load("lgmap.rda")

# >>>>>>>>>>>>>>>Start of UI

ui <- dashboardPage(
  dashboardHeader(title = "Chaos",titleWidth = "450px",
                  tags$li(class = "dropdown",tags$a("by Ken Harmon")), 
                  dropdownMenu(type = "messages", badgeStatus = "success",messageItem("Support Team",
                                                                                      "harmkenn@gmail.com",time = "5 mins")),dropdownMenuOutput(outputId = "notifications")),
  
  # >>>>>>>>>>>>>>>Side Bar  
  
  dashboardSidebar(width = 150,
    sidebarMenu(
     menuItem("Logistic", tabName = "log"),
     sliderInput("r","Rate",0,4,0,step=.1,animate = TRUE),
     sliderInput("s","Starting x",0,.9,.25,step=.01),
     menuItem("Log Set", tabName = "logset")
    ) #End sidebarMenu
  ), #End dashboardSidebar
  
  # <<<<<<<<<<<<<End Sidebar
  # >>>>>>>>>>>>>Dashboard Body
  
  dashboardBody(
    tabItems(
      
################### Logistic Tab UI
# https://en.wikipedia.org/wiki/Logistic_map     
      tabItem("log",
        fluidRow(withMathJax(),
          column(width = 12,
            box(title = "Logistic", width = NULL, status = "primary",
              uiOutput("formula"),
              plotOutput("log_plot")
            ) #Ebox
          ) #Ecolumn
        ) #EfluidRow
      ), #EtabItem log
      tabItem("logset", 
        fluidRow(withMathJax(),
          column(width = 12,
            box(title = "Logistic Map", width = NULL, status = "primary",
              uiOutput("formula2"),
                plotlyOutput("log_map")
              ) # End Box
            ) # End Column
        )# End Fluid Row
      ) #EtabItem logset
    ) #End tabItems
  ) #EdashboardBody
) #EdashboardPage

# >>>>>>>>>>> Start of the Server
server <- function(input, output, session) {
  output$log_map <- renderPlotly({plot_ly(lgmap, x = ~r, y = ~x,frame = ~r, type = 'scatter', mode = 'markers')})
  output$formula <- renderUI({
    my_calculated_value <- 5
    withMathJax(paste0("$${ x }_{ n+1 }=r{ x }_{ n }( 1-{ x }_{ n })$$"))
  }) 

  
  observeEvent(input$r | input$s, {
    logx <- data.frame(n=numeric(),x=numeric())
    r <- input$r
    s <- input$s
    logx[1,1] <- 1
    logx[1,2] <- r*s*(1-s)
    for(n in 2:50){
      logx[n,1] <- n
      logx[n,2] <- r*logx[n-1,2]*(1-logx[n-1,2])
    }
    output$log_plot <- renderPlot({logx %>% ggplot(aes(n,x)) + geom_point() + geom_line() + ylim(c(0,1))})
    
  }) #End of ObserveEvent Slider
} #end of the server

# Run the application 
shinyApp(ui = ui, server = server)

