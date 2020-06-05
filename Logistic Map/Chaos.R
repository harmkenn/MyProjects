library(shiny)
library(shinydashboard)
box <- shinydashboard::box
library(tidyverse)
theme_set(theme_bw())



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
     sliderInput("r","Rate",0,9,0,step=.1,animate = TRUE),
     sliderInput("s","Starting x",0,9,0,step=.1,animate = TRUE),
     menuItem("Data Sets", tabName = "datasets")
    ) #End sidebarMenu
  ), #End dashboardSidebar
  
  # <<<<<<<<<<<<<End Sidebar
  # >>>>>>>>>>>>>Dashboard Body
  
  dashboardBody(
    tabItems(
      
################### Logistic Tab UI
# https://en.wikipedia.org/wiki/Logistic_map     
      tabItem("log",
        fluidRow(
          column(width = 12,
            box(title = "Data Input", width = NULL, status = "primary",
              plotOutput("log_plot")
            ) #Ebox
          ) #Ecolumn
        ) #EfluidRow
      ), #EtabItem log
      tabItem("datasets", "All the pre-built datasets will go here") #EtabItem Datasets
    ) #End tabItems
  ) #EdashboardBody
) #EdashboardPage

# >>>>>>>>>>> Start of the Server
server <- function(input, output, session) {
  

} #end of the server

# Run the application 
shinyApp(ui = ui, server = server)

