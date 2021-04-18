#library(shiny)
#library(shinydashboard)
#library(tidyverse)
#library(plotly)
pacman::p_load(shiny,shinydashboard,tidyverse,plotly,DT)

AllGames <- read.csv("All Games.csv")

########################### Start of UI

ui <- dashboardPage(
  dashboardHeader(title = "Shiny March Madness Test v1.0",titleWidth = "450px",
                  tags$li(class = "dropdown",tags$a("by Ken Harmon")),
                  dropdownMenuOutput(outputId = "notifications")),
  
########################### Start Side Bar  
  
  dashboardSidebar(width = 150,
     sidebarMenu(
       menuItem("All Games", tabName = "all_games"),
       menuItem("Seed History", tabName = "seed_history")
     ) #################### End sidebarMenu
  ), ###################### End dashboardSidebar
  
########################### End Side Bar
########################### Start Dashboard Body
  dashboardBody(
########################### Starts Tab Items    
    tabItems(
########################### Starts All Games Tab
      tabItem("all_games",
        fluidRow(
          column(width = 12,
            box(title = "All Games since 1985", width = NULL, status = "primary",
              DTOutput("tbl_all_games")       
            ) ############# End box
          ), ############## End column      
        ) ################# End of fluidrow
      ), ################### End All Games Tab
      tabItem("seed_history",
        fluidRow(
          column(width = 12,
             box(title = "Seed History since 1985", width = NULL, status = "primary",
                 DTOutput("tbl_seed_history")       
             ) ############# End box
          ), ############## End column      
        ) ################# End of fluidrow
      ) ################### End All Games Tab     
    ) ##################### End tabItems
  ) ####################### End dashboard Body
) ######################### End dashboard Page

########################### End UI

########################### Start of the Server
server <- function(input, output, session) {
  output$tbl_all_games <- renderDT(AllGames)
} ######################## End of the server

########################## Run the application 
shinyApp(ui = ui, server = server)