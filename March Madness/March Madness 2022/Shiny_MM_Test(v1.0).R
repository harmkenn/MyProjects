#library(shiny)
#library(shinydashboard)
#library(tidyverse)
#library(plotly)
pacman::p_load(shiny,shinydashboard,tidyverse,plotly,DT,formattable,magrittr)

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
       menuItem("Brackets", tabName = "brackets"),
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
########################### Starts Brackets Tab
      tabItem("brackets",
              fluidRow(
                column(width = 12,
                  box(title = "Brackets since 1985", width = NULL, status = "primary",
                    sliderInput("sldr_year","Year",2008,2021,2021,step=1,width = 500,ticks = FALSE),
                    formattableOutput("tbl_brackets")       
                  ) ############# End box
                ), ############## End column      
              ) ################# End of fluidrow
      ), ################### End Brackets Tab
########################### Start Seed History Tab
      tabItem("seed_history",
        fluidRow(
          column(width = 12,
             box(title = "Seed History since 1985", width = NULL, status = "primary",
                 DTOutput("tbl_seed_history")       
             ) ############# End box
          ), ############## End column      
        ) ################# End of fluidrow
      ) ################### End Seed History Tab     
    ) ##################### End tabItems
  ) ####################### End dashboard Body
) ######################### End dashboard Page

########################### End UI

########################### Start of the Server
server <- function(input, output, session) {
########################### Start of All Games Tab
  output$tbl_all_games <- renderDT(AllGames%>%select(c(2:11)),rownames = FALSE)
########################### End of All Games Tab
########################### Start of Brackets Tab
  observeEvent(input$sldr_year,{
    year <- input$sldr_year
    games <- AllGames %>% filter(Year == year) %>% 
      mutate(top = paste(W.Seed,Winner,sep = " "),bottom = paste(L.Seed,Loser,sep = " "))
    top <- games %>% select(Game,top,W.Score) %>% set_colnames(c("Game","Team","Score"))
    bottom <- games %>% select(Game,bottom,L.Score)%>% set_colnames(c("Game","Team","Score"))
    this_year <- rbind(top,bottom)%>%arrange(Game,desc(Score))%>%mutate(all=paste(Team,Score,sep=" "))
    col1 <- this_year%>%filter(Game %in% (1:16)) %>%select(all)
    col2 <- this_year%>%filter(Game %in% (33:40)) %>%select(all)
    output$tbl_brackets <- renderFormattable(col1)  
  })
  
########################### End of Brackets Tab
} ######################## End of the server

########################## Run the application 
shinyApp(ui = ui, server = server)