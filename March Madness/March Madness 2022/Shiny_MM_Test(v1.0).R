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
                    sliderInput("sldr_year","Year",1985,2021,2021,step=1,width = 500,ticks = FALSE),
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
    slot <- games %>% filter(Round == 1) %>% mutate(W.Slot = Game + W.Seed/100, L.Slot = Game + L.Seed/100)
    slotsort <- rbind(slot %>% select(top,W.Slot) %>% set_colnames(c("sTeam","Slot")),
                      slot %>% select(bottom,L.Slot) %>% set_colnames(c("sTeam","Slot")))
    games <- left_join(games,slotsort,by = c("top"="sTeam"))
    games <- left_join(games,slotsort,by = c("bottom"="sTeam"))
    top <- games %>% select(Game,W.Seed,top,W.Score,Slot.x) %>% set_colnames(c("Game","Seed","Team","Score","Slot"))
    bottom <- games %>% select(Game,L.Seed,bottom,L.Score,Slot.y)%>% set_colnames(c("Game","Seed","Team","Score","Slot"))
    this_year <- rbind(top,bottom)%>%filter(Game %in% 1:63) %>% arrange(Game,Slot)%>%mutate(all=paste(Team,Score,sep=" "))
    bracket <- data.frame(matrix("", nrow = 32, ncol = 11))
    colnames(bracket) <- c("W.Round 1","W.Round 2","W.Round 3","W.Round 4","W.Round 5","Round 6","E.Round 5","E.Round 4","E.Round 3","E.Round 2","E.Round 1")
    for (i in 1:32){
      bracket[i,1] <- this_year$all[i]
      bracket[i,11] <- this_year$all[i+32]
    }
    for (i in 1:16){
      bracket[2*i-1,2] <- this_year$all[64+i]
      bracket[2*i-1,10] <- this_year$all[80+i]
    }
    for (i in 1:8){
      bracket[4*i-2,3] <- this_year$all[96+i]
      bracket[4*i-2,9] <- this_year$all[104+i]
    }
    for (i in 1:4){
      bracket[8*i-4,4] <- this_year$all[112+i]
      bracket[8*i-4,8] <- this_year$all[116+i]
    }
    for (i in 1:2){
      bracket[16*i-8,5] <- this_year$all[120+i]
      bracket[16*i-8,7] <- this_year$all[122+i]
    }
    bracket[16,6] <- this_year$all[125]
    bracket[17,6] <- this_year$all[126]
    bracket[3,6] <- games$Winner[63]
    bracket <- bracket %>% formattable()
    output$tbl_brackets <- renderFormattable(bracket)  
  })
  
########################### End of Brackets Tab
} ######################## End of the server

########################## Run the application 
shinyApp(ui = ui, server = server)