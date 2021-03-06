#library(shiny)
#library(shinydashboard)
#library(tidyverse)
#library(plotly)

pacman::p_load(shiny,shinydashboard,tidyverse,plotly,DT,formattable,magrittr,gt)


#AllGames <- read.csv("All Games.csv")
#save(AllGames, file="AllGames.rda")
load("AllGames.rda")

# For Seed History
seed.history <- data.frame(rbind(table(AllGames$W.Seed,AllGames$Round)))%>%select(1:6)
seed.history$exp_wins <- rowSums(seed.history)/144
SeedSum <- gt(seed.history,,,TRUE)%>% 
  data_color(
    columns = 2:8, 
    colors = scales::col_numeric(
      palette = c("white","blue") %>% as.character(),
      domain = NULL
    )
  ) 
# For Team Wins
R1 <- AllGames %>% filter(Round == 1)
R1L <- data.frame(rbind(table(R1$Loser,R1$Year)))%>% add_rownames()
R2 <- AllGames %>% filter(Round == 2)
R2L <- data.frame(rbind(2*table(R2$Loser,R2$Year)))%>% add_rownames()
R3 <- AllGames %>% filter(Round == 3)
R3L <- data.frame(rbind(3*table(R3$Loser,R3$Year)))%>% add_rownames()
R4 <- AllGames %>% filter(Round == 4)
R4L <- data.frame(rbind(4*table(R4$Loser,R4$Year)))%>% add_rownames()
R5 <- AllGames %>% filter(Round == 5)
R5L <- data.frame(rbind(5*table(R5$Loser,R5$Year)))%>% add_rownames()
R6 <- AllGames %>% filter(Round == 6)
R6L <- data.frame(rbind(6*table(R6$Loser,R6$Year)))%>% add_rownames()
R6W <- data.frame(rbind(7*table(R6$Winner,R6$Year)))%>% add_rownames()
team.wins <- rbind(R1L,R2L,R3L,R4L,R5L,R6L,R6W)
team.wins <- team.wins %>% group_by(rowname) %>% summarise_all(sum)
colnames(team.wins) <- c("Team",seq(1985,2019,1),2021)
team.wins[,2:37] <- team.wins[,2:37] - 1
team.wins[team.wins == -1] <- NA
#team.wins <- team.wins[,order(ncol(team.wins):1)]


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
       menuItem("Seed History", tabName = "seed_history"),
       menuItem("Team Wins", tabName = "team_wins")
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
                gt_output("tbl_seed_history")       
             ) ############# End box
          ), ############## End column      
        ) ################# End of fluidrow
      ), ################## End Seed History Tab  
########################### Start Seed History Tab
tabItem("team_wins",
        fluidRow(
          column(width = 12,
                 box(title = "Team Wins since 1985", width = NULL, status = "primary",
                     DTOutput("tbl_team_wins")       
                 ) ############# End box
          ), ############## End column      
        ) ################# End of fluidrow
) ################### End Team Wins Tab  
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
########################### Start of Seed History  
  output$tbl_seed_history <- render_gt(SeedSum)
########################### End of Seed History 
########################### Start of Team Wins  
  output$tbl_team_wins <- renderDT(team.wins,options = list(scrollX = TRUE,
                                                            fixedColumns = list(leftColumns = 1),
                                                            autoWidth = TRUE,
                                                            columnDefs = list(list(width = '130px', targets = c(0)))
                                                            ),extensions = 'FixedColumns', rownames = FALSE)
########################### End of Team Wins
} ######################## End of the server

########################## Run the application 
shinyApp(ui = ui, server = server)