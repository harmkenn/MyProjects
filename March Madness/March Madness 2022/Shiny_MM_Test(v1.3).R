#library(shiny)
#library(shinydashboard)
#library(tidyverse)
#library(plotly)
#library(pacman)
pacman::p_load(shiny,shinydashboard,tidyverse,plotly,DT,formattable,magrittr,gt,caret, e1071,glmnet)



load("AllGames.rda")
load("TeamRank.rda")
load("AllCombine.rda")
load("JTCombine.rda")

 # For Seed History
seed.history <- data.frame(rbind(table(AllGames$W.Seed,AllGames$Round)))%>%select(1:6)
seed.history$exp_wins <- rowSums(seed.history)/144
SeedSum <- gt(seed.history,,,TRUE)%>% 
    data_color(
        columns = 1:7, 
        colors = scales::col_numeric(
            palette = c("white","blue") %>% as.character(),
            domain = NULL
        )
    ) 
# For Team Wins
R1 <- AllGames %>% filter(Round == 1)
R1L <- data.frame(rbind(table(R1$Loser,R1$Year)))%>% rownames_to_column()
R2 <- AllGames %>% filter(Round == 2)
R2L <- data.frame(rbind(2*table(R2$Loser,R2$Year)))%>% rownames_to_column()
R3 <- AllGames %>% filter(Round == 3)
R3L <- data.frame(rbind(3*table(R3$Loser,R3$Year)))%>% rownames_to_column()
R4 <- AllGames %>% filter(Round == 4)
R4L <- data.frame(rbind(4*table(R4$Loser,R4$Year)))%>% rownames_to_column()
R5 <- AllGames %>% filter(Round == 5)
R5L <- data.frame(rbind(5*table(R5$Loser,R5$Year)))%>% rownames_to_column()
R6 <- AllGames %>% filter(Round == 6)
R6L <- data.frame(rbind(6*table(R6$Loser,R6$Year)))%>% rownames_to_column()
R6W <- data.frame(rbind(7*table(R6$Winner,R6$Year)))%>% rownames_to_column()
team.wins <- rbind(R1L,R2L,R3L,R4L,R5L,R6L,R6W)
team.wins <- team.wins %>% group_by(rowname) %>% summarise_all(sum)
colnames(team.wins) <- c("Team",seq(1985,2019,1),2021)
team.wins[,2:37] <- team.wins[,2:37] - 1
team.wins[team.wins == -1] <- NA
#team.wins <- team.wins[,order(ncol(team.wins):1)]


########################### Start of UI

ui <- dashboardPage(
    dashboardHeader(title = "Shiny March Madness Test v1.3",titleWidth = "450px",
                    tags$li(class = "dropdown",tags$a("by Ken Harmon")),
                    dropdownMenuOutput(outputId = "notifications")),
    
    ########################### Start Side Bar  
    
    dashboardSidebar(width = 150,
                     sidebarMenu(
                         menuItem("All Games", tabName = "all_games"),
                         menuItem("Brackets", tabName = "brackets"),
                         menuItem("Seed History", tabName = "seed_history"),
                         menuItem("Team Wins", tabName = "team_wins"),
                         menuItem("Team Rank", tabName = "team_rank"),
                         menuItem("Back Predict", tabName = "back_predict"),
                         menuItem("Full Predict", tabName = "full_predict")
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
            ########################### Start Team Wins Tab
            tabItem("team_wins",
                    fluidRow(
                        column(width = 12,
                               box(title = "Team Wins since 1985", width = NULL, status = "primary",
                                   DTOutput("tbl_team_wins")       
                               ) ############# End box
                        ), ############## End column      
                    ) ################# End of fluidrow
            ), ################### End Team Wins Tab
            ########################### Start Team Wins Tab
            tabItem("team_rank",
                    fluidRow(
                        column(width = 12,
                               box(title = "Team Rank since 2008", width = NULL, status = "primary",
                                   DTOutput("tbl_team_rank")       
                               ) ############# End box
                        ), ############## End column      
                    ) ################# End of fluidrow
            ), ################### End Team Rank Tab
            ########################### Start Back Predict Tab
            tabItem("back_predict",
                    fluidRow(
                        column(width = 12,
                            box(title = "Predicting Back Per Game", width = NULL, status = "primary",
                                sliderInput("sldr_year_p","Year",2008,2021,2021,step=1,width = 500,ticks = FALSE),
                                textOutput("txt_ESPN"),
                                formattableOutput("tbl_back_predict")     
                        ) ############# End box
                        ), ############## End column      
                    ) ################# End of fluidrow
            ), ################### End Back Predict Tab
            ########################### Start Full Back Predict Tab
            tabItem("full_predict",
                    fluidRow(
                        column(width = 12,
                               box(title = "Predicting Back Full Bracket", width = NULL, status = "primary",
                                   sliderInput("sldr_year_f_p","Year",2008,2021,2021,step=1,width = 500,ticks = FALSE),
                                   textOutput("txt_ESPN_f"),
                                   formattableOutput("tbl_full_predict")     
                               ) ############# End box
                        ), ############## End column      
                    ) ################# End of fluidrow
            ) ################### End Full Back Predict Tab
        ) ##################### End tabItems
    ) ####################### End dashboard Body
) ######################### End dashboard Page

########################### End UI

########################### Start of the Server
server <- function(input, output, session) {
    ########################### Start of All Games Tab
    output$tbl_all_games <- renderDT(AllGames%>%select(c(2:11)),options = list(scrollX = TRUE,scrollY = TRUE),rownames = FALSE)
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
    ########################### Start of Team Rank  
    output$tbl_team_rank <- renderDT(TeamRank,options = list(scrollX = TRUE,
                                                              fixedColumns = list(leftColumns = 2),
                                                              autoWidth = TRUE,
                                                              columnDefs = list(list(width = '130px', targets = c(1)))
    ),extensions = 'FixedColumns', rownames = FALSE)
    ########################### End of Team Rank
    ########################### Start of Back Predict Tab
    observeEvent(input$sldr_year_p,{
        pyear <- input$sldr_year_p
        model_games <- AllCombine %>% filter(Year != pyear)
        test_games <- AllCombine %>% filter(Year == pyear)
        model <- lm(amv ~ .,data =model_games[,c(-5,-6,-8,-9,-10,-30)])
        model <- train(amv ~., data = model_games[,c(-5,-6,-8,-9,-10,-30)], method = "glmnet",
                       trControl = trainControl("cv", number = 10),tuneLength = 10)
        test_games$pmv <- predict(model, test_games, type = "raw")

        show_predict <- test_games %>% mutate(Favored = paste(F.Seed,F.Team,sep = " "),
                                              Underdog = paste(U.Seed,U.Team,sep = " ")) %>%
            select (c(2,3,50:53)) %>% filter(Game >= 0)
        show_predict$Actual_Winner <- ifelse(show_predict$amv >= 0, show_predict$Favored,show_predict$Underdog)
        show_predict$Predicted_Winner <- ifelse(show_predict$pmv >= 0, show_predict$Favored,show_predict$Underdog)
        show_predict$ESPN_Points <- ifelse(show_predict$Actual_Winner == show_predict$Predicted_Winner,10*2^(as.integer(show_predict$Round)-1),0)
        output$tbl_back_predict <- renderFormattable(show_predict %>% formattable())  
        output$txt_ESPN <- renderText(sum(show_predict$ESPN_Points))
    })
    
    ########################### End of Brackets Tab
    ########################### Start of Full Predict Tab
    observeEvent(input$sldr_year_f_p,{
        pyear <- input$sldr_year_f_p
        if (pyear == 2020) {pyear <- 2021}
        FUGames <- read.csv("FUAllGames.csv")
        stage1 <- FUGames %>% filter(Year >= 2008) %>% select(c(-3))
        stage1$amv <- stage1$A.F.Score - stage1$A.U.Score
        stage2 <- left_join(stage1, JTCombine, by = c("Year"="Year","Round"="Round","Game"="Game","A.F.Seed"="Seed","A.F.Team"="Team"))
        stage3 <- left_join(stage2, JTCombine, by = c("Year"="Year","Round"="Round","Game"="Game","A.U.Seed"="Seed","A.U.Team"="Team"),suffix = c("_F","_U")) 
        model.years <- stage3 %>% filter(Year != pyear,Round != 'PI')
        #p_model <- lm(amv ~ .,data =model.years[,c(4,7,10,12:30,32:50)])
        p_model <- train(amv ~., data = model.years[,c(4,7,10,12:30,32:50)], method = "glmnet",
                       trControl = trainControl("cv", number = 2),tuneLength = 2)
   
        
        # Round 1
        
        R1 <- stage3 %>% filter(Year == pyear, Round == 1)
        R1$pmv <- predict(p_model, R1, type = "raw")
        R1$A.W.Seed <- ifelse(R1$amv >=0, R1$A.F.Seed, R1$A.U.Seed)
        R1$A.W.Team <- ifelse(R1$amv >=0, R1$A.F.Team, R1$A.U.Team)
        R1$P.W.Seed <- ifelse(R1$pmv >=0, R1$A.F.Seed, R1$A.U.Seed)
        R1$P.W.Team <- ifelse(R1$pmv >=0, R1$A.F.Team, R1$A.U.Team)
        p_show <- R1 %>% mutate (A.Favored = paste(A.F.Seed,A.F.Team,sep = " "),
                                 A.Underdog = paste(A.U.Seed,A.U.Team,sep = " "),
                                 A.Winner = paste(A.W.Seed,A.W.Team,sep = " "),
                                 P.Favored = A.Favored,
                                 P.Underdog = A.Underdog,
                                 P.Winner = paste(P.W.Seed,P.W.Team,sep = " ")) %>% select(1,2,3,56,57,59,60,10,51,58,61)
        p_show$ESPN_Points <- ifelse(p_show$A.Winner == p_show$P.Winner,10,0)
        
        # Round 2
        
        R2 <- stage3 %>% filter(Year == pyear, Round == 2) %>% select(1:10)
        for (i in 1:16){
            R2$P.F.Seed[i] <- min(R1$P.W.Seed[2*i-1],R1$P.W.Seed[2*i])
            R2$P.F.Team[i] <- ifelse(R2$P.F.Seed[i]==R1$P.W.Seed[2*i-1],R1$P.W.Team[2*i-1],R1$P.W.Team[2*i])
            R2$P.U.Seed[i] <- max(R1$P.W.Seed[2*i-1],R1$P.W.Seed[2*i])
            R2$P.U.Team[i] <- ifelse(R2$P.U.Seed[i]==R1$P.W.Seed[2*i-1],R1$P.W.Team[2*i-1],R1$P.W.Team[2*i])
        }
        R2B <- left_join(R2, JTCombine %>% select(c(-2,-3,-4)), by = c("Year"="Year","P.F.Team"="Team"))
        R2C <- left_join(R2B, JTCombine %>% select(c(-2,-3,-4)), by = c("Year"="Year","P.U.Team"="Team"),suffix = c("_F","_U")) 
        R2C <- unique(R2C)
        R2C$pmv <- predict(p_model, R2C, type = "raw")
        R2C$A.W.Seed <- ifelse(R2C$amv >=0, R2C$A.F.Seed, R2C$A.U.Seed)
        R2C$A.W.Team <- ifelse(R2C$amv >=0, R2C$A.F.Team, R2C$A.U.Team)
        R2C$P.W.Seed <- ifelse(R2C$pmv >=0, R2C$P.F.Seed, R2C$P.U.Seed)
        R2C$P.W.Team <- ifelse(R2C$pmv >=0, R2C$P.F.Team, R2C$P.U.Team)
        p_show_2 <- R2C %>% mutate (A.Favored = paste(A.F.Seed,A.F.Team,sep = " "),
                                    A.Underdog = paste(A.U.Seed,A.U.Team,sep = " "),
                                    A.Winner = paste(A.W.Seed,A.W.Team,sep = " "),
                                    P.Favored = paste(P.F.Seed,P.F.Team,sep = " "),
                                    P.Underdog = paste(P.U.Seed,P.U.Team,sep = " "),
                                    P.Winner = paste(P.W.Seed,P.W.Team,sep = " "))
        p_show_2 $ESPN_Points <- ifelse(p_show_2$A.Winner == p_show_2$P.Winner,20,0)
        p_show_2 <- p_show_2 %>% select(colnames(p_show))
        p_show <- rbind(p_show,p_show_2)
        
        # Round 3
        
        R3 <- stage3 %>% filter(Year == pyear, Round == 3) %>% select(1:10)
        for (i in 1:8){
            R3$P.F.Seed[i] <- min(R2C$P.W.Seed[2*i-1],R2C$P.W.Seed[2*i])
            R3$P.F.Team[i] <- ifelse(R3$P.F.Seed[i]==R2C$P.W.Seed[2*i-1],R2C$P.W.Team[2*i-1],R2C$P.W.Team[2*i])
            R3$P.U.Seed[i] <- max(R2C$P.W.Seed[2*i-1],R2C$P.W.Seed[2*i])
            R3$P.U.Team[i] <- ifelse(R3$P.U.Seed[i]==R2C$P.W.Seed[2*i-1],R2C$P.W.Team[2*i-1],R2C$P.W.Team[2*i])
        }
        R3B <- unique(left_join(R3, JTCombine %>% select(c(-2,-3,-4)), by = c("Year"="Year","P.F.Team"="Team")))
        R3C <- unique(left_join(R3B, JTCombine %>% select(c(-2,-3,-4)), by = c("Year"="Year","P.U.Team"="Team"),suffix = c("_F","_U"))) 
        R3C$pmv <- predict(p_model, R3C, type = "raw")
        R3C$A.W.Seed <- ifelse(R3C$amv >=0, R3C$A.F.Seed, R3C$A.U.Seed)
        R3C$A.W.Team <- ifelse(R3C$amv >=0, R3C$A.F.Team, R3C$A.U.Team)
        R3C$P.W.Seed <- ifelse(R3C$pmv >=0, R3C$P.F.Seed, R3C$P.U.Seed)
        R3C$P.W.Team <- ifelse(R3C$pmv >=0, R3C$P.F.Team, R3C$P.U.Team)
        p_show_3 <- R3C %>% mutate (A.Favored = paste(A.F.Seed,A.F.Team,sep = " "),
                                    A.Underdog = paste(A.U.Seed,A.U.Team,sep = " "),
                                    A.Winner = paste(A.W.Seed,A.W.Team,sep = " "),
                                    P.Favored = paste(P.F.Seed,P.F.Team,sep = " "),
                                    P.Underdog = paste(P.U.Seed,P.U.Team,sep = " "),
                                    P.Winner = paste(P.W.Seed,P.W.Team,sep = " "))
        p_show_3 $ESPN_Points <- ifelse(p_show_3$A.Winner == p_show_3$P.Winner,40,0)
        p_show_3 <- p_show_3 %>% select(colnames(p_show))
        p_show <- rbind(p_show,p_show_3)
        
        # Round 4
        
        R4 <- stage3 %>% filter(Year == pyear, Round == 4) %>% select(1:10)
        for (i in 1:4){
            R4$P.F.Seed[i] <- min(R3C$P.W.Seed[2*i-1],R3C$P.W.Seed[2*i])
            R4$P.F.Team[i] <- ifelse(R4$P.F.Seed[i]==R3C$P.W.Seed[2*i-1],R3C$P.W.Team[2*i-1],R3C$P.W.Team[2*i])
            R4$P.U.Seed[i] <- max(R3C$P.W.Seed[2*i-1],R3C$P.W.Seed[2*i])
            R4$P.U.Team[i] <- ifelse(R4$P.U.Seed[i]==R3C$P.W.Seed[2*i-1],R3C$P.W.Team[2*i-1],R3C$P.W.Team[2*i])
        }
        R4B <- unique(left_join(R4, JTCombine %>% select(c(-2,-3,-4)), by = c("Year"="Year","P.F.Team"="Team")))
        R4C <- unique(left_join(R4B, JTCombine %>% select(c(-2,-3,-4)), by = c("Year"="Year","P.U.Team"="Team"),suffix = c("_F","_U"))) 
        R4C$pmv <- predict(p_model, R4C, type = "raw")
        R4C$A.W.Seed <- ifelse(R4C$amv >=0, R4C$A.F.Seed, R4C$A.U.Seed)
        R4C$A.W.Team <- ifelse(R4C$amv >=0, R4C$A.F.Team, R4C$A.U.Team)
        R4C$P.W.Seed <- ifelse(R4C$pmv >=0, R4C$P.F.Seed, R4C$P.U.Seed)
        R4C$P.W.Team <- ifelse(R4C$pmv >=0, R4C$P.F.Team, R4C$P.U.Team)
        p_show_4 <- R4C %>% mutate (A.Favored = paste(A.F.Seed,A.F.Team,sep = " "),
                                    A.Underdog = paste(A.U.Seed,A.U.Team,sep = " "),
                                    A.Winner = paste(A.W.Seed,A.W.Team,sep = " "),
                                    P.Favored = paste(P.F.Seed,P.F.Team,sep = " "),
                                    P.Underdog = paste(P.U.Seed,P.U.Team,sep = " "),
                                    P.Winner = paste(P.W.Seed,P.W.Team,sep = " "))
        p_show_4 $ESPN_Points <- ifelse(p_show_4$A.Winner == p_show_4$P.Winner,80,0)
        p_show_4 <- p_show_4 %>% select(colnames(p_show))
        p_show <- rbind(p_show,p_show_4)
        
        # Round 5
        
        R5 <- stage3 %>% filter(Year == pyear, Round == 5) %>% select(1:10)
        for (i in 1:2){
            R5$P.F.Seed[i] <- min(R4C$P.W.Seed[2*i-1],R4C$P.W.Seed[2*i])
            R5$P.F.Team[i] <- ifelse(R5$P.F.Seed[i]==R4C$P.W.Seed[2*i-1],R4C$P.W.Team[2*i-1],R4C$P.W.Team[2*i])
            R5$P.U.Seed[i] <- max(R4C$P.W.Seed[2*i-1],R4C$P.W.Seed[2*i])
            R5$P.U.Team[i] <- ifelse(R5$P.F.Team[i]==R4C$P.W.Team[2*i-1],R4C$P.W.Team[2*i],R4C$P.W.Team[2*i-1])
        }
        R5B <- unique(left_join(R5, JTCombine %>% select(c(-2,-3,-4)), by = c("Year"="Year","P.F.Team"="Team")))
        R5C <- unique(left_join(R5B, JTCombine %>% select(c(-2,-3,-4)), by = c("Year"="Year","P.U.Team"="Team"),suffix = c("_F","_U"))) 
        R5C$pmv <- predict(p_model, R5C, type = "raw")
        R5C$A.W.Seed <- ifelse(R5C$amv >=0, R5C$A.F.Seed, R5C$A.U.Seed)
        R5C$A.W.Team <- ifelse(R5C$amv >=0, R5C$A.F.Team, R5C$A.U.Team)
        R5C$P.W.Seed <- ifelse(R5C$pmv >=0, R5C$P.F.Seed, R5C$P.U.Seed)
        R5C$P.W.Team <- ifelse(R5C$pmv >=0, R5C$P.F.Team, R5C$P.U.Team)
        p_show_5 <- R5C %>% mutate (A.Favored = paste(A.F.Seed,A.F.Team,sep = " "),
                                    A.Underdog = paste(A.U.Seed,A.U.Team,sep = " "),
                                    A.Winner = paste(A.W.Seed,A.W.Team,sep = " "),
                                    P.Favored = paste(P.F.Seed,P.F.Team,sep = " "),
                                    P.Underdog = paste(P.U.Seed,P.U.Team,sep = " "),
                                    P.Winner = paste(P.W.Seed,P.W.Team,sep = " "))
        p_show_5 $ESPN_Points <- ifelse(p_show_5$A.Winner == p_show_5$P.Winner,160,0)
        p_show_5 <- p_show_5 %>% select(colnames(p_show))
        p_show <- rbind(p_show,p_show_5)
        
        # Round 6
        
        R6 <- stage3 %>% filter(Year == pyear, Round == 6) %>% select(1:10)
        for (i in 1:1){
            R6$P.F.Seed[i] <- min(R5C$P.W.Seed[2*i-1],R5C$P.W.Seed[2*i])
            R6$P.F.Team[i] <- ifelse(R6$P.F.Seed[i]==R5C$P.W.Seed[2*i-1],R5C$P.W.Team[2*i-1],R5C$P.W.Team[2*i])
            R6$P.U.Seed[i] <- max(R5C$P.W.Seed[2*i-1],R5C$P.W.Seed[2*i])
            R6$P.U.Team[i] <- ifelse(R6$P.F.Team[i]==R5C$P.W.Team[2*i-1],R5C$P.W.Team[2*i],R5C$P.W.Team[2*i-1])
        }
        R6B <- unique(left_join(R6, JTCombine %>% select(c(-2,-3,-4)), by = c("Year"="Year","P.F.Team"="Team")))
        R6C <- unique(left_join(R6B, JTCombine %>% select(c(-2,-3,-4)), by = c("Year"="Year","P.U.Team"="Team"),suffix = c("_F","_U"))) 
        R6C$pmv <- predict(p_model, R6C, type = "raw")
        R6C$A.W.Seed <- ifelse(R6C$amv >=0, R6C$A.F.Seed, R6C$A.U.Seed)
        R6C$A.W.Team <- ifelse(R6C$amv >=0, R6C$A.F.Team, R6C$A.U.Team)
        R6C$P.W.Seed <- ifelse(R6C$pmv >=0, R6C$P.F.Seed, R6C$P.U.Seed)
        R6C$P.W.Team <- ifelse(R6C$pmv >=0, R6C$P.F.Team, R6C$P.U.Team)
        p_show_6 <- R6C %>% mutate (A.Favored = paste(A.F.Seed,A.F.Team,sep = " "),
                                    A.Underdog = paste(A.U.Seed,A.U.Team,sep = " "),
                                    A.Winner = paste(A.W.Seed,A.W.Team,sep = " "),
                                    P.Favored = paste(P.F.Seed,P.F.Team,sep = " "),
                                    P.Underdog = paste(P.U.Seed,P.U.Team,sep = " "),
                                    P.Winner = paste(P.W.Seed,P.W.Team,sep = " "))
        p_show_6 $ESPN_Points <- ifelse(p_show_6$A.Winner == p_show_6$P.Winner,320,0)
        p_show_6 <- p_show_6 %>% select(colnames(p_show))
        p_show <- rbind(p_show,p_show_6)
        
        output$tbl_full_predict <- renderFormattable(p_show %>% formattable())  
        output$txt_ESPN_f <- renderText(sum(p_show$ESPN_Points))
    })
    
    ########################### End of Full Brackets Tab
} ######################## End of the server

########################## Run the application 
shinyApp(ui = ui, server = server)
