#pacman::p_load(shiny,shinydashboard,tidyverse,plotly,DT,formattable,magrittr,gt,caret, e1071,glmnet)
require(shiny)
require(shinydashboard)
require(tidyverse)
require(plotly)
require(DT)
require(formattable)
require(magrittr)
require(gt)
require(caret)
require(e1071)
require(glmnet)

load("NBA_Worth.rda")
load("Players_2022.rda")


########################### Start of UI

ui <- dashboardPage(
    dashboardHeader(title = "NBA MoneyBall (v1.0)",titleWidth = "450px",
                    tags$li(class = "dropdown",tags$a("by Ken Harmon")),
                    dropdownMenuOutput(outputId = "notifications")),
    
    ########################### Start Side Bar  
    
    dashboardSidebar(width = 150,
                     sidebarMenu(
                         menuItem("NBA Players", tabName = "nba_players"),
                         menuItem("Worth", tabName = "worth"),
                         menuItem("Free Agents", tabName = "free_agents")
                     ) #################### End sidebarMenu
    ), ###################### End dashboardSidebar
    
    ########################### End Side Bar
    ########################### Start Dashboard Body
    dashboardBody(
        ########################### Starts Tab Items    
        tabItems(
            ########################### Starts NBA Players Tab
            tabItem("nba_players",
                    fluidRow(
                        column(width = 12,
                               box(title = "NBA Players since 1990", width = NULL, status = "primary",
                                   DTOutput("tbl_nba_players")       
                               ) ############# End box
                        ) ############## End column      
                    ) ################# End of fluidrow
            ), ################### End NBA Players Tab
            ########################### Starts Worth Tab
            tabItem("worth",
                    fluidRow(
                        column(width = 12,
                               box(title = "NBA Players worth since 1990", width = NULL, status = "primary",
                                   DTOutput("tbl_worth")       
                               ) ############# End box
                        ) ############## End column      
                    ) ################# End of fluidrow
            ), ################### End Worth Tab
            ########################### Start Free Agents Tab
            tabItem("free_agents",
                    fluidRow(
                        column(width = 12,
                               box(title = "Free Agents", width = NULL, status = "primary",
                                   box(title = "NBA Players for 2021-22 ", width = NULL, status = "primary",
                                    DTOutput("tbl_fa")      
                               ) ############# End box
                        ) ############## End column      
                    ) ################# End of fluidrow
            ) ################### End Free Agents Tab
        ) ##################### End tabItems
    ) ####################### End dashboard Body
) ######################### End dashboard Page

)########################### End UI

########################### Start of the Server
server <- function(input, output, session) {
    # Start of NBA Players
    output$tbl_nba_players <- renderDT(NBA_Worth,filter = "top",options = list(scrollX = TRUE,scrollY = TRUE),rownames = FALSE)
    # End of NBA Players
    # Start of Worth
    launch <- NBA_Worth %>% select(1:8,31:33)
    output$tbl_worth <- renderDT(launch,filter = "top",options = list(scrollX = TRUE,scrollY = TRUE),rownames = FALSE)
    # End of Worth
    # Start of Free Agents
    launch <- Match %>% select(1:8,29:33)
    output$tbl_fa <- renderDT(launch %>% datatable(extensions = 'Buttons',filter = "top",
                                            options = list(pageLength = 50,scrollX=TRUE,dom = 'T<"clear">lBfrtip')) %>%
                                  formatCurrency(9:13, currency = "", interval = 3, mark = ",")
    ) 
    # End of Free Agents
} ######################## End of the server

########################## Run the application 
shinyApp(ui = ui, server = server)
