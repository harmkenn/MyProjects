library(tidyverse)
library(shiny)
library(rlist)

ch_mort <- read_csv("gm_child_mortality.csv")
L_ch_mort <- pivot_longer(ch_mort, 2:220, names_to = "Year", values_to = "Deaths")
region <- read_csv("gm_region.csv")
gap <- left_join(L_ch_mort,region)
fert <- read_csv("gm_fertility.csv")
L_fert <- pivot_longer(fert, 2:220, names_to = "Year", values_to = "Births")
gap <- left_join(gap,L_fert)
choices <- list.remove(colnames(gap),c(1,2,4)) 

# >>>>>>>>>>>>>>>Start of UI

ui <- dashboardPage(
  dashboardHeader(title = "Gapminder v1.0",titleWidth = "450px",
                  tags$li(class = "dropdown",tags$a("by Ken Harmon")),
                  dropdownMenuOutput(outputId = "notifications")),
  
  # >>>>>>>>>>>>>>>Side Bar  
  
  dashboardSidebar(width = 150,
    sidebarMenu(
      selectInput("x", "x-axis", choices = choices), 
      selectInput("y", "y-axis", choices = choices)
    ) #End sidebarMenu
  ), #End dashboardSidebar
  
  # <<<<<<<<<<<<<End Sidebar
  # >>>>>>>>>>>>>Dashboard Body
  
  dashboardBody(
    plotOutput("plot"),
    sliderInput("Year","Year",min = 1850,max = 2018,value = 2000, step = 2,
                animate=animationOptions(interval = 1000, loop = TRUE))
  ) #EdashboardBody
) #EdashboardPage

##############################    Start of the Server
server <- function(input, output, session) {
  observeEvent(input$Year,{
    gapplot <- gap %>% filter(Year == input$Year) %>% 
      ggplot(aes(x=input$x, y=input$y, color = region)) + 
      geom_point() + xlim(c(0,650)) + ylim(c(0,9)) 
    output$plot <- renderPlot({gapplot})
  }) #EobserveEvent
} #end of the server

# Run the application 
shinyApp(ui = ui, server = server)