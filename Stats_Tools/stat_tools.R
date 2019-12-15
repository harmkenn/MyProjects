library(shiny)
library(shinydashboard)
box <- shinydashboard::box
library(tidyverse)
library(rhandsontable)
library(ggplot2)
library(qqplotr)
library(cowplot)
theme_set(theme_bw())

data.discr <- data.frame(as.numeric(matrix(, nrow=500, ncol=1)))
colnames(data.discr) <- "A"
binwidth <- 1
gx <- 1

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
                column(width = 2,
                       box(
                         title = "Data Input", width = NULL, status = "primary",
                         actionButton("clear","Clear"),actionButton("plot","Plot"),
                         rHandsontableOutput("dt")
                       ) #Ebox
                ), #Ecolumn
                column(width = 5,
                       box(
                         title = "Histogram", width = NULL,
                         plotOutput("hist")
                       ), #Ebox
                       box(
                         title = "Percentile", width = NULL,
                         splitLayout(
                           textInput("ptile","Percentile:",width="25%"),
                           actionButton("goptile","Get"),
                           textOutput("pptile")
                         ) #EsplitLayout
                       ) #Ebox
                ), #Ecolumn
                column(width = 5,
                       box(
                         title = "Summary Statistics", width = NULL, solidHeader = TRUE,
                         tableOutput("dss")
                       ), #Ebox
                       box(
                         title = "qqplot", width = NULL, background = "aqua",
                         plotOutput("qqplot"),
                         valueBoxOutput("qqalert")
                       ) #Ebox
                ) #Ecolumn
              ) #EfluidRow
      ), #EtabItem ds
      tabItem("zTest","zTest goes Here"), #EtabItem zTest
      tabItem("tTestData",
              fluidRow(
                column(width = 2,
                       box(title = "Data Input", width = NULL, status = "primary",
                           actionButton("cleart","Clear"),actionButton("plott","Plot"),
                           rHandsontableOutput("dtt")
                       ) #Ebox
                ), #Ecolumn
                column(width = 5,
                       box(title = "Summary Statistics", width = NULL, solidHeader = TRUE,
                           tableOutput("dsst")
                       ), #Ebox
                       box(title = "qqplot", width = NULL, background = "aqua",
                           plotOutput("qqplott"),
                           valueBoxOutput("qqalertt")
                       ) #Ebox
                ), #Ecolumn
                column(width = 5,
                       box(title = "Hypothesis Test", width = NULL,
                           splitLayout(
                             textInput("th0","Null:",0,width="50%"),
                             textInput("tAlpha","Alpha:",.05,width="50%"),
                             radioButtons("ttail","",c("Left Tail"="less","Two Tail"="two.sided","Right Tail"="greater"),inline = FALSE,width = "50%"),
                             actionButton("ttest","Test")
                           ), #EsplitLayout
                           tableOutput("ttr"),
                           plotOutput("ttgraph")
                       ), #Ebox
                       box(title = "Confidence Interval", width = NULL,
                       ) #Ebox
                ) #Ecolumn
              ) #EfluidRow
      ), #EtabItem tTestData
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
  hist.x <- reactiveValues(values = data.discr)
  t.x <- reactiveValues(values = data.discr)
  output$dt <- renderRHandsontable({rhandsontable(data.in$values)})
  output$dtt <- renderRHandsontable({rhandsontable(data.in$values)})
  observeEvent(eventExpr = input$plot, {
    data.in$values <- hot_to_r(input$dt)
    if(sum(!is.na(data.in$values[,1]))>1){
      hist.x <- data.in$values[!is.na(data.in$values)]
      count <- length(hist.x)
      bins <- ceiling(1+3.322*log10(count))
      if (count > 2) {binwidth <- (max(hist.x)-min(hist.x)+2)/(bins-2)}
      hist.df <- data.frame(hist.x)
      if(!is.null(tryCatch(ggplot(hist.df), error = function(e){}))){
        dd1 <- hist.df %>%
          ggplot() + 
          geom_histogram(aes(x=hist.df[,1]),color="darkblue", fill="lightblue",binwidth=binwidth)+
          labs(x="A") + xlim(c(min(hist.x)-binwidth,max(hist.x)+binwidth))
      } #Eif
      if(!is.null(tryCatch(ggplot(hist.df), error = function(e){}))){
        dd2 <- hist.df %>%
          ggplot(aes(x="", y = hist.df[,1])) +
          geom_boxplot(color="darkblue", fill="lightblue", outlier.colour="red", outlier.shape=8, outlier.size=4) + 
          geom_jitter(width = .1) +
          coord_flip() +
          theme_classic() +
          labs(x="", y="A" ) + ylim(c(min(hist.x)-binwidth,max(hist.x)+binwidth))
      } #Eif
      output$hist <- renderPlot({
        plot_grid(dd1, dd2, ncol = 1, rel_heights = c(2, 1),align = 'v', axis = 'lr') 
      }) #Eoutput$hist
      
      output$qqplot <- renderPlot({
        hist.df %>% ggplot(mapping = aes(sample = hist.x)) +
          stat_qq_band() +
          stat_qq_line() +
          stat_qq_point() +
          labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
      }) #Eoutput$qqplot
      good <- shapiro.test(hist.x)
      output$qqalert <- renderValueBox({
        valueBox(round(good$p.value,3), subtitle = "p-value",width = 5,color = if (good$p.value < .05) {"red"} else {"green"})
      }) #Eoutput$qqalert
      sx <- summary(hist.x)
      sxe <- quantile(hist.x, c(0.25, 0.75), type = 6)
      dsse <- matrix(formatC(c("","","","","",sxe[2],"",sxe[1],""),
                             format="f",digits = 7,drop0trailing = TRUE),ncol=1,nrow=9)
      dss <- matrix(formatC(c(length(hist.x),sx[4],sd(hist.x),var(hist.x),
                              sx[6],sx[5],sx[3],sx[2],sx[1]),
                            format="f",digits = 7,drop0trailing = TRUE),ncol=1,nrow=9)
      dss <- cbind(dss,dsse)
      rownames(dss) <- c("Count","Mean","Standard Dev","Variance","Max","3rd Quartile","Median","1st Quartile","Min")
      output$dss <- renderTable({dss},rownames = TRUE,colnames=FALSE)
    }#Eif
  }) #EobserveEvent
  observeEvent(eventExpr = input$clear, {
    data.discr <- data.frame(matrix(NA_real_, nrow = 500, ncol = 1))
    colnames(data.discr) <- "A"
    data.in <- reactiveValues(values = data.discr)
    output$dt <- renderRHandsontable({rhandsontable(data.in$values)})
  }) #EobserveEvent
  observeEvent(eventExpr = input$goptile, {
    data.in$values <- hot_to_r(input$dt)
    if(sum(!is.na(data.in$values[,1]))>1){
      hist.x <- data.in$values[!is.na(data.in$values)]
    }
    ptileout <- quantile(hist.x, as.numeric(input$ptile) / 100, type = 6)
    output$pptile <- renderText({paste("The ",input$ptile," percentile is: ",round(ptileout,2))})
  }) #EobserveEvent
  observeEvent(eventExpr = input$plott, {
    t.x$values <- hot_to_r(input$dtt)
    if(sum(!is.na(t.x$values[,1]))>1){
      t.x <- t.x$values[!is.na(t.x$values)]
      t.df <- data.frame(t.x)
      output$qqplott <- renderPlot({
        t.df %>% ggplot(mapping = aes(sample = t.x)) +
          stat_qq_band() +
          stat_qq_line() +
          stat_qq_point() +
          labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
      }) #Eoutput$qqplot
      good <- shapiro.test(t.x)
      output$qqalertt <- renderValueBox({
        valueBox(round(good$p.value,3), subtitle = "p-value",width = 5,color = if (good$p.value < .05) {"red"} else {"green"})
      }) #Eoutput$qqalert
      sx <- summary(t.x)
      dss <- matrix(formatC(c(length(t.x),sx[4],sd(t.x)),
                            format="f",digits = 7,drop0trailing = TRUE),ncol=3,nrow=1)
      colnames(dss) <- c("Count","Mean","Standard Dev")
      output$dsst <- renderTable({dss},rownames = FALSE,colnames=TRUE)
      
    }#Eif
  }) #EobserveEvent
  observeEvent(eventExpr = input$ttest, {
    t.x$values <- hot_to_r(input$dtt)
    if(sum(!is.na(t.x$values[,1]))>1){
      t.x <- t.x$values[!is.na(t.x$values)]
    } #Eif
    alpha <- as.numeric(input$tAlpha)
    mu <- as.numeric(input$th0)
    tail <- input$ttail
    if(input$ttail == "two.sided"){cl <- 1 - alpha}else{cl <- 1 - 2*alpha}
    ttr <- t.test(t.x,alternative = tail,mu=mu, conf.level = cl)
    ttt <- matrix(formatC(c(length(t.x)-1,ttr$statistic,ttr$p.value),
                          format="f",digits = 7,drop0trailing = TRUE),ncol=3,nrow=1)
    colnames(ttt) <- c("df","t-score","P-Value")
    output$ttr <- renderTable({ttt},rownames = FALSE,colnames=TRUE)
  })#EobserveEvent
} #end of the server

# Run the application 
shinyApp(ui = ui, server = server)