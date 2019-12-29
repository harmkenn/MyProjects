library(shiny)
library(shinyjs)
library(shinydashboard)
box <- shinydashboard::box
library(tidyverse)
library(rhandsontable) 
library(ggplot2)
library(qqplotr) 
library(cowplot)
library(grid)
library(gridExtra)
theme_set(theme_bw())

data.discr <- data.frame(matrix(numeric(), nrow=500, ncol=1))
colnames(data.discr) <- "A"
data.ttest <- data.frame(matrix(numeric(), nrow=500, ncol=1))
colnames(data.ttest) <- "A"
data.anova <- data.frame(matrix(numeric(), nrow=300, ncol=6))
colnames(data.anova) <- c("A","B","C","D","E","F")
data.anova.s <- data.frame(matrix(numeric(), nrow=3, ncol=6))
colnames(data.anova.s) <- c("A","B","C","D","E","F")
rownames(data.anova.s) <- c("Count","Mean","SD")
binwidth <- 1

# >>>>>>>>>>>>>>>Start of UI

ui <- dashboardPage(
  dashboardHeader(title = span("Shiny Stat Tools"),titleWidth = "450px",
                  tags$li(class = "dropdown",tags$a("by Ken Harmon")),
                  dropdownMenuOutput(outputId = "notifications")),
  
  # >>>>>>>>>>>>>>>Side Bar  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Descriptive Stats", tabName = "ds"),
      menuItem("Normal", tabName = "normal"),
      menuItem("All t-Tests", tabName = "tTest"),
      menuItem("ANOVA", tabName = "anova"),
      menuItem("Proportions", tabName = "props"),
      menuItem("Chi-Square", tabName = "Chi"),
      menuItem("Linear Regression", tabName = "LR"),
      menuItem("Discrete", tabName = "Disc"),
      menuItem("Data Sets", tabName = "datasets")
    ) #End sidebarMenu
  ), #End dashboardSidebar
  
  # <<<<<<<<<<<<<End Sidebar
  # >>>>>>>>>>>>>Dashboard Body
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    tabItems(
      
      # >>>>>>>>>>>>>>Discrete Tab UI
      
      tabItem("ds",
        fluidRow(
          column(width = 2,
           box(title = "Data Input", width = NULL, status = "primary",
               actionButton("clear","Clear"),actionButton("plot","Plot"),
               rHandsontableOutput("dt")
           ) #Ebox
          ), #Ecolumn
          column(width = 5,
           box(title = "Histogram", width = NULL,
               plotOutput("hist")
           ), #Ebox
           box(title = "Percentile", width = NULL,
               splitLayout(
                 numericInput("ptile","Percentile:",50,width="50%"),
                 actionButton("goptile","Get"),
                 textOutput("pptile")
               ) #EsplitLayout
           ) #Ebox
          ), #Ecolumn
          column(width = 5,
           box(title = "Summary Statistics", width = NULL, solidHeader = TRUE,
               tableOutput("dss")
           ), #Ebox
           box(title = "qqplot", width = NULL, background = "blue",
               plotOutput("qqplot"),
               valueBoxOutput("qqalert")
           ) #Ebox
          ) #Ecolumn
        ) #EfluidRow
      ), #EtabItem ds
      
      # <<<<<<<<<<<<< Discrete Tab UI
      # >>>>>>>>>>>>> Normal Tab UI
      
      tabItem("normal",
        fluidRow(
          column(width = 3,
                 box(title = "Selections", width = NULL, solidHeader = TRUE,
                     radioButtons("nway","",c("z to Prob","Prob to z")),
                     numericInput("nmu","Mean:",0),
                     numericInput("nsd", "Standard Dev:",1),
                     actionButton("nReset","Reset")
                 ), #Ebox
                 conditionalPanel(condition = "input.nway == 'z to Prob'",
                    helpText("Shade:"),
                    checkboxInput("Left","Left"),
                    checkboxInput("Center","Center"),
                    checkboxInput("Right","Right"),
                    helpText("z-Score Cut-offs"),
                    checkboxInput("nsym","Symmetric"),
                 ), #EconditionalPanel
                 conditionalPanel(condition = "input.nway == 'Prob to z'",
                                  radioButtons("nshade","Shade", choices = c("Left","Center","Right"))
                 ), #EconditionalPanel
          ), #Ecolumn left
          column(width = 9,
             conditionalPanel(condition = "input.nway == 'z to Prob'",
                splitLayout(
                  numericInput("lz","Left z-Score",-1,width="25%",step = .1),
                  actionButton("z2p","Find Probability"),
                  numericInput("rz","Right z-Score",1,width="25%", step = .1)
                ), #EsplitLayout
                plotOutput("npp"),
                textOutput("npptext")
             ), #EconditionalPanel
             conditionalPanel(condition = "input.nway == 'Prob to z'",
                splitLayout(
                  numericInput("prob","Percent",40,width="25%"),
                  actionButton("p2z","Find Probability")
                ), #EsplitLayout
                plotOutput("npz"),
                textOutput("npztext")
             ) #EconditionalPanel
          ) #Ecolumn Main
        ) #EfluidRow Normal Tab
      ), #EtabItem Normal
      
      # <<<<<<<<<<<<<<<<< Normal TAB UI
      # >>>>>>>>>>>>>>>>> tTest TAB UI
      
      tabItem("tTest",
        fluidRow(
          column(width = 3,
         box(title = "Data Input",width = NULL,status = "primary",
             checkboxInput("Statistics","Statistics"),
             conditionalPanel(condition = "input.Statistics == 0",
                  radioButtons("tchoice","Input:",c("Single Data","Paired Data","2 Sample t-Test")),
                  actionButton("cleart", "Clear"),
                  actionButton("plott", "Plot"),
                  rHandsontableOutput("dtt")
             ), #End of conditionalPanel
             conditionalPanel(condition = "input.Statistics == 1",
                radioButtons("tchoice","Input:",c("Single Data","2 Sample t-Test")),
             ), #End of conditionalPanel
             conditionalPanel(condition = "input.Statistics == 1 && input.tchoice == 'Single Data'",
                numericInput("tmean","Mean:",0),
                numericInput("tsd","Standard Deviation:",1),
                numericInput("tn","Count:",1)
             ), #End of conditionalPanel
             conditionalPanel(condition = "input.Statistics == 1 && input.tchoice == '2 Sample t-Test'",
                  numericInput("tmean","Mean A:",0),
                  numericInput("tsd","Standard Deviation A:",1),
                  numericInput("tn","Count A:",1),
                  numericInput("tmean.b","Mean B:",0),
                  numericInput("tsd.b","Standard Deviation B:",1),
                  numericInput("tn.b","Count B:",1)
               ), #End of conditionalPanel
           ), #Ebox
          ), #Ecolumn
          conditionalPanel(condition = "input.Statistics == 0",
           column(width = 4,
              box(title = "Summary Statistics", width = NULL, background = "blue",
                  tableOutput("ttst"),
                  plotOutput("ttqq"),
                  splitLayout(
                    valueBoxOutput("qqalertt", width = NULL),
                    valueBoxOutput("qqalertt.b", width = NULL)
                  ), #EsplitLayout
              ), #Ebox
           ), #Ecolumn
          ), #End of Conditional
          column(width = 5,
           box(title = "Hypothesis Test", width = NULL,
               splitLayout(
                 numericInput("th0","Null:",0,width=NULL),
                 numericInput("tAlpha","Alpha:",.05,width=NULL),
                 radioButtons("ttail","",c("Left Tail"="less","Two Tail"="two.sided","Right Tail"="greater"),inline = FALSE,width = "50%"),
                 actionButton("ttest","Test")
               ), #EsplitLayout
               conditionalPanel(condition = "input.tchoice == '2 Sample t-Test'",checkboxInput("eqvar","Equal Variances",TRUE)),
               plotOutput("ttgraph")
           ), #Ebox
          ) #Ecolumn
        ) #EfluidRow
      ), #EtabItem tTest
      
      ######################## End t-test TAB UI #######################
      ######################## ANOVA Tab UI #############################
      
      tabItem("anova",
        fluidRow(
          column(width = 4, 
             box(title = "Data Input", width = 12, 
               checkboxInput("anovastat","Statistics"),
               actionButton("anovatc", "Clear"),
               actionButton("anovaplot", "Plot"),
               rHandsontableOutput("anovat")
             ), #Ebox
          ), #Ecolumn left
          conditionalPanel(condition = "input.anovastat == 0",
           column(width = 3, 
            box(title = "graphs",width = 12, background = "blue", 
              plotOutput("anovabox"),
              plotOutput("anovaqq")
            ), #Ebox
           ), #Ecolumn middle
          ), #EconditionalPanel
          column(width = 5, 
           box(title = "Results", width = 12, 
             tableOutput("anovaut"),
             tableOutput("anovalt"),
             numericInput("F.alpha","Alpha:",.05),
             plotOutput("F.plot")
           ), #Ebox
          ), #Ecolumn right
        ) #EfluidRow
      ), #EtabItem ANOVA
      
      ######################## End ANOVA Tab UI #########################
      ######################## Proportions UI ##############################
      
      tabItem("props",
        fluidRow(
          column(width = 4, 
             box(title = "Data Input", width = 12, 
               radioButtons("props","",choices = c("1 Proportion", "2 Proportions")),
                 splitLayout(
                   numericInput("x1","Set 1 x:",0),
                   numericInput("n1","Set 1 n:",1)
                  ), #Esplit
               conditionalPanel(condition = "input.props == '2 Proportions'",
                  splitLayout(
                   numericInput("x2","Set 2 x:",0),
                   numericInput("n2","Set 2 n:",1)
                  ), #Esplit
               ), #EconditionalPanel
             ), #Ebox
          ), #Ecolumn left
          column(width = 3,
            box(title = "Graphs",width = 12, background = "blue", 
              plotOutput("pplot")
            ) #Ebox
          ), #Ecolumn Middle  
          column(width = 5,
            box(title = "Results",width = 12,  
              splitLayout(
                 numericInput("ph0","Null:",.5,width=NULL),
                 numericInput("pAlpha","Alpha:",.05,width=NULL),
                 radioButtons("ptail","",c("Left Tail"="less","Two Tail"="two.sided","Right Tail"="greater"),inline = FALSE,width = "50%"),
                 actionButton("ptest","Test")
               ), #EsplitLayout
            ) #Ebox
          ), #Ecolumn Right 
        ) #EfluidRow
      ), #EtabItem props
      
      ######################## End Proportions UI #############################
      
      tabItem("Chi","Chi goes Here"), #EtabItem Chi
      tabItem("LR","LR goes Here"), #EtabItem LR
      tabItem("Disc","Disc goes Here"), #EtabItem Disc
      tabItem("datasets", "All the pre-built datasets will go here") #EtabItem Datasets
    ) #End tabItems
  ) #EdashboardBody
) #EdashboardPage

# >>>>>>>>>>> Start of the Server
server <- function(input, output, session) {
  
  # >>>>>>>>>> Variables
  
  disc.in <- reactiveValues(values = data.discr)
  hist.x <- reactiveValues(values = data.discr)
  t.in <- reactiveValues(values = data.ttest)
  anova.in <- reactiveValues(values = data.anova)
  anova.s.in <- reactiveValues(values = data.anova.s)
  
  #>>>>>>>>>> Discrete Tab Server  
  
  output$dt <- renderRHandsontable({rhandsontable(disc.in$values)})
  
  observeEvent(eventExpr = input$plot, {
    disc.in$values <- hot_to_r(input$dt)
    if(sum(!is.na(disc.in$values[,1]))>1){
      hist.x <- disc.in$values[!is.na(disc.in$values)]
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
                             format="f",digits = 6,drop0trailing = TRUE),ncol=1,nrow=9)
      dss <- matrix(formatC(c(length(hist.x),sx[4],sd(hist.x),var(hist.x),
                              sx[6],sx[5],sx[3],sx[2],sx[1]),
                            format="f",digits = 6,drop0trailing = TRUE),ncol=1,nrow=9)
      dss <- cbind(dss,dsse)
      rownames(dss) <- c("Count","Mean","Standard Dev","Variance","Max","3rd Quartile","Median","1st Quartile","Min")
      output$dss <- renderTable({dss},rownames = TRUE,colnames=FALSE)
    }#Eif
  }) #EobserveEvent
  observeEvent(eventExpr = input$clear, {
    data.discr <- data.frame(matrix(NA_real_, nrow = 500, ncol = 1))
    colnames(data.discr) <- "A"
    disc.in <- reactiveValues(values = data.discr)
    output$dt <- renderRHandsontable({rhandsontable(disc.in$values)})
  }) #EobserveEvent
  observeEvent(eventExpr = input$goptile, {
    disc.in$values <- hot_to_r(input$dt)
    if(sum(!is.na(disc.in$values[,1]))>1){
      hist.x <- disc.in$values[!is.na(disc.in$values)]
    }
    ptileout <- quantile(hist.x, (input$ptile) / 100, type = 6)
    output$pptile <- renderText({paste("The ",input$ptile," percentile is: ",round(ptileout,2))})
  }) #EobserveEvent
  
  # <<<<<<<<<<<<<< End of Discrete Tab Server
  # >>>>>>>>>>>>>> Start of Normal Tab Server
  
  x <- seq(from = -4, to = 4, by = .01)
  observeEvent(input$rz, {
    if(input$nsym == TRUE){
      updateNumericInput(session, "lz", value = -1*input$rz)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$nsym, {
    if(input$nsym == TRUE){
      updateNumericInput(session, "lz", value = -1*input$rz)
      disable("lz")
    }
  }, ignoreInit = TRUE)
  observeEvent(c(input$z2p,input$nsym), {
    mu <- input$nmu
    sd <- input$nsd
    lz <- input$lz
    rz <- input$rz
    s.df <- data.frame(x,y=dnorm(x))
    normp <- s.df %>% ggplot(aes(x,y))+geom_line()+
      geom_area(aes(y=y),alpha=0) + scale_x_continuous(sec.axis = sec_axis(~.*sd+mu, name = "A")) +
      theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
      labs(x = "Z") + geom_segment(aes(x = lz, y = 0, xend = lz, yend = dnorm(lz)),color="red") + 
      geom_segment(aes(x = rz, y = 0, xend = rz, yend = dnorm(rz)),color="red")
    output$npp <- renderPlot({
      if(input$Left == TRUE){normp <- normp + geom_area(data=subset(s.df,x<lz),aes(y=y), fill ="blue", alpha = .5)}else{normp}
      if(input$Center == TRUE){normp <- normp + geom_area(data=subset(s.df,x > lz & x < rz),aes(y=y), fill ="blue", alpha = .5)}else{normp}
      if(input$Right == TRUE){normp + geom_area(data=subset(s.df,x > rz),aes(y=y), fill ="blue", alpha = .5)}else{normp}
    })
    output$npptext <- renderText({
      if(input$Left == TRUE){tp <- pnorm(lz)}else{tp <- 0}
      if(input$Center == TRUE){tp <- tp + pnorm(rz) - pnorm(lz)}else{tp <- tp}
      if(input$Right == TRUE){tp <- tp + 1 - pnorm(rz)}else{tp <- tp}
      paste("Total Prabability: ",tp)
    })
  }) #EobserveEvent
  observeEvent(c(input$p2z,input$nshade), {
    mu <- input$nmu
    sd <- input$nsd
    prob <- input$prob
    s.df <- data.frame(x,y=dnorm(x))
    npz <- s.df %>% ggplot(aes(x,y))+geom_line()+
      geom_area(aes(y=y),alpha=0) + scale_x_continuous(sec.axis = sec_axis(~.*sd+mu, name = "A")) +
      theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
      labs(x = "Z") 
    if(input$nshade == "Left"){
      z <- qnorm(prob/100)
      npz <- npz + geom_area(data=subset(s.df,x <= z),aes(y=y), fill ="blue", alpha = .5) + 
        geom_segment(aes(x = z, y = 0, xend = z, yend = dnorm(z)),color="red")
      npztext <- paste("z-Score: ",z)
    }else if (input$nshade == "Center"){
      z <- qnorm((1-prob/100)/2)
      npz <- npz + geom_area(data=subset(s.df,x >= z & x <= -z),aes(y=y), fill ="blue", alpha = .5) + 
        geom_segment(aes(x = z, y = 0, xend = z, yend = dnorm(z)),color="red") +
        geom_segment(aes(x = -z, y = 0, xend = -z, yend = dnorm(-z)),color="red")
      npztext <- paste("z-Scores: ",z," & ",-z)
    }else if(input$nshade == "Right"){
      z <- qnorm(1 - prob/100)
      npz <- npz + geom_area(data=subset(s.df,x >= z),aes(y=y), fill ="blue", alpha = .5) + 
        geom_segment(aes(x = z, y = 0, xend = z, yend = dnorm(z)),color="red")
      npztext <- paste("z-Score: ",z) 
    }
    output$npz <- renderPlot({npz})
    output$npztext <- renderText({npztext})
  }) #EobserveEvent  
  
  # <<<<<<<<<<<<<< End of Normal Tab Server
  # >>>>>>>>>>>>>> Start of t-test Tab Server
  
  output$dtt <- renderRHandsontable({rhandsontable(t.in$values)})
  
  observeEvent(c(input$cleart,input$tchoice), {
    if(input$tchoice == "Single Data"){
      data.ttest <- data.frame(matrix(NA_real_, nrow = 500, ncol = 1))
      colnames(data.ttest) <- "A"
      t.in <- reactiveValues(values = data.ttest)
      output$dtt <- renderRHandsontable({rhandsontable(t.in$values)})
    }else if(input$tchoice == "Paired Data"){
      data.ttest <- data.frame(matrix(NA_real_, nrow = 500, ncol = 3))
      colnames(data.ttest) <- c("diff","A","B")
      t.in <- reactiveValues(values = data.ttest)
      output$dtt <- renderRHandsontable({rhandsontable(t.in$values) %>% 
          hot_col("diff", readOnly = TRUE)})
    }else if(input$tchoice == "2 Sample t-Test"){
      data.ttest <- data.frame(matrix(NA_real_, nrow = 500, ncol = 2))
      colnames(data.ttest) <- c("A","B")
      t.in <- reactiveValues(values = data.ttest)
      output$dtt <- renderRHandsontable({rhandsontable(t.in$values)}) 
    }
  }) #EobserveEvent
  
  observeEvent(input$plott, {
    t.in$values <- hot_to_r(input$dtt)
    if(input$tchoice == "Paired Data"){
      t.in$values[,1] <- t.in$values[,2]-t.in$values[,3]
      output$dtt <- renderRHandsontable({rhandsontable(t.in$values) %>% 
          hot_col("diff", readOnly = TRUE)})
    }
    if(sum(!is.na(t.in$values[,1]))>1){
      t.x <- t.in$values[,1]
      t.x <- t.x[!is.na(t.x)]
      t.df <- data.frame(t.x)
      qqt <- t.df %>% ggplot(mapping = aes(sample = t.x)) +
        stat_qq_band() +
        stat_qq_line() +
        stat_qq_point() +
        labs(title="A",x = "Theoretical Quantiles", y = "Sample Quantiles")
      good <- shapiro.test(t.x)
      output$qqalertt <- renderValueBox({
        valueBox(round(good$p.value,3), subtitle = "p-value",width = 5,color = if (good$p.value < .05) {"red"} else {"green"})
      }) #Eoutput$qqalert      
      sx <- summary(t.x)
      dss <- matrix(formatC(c(length(t.x),sx[4],sd(t.x),sd(t.x)/sqrt(length(t.x))),
                            format="f",digits = 6,drop0trailing = TRUE),ncol=4,nrow=1)
      colnames(dss) <- c("Count","Mean","Standard Dev","Standard Error")
      rownames(dss) <- c("A")
      tse <- sd(t.x)/sqrt(length(t.x))
    }#Eif
    if(input$tchoice == "2 Sample t-Test"){
      t.x.b <- t.in$values[,2]
      t.x.b <- t.x.b[!is.na(t.x.b)]
      t.df.b <- data.frame(t.x.b)
      qqt.b <- t.df.b %>% ggplot(mapping = aes(sample = t.x.b)) +
        stat_qq_band() +
        stat_qq_line() +
        stat_qq_point() +
        labs(title="B", x = "Theoretical Quantiles", y = "Sample Quantiles")
      good.b <- shapiro.test(t.x.b)
      output$qqalertt.b <- renderValueBox({
        valueBox(round(good.b$p.value,3), subtitle = "p-value",width = 5,color = if (good.b$p.value < .05) {"red"} else {"green"})
      }) #Eoutput$qqalert.b      
      sx.b <- summary(t.x.b)
      dss.b <- matrix(formatC(c(length(t.x.b),sx.b[4],sd(t.x.b),sd(t.x.b)/sqrt(length(t.x.b))),
                              format="f",digits = 6,drop0trailing = TRUE),ncol=4,nrow=1)
      dmean <- (sx[4]-sx.b[4])
      tse <- sqrt(sd(t.x.b)^2/length(t.x.b)+sd(t.x)^2/length(t.x))
      dss.t <- matrix(formatC(c(NA,dmean,NA,tse),
                              format="f",digits = 6,drop0trailing = TRUE),ncol=4,nrow=1)
      dss <- rbind(dss,dss.b,dss.t)
      rownames(dss) <- c("A","B", "Total")
    }#Eif
    output$ttst <- renderTable({dss},rownames = TRUE,colnames=TRUE)
    if(input$tchoice == "Single Data"){output$ttqq <- renderPlot({qqt})
    }else if (input$tchoice == "2 Sample t-Test"){
      boxpA <- t.df %>% ggplot(aes(x="", y = t.x)) +
        geom_boxplot(color="darkblue", fill="lightblue", outlier.colour="red", outlier.shape=8, outlier.size=4) + 
        geom_jitter(width = .1) + theme_classic() +
        labs(x="", y="A" ) + ylim(c(min(t.x,t.x.b),max(t.x,t.x.b)))
      boxpB <- t.df.b %>% ggplot(aes(x="", y = t.x.b)) +
        geom_boxplot(color="darkblue", fill="lightblue", outlier.colour="red", outlier.shape=8, outlier.size=4) + 
        geom_jitter(width = .1) + theme_classic() +
        labs(x="", y="B" ) + ylim(c(min(t.x,t.x.b),max(t.x,t.x.b)))
      output$ttqq <- renderPlot({grid.arrange(boxpA,boxpB,qqt, qqt.b, ncol =2, nrow =2)})
    } #Eif
  }) #EobserveEvent
  
  ##################### Run the t-Test ########################  
  
  observeEvent(input$ttest, {
    alpha <- input$tAlpha
    mu <- input$th0
    tail <- input$ttail
    
    ################## t-Test Data ##########################    
    
    if(input$Statistics == FALSE){
      t.in$values <- hot_to_r(input$dtt)
      if(sum(!is.na(t.in$values[,1]))>1){
        t.x <- t.in$values[,1]
        t.x <- t.x[!is.na(t.x)]
        tse <- sd(t.x)/sqrt(length(t.x))
      } #Eif
      tmean <- mean(t.x)
      if (tail == "less"){
        cl <- 1 - 2*alpha
        if(input$tchoice == "2 Sample t-Test"){
          t.x.b <- t.in$values[,2]
          t.x.b <- t.x.b[!is.na(t.x.b)]
          tmean <- tmean - mean(t.x.b)
          ttr <- t.test(t.x,t.x.b,alternative = tail,mu=mu, var.equal = input$eqvar, conf.level = cl)
          tse <- sqrt(sd(t.x.b)^2/length(t.x.b)+sd(t.x)^2/length(t.x))
        }else{
          ttr <- t.test(t.x,alternative = tail,mu=mu, conf.level = cl)
          
        }
        df <- ttr$parameter
        tcv <- qt(alpha,df)
        t.s <- ttr$statistic
        t.p <- pt(t.s,df)
        U <- max(abs(tcv),abs(t.s))+1
        L <- -1*U
        x <- seq(from = L, to = U, by = .01)
        s.df <- data.frame(x,y=dt(x,df))
        tp <- s.df %>% ggplot(aes(x,y))+geom_line()+
          geom_area(data=subset(s.df,x<=tcv),aes(y=y), fill ="red", alpha = .5) +
          geom_area(data=subset(s.df,x<=t.s),aes(y=y), fill ="blue", alpha = .5)
      }else if(tail == "greater"){
        cl <- 1 - 2*alpha
        if(input$tchoice == "2 Sample t-Test"){
          t.x.b <- t.in$values[,2]
          t.x.b <- t.x.b[!is.na(t.x.b)]
          ttr <- t.test(t.x,t.x.b,alternative = tail,mu=mu, var.equal = input$eqvar, conf.level = cl)
          tse <- sqrt(sd(t.x.b)^2/length(t.x.b)+sd(t.x)^2/length(t.x))
        }else{
          ttr <- t.test(t.x,alternative = tail,mu=mu, conf.level = cl)
        }
        df <- ttr$parameter
        tcv <- qt(1-alpha,df)
        t.s <- ttr$statistic
        t.p <- 1 - pt(t.s,df)
        U <- max(abs(tcv),abs(t.s))+1
        L <- -1*U
        x <- seq(from = L, to = U, by = .01)
        s.df <- data.frame(x,y=dt(x,df))
        tp <- s.df %>% ggplot(aes(x,y))+geom_line()+
          geom_area(data=subset(s.df,x>=tcv),aes(y=y), fill ="red", alpha = .5) +
          geom_area(data=subset(s.df,x>=t.s),aes(y=y), fill ="blue", alpha = .5)
      }else{
        cl <- 1 - alpha
        if(input$tchoice == "2 Sample t-Test"){
          t.x.b <- t.in$values[,2]
          t.x.b <- t.x.b[!is.na(t.x.b)]
          ttr <- t.test(t.x,t.x.b,alternative = tail,mu=mu, var.equal = input$eqvar, conf.level = cl)
          tse <- sqrt(sd(t.x.b)^2/length(t.x.b)+sd(t.x)^2/length(t.x))
        }else{
          ttr <- t.test(t.x,alternative = tail,mu=mu, conf.level = cl)
        }
        
        df <- ttr$parameter
        tcv <- qt(alpha/2,df)
        t.s <- ttr$statistic
        t.p <- 2*(pt(-abs(t.s),df))
        U <- max(abs(tcv),abs(t.s))+2
        L <- -1*U
        x <- seq(from = L, to = U, by = .01)
        s.df <- data.frame(x,y=dt(x,df))
        tp <- s.df %>% ggplot(aes(x,y))+geom_line()+
          geom_area(data=subset(s.df,x <= -abs(tcv)),aes(y=y), fill ="red", alpha = .5) +
          geom_area(data=subset(s.df,x >= abs(tcv)),aes(y=y), fill ="red", alpha = .5) +
          geom_area(data=subset(s.df,x <= -abs(t.s)),aes(y=y), fill ="blue", alpha = .5) +
          geom_area(data=subset(s.df,x >= abs(t.s)),aes(y=y), fill ="blue", alpha = .5)
      } #Eif
      
      ############ t-Test Statistics Server #############################           
      
    }else if(input$Statistics == TRUE){
      if(input$tchoice != "2 Sample t-Test"){
        tmean <- input$tmean
        df <- input$tn - 1
        tse <- input$tsd/sqrt(df+1)
        t.s <- (tmean-mu)/(tse)
      }else if(input$tchoice == "2 Sample t-Test" && input$eqvar == TRUE){
        tmean <- input$tmean - input$tmean.b
        df <- input$tn + input$tn.b - 2
        sea <- input$tsd/sqrt(input$tn)
        seb <- input$tsd.b/sqrt(input$tn.b)
        tse <- sqrt(sea^2+seb^2)
        t.s <- (input$tmean-input$tmean.b)/tse
      }else if(input$tchoice == "2 Sample t-Test" && input$eqvar == FALSE){
        sea <- input$tsd/sqrt(input$tn)
        seb <- input$tsd.b/sqrt(input$tn.b)
        tse <- sqrt(sea^2+seb^2)
        df <- tse^4/(sea^4/(input$tn-1)+seb^4/(input$tn.b-1))
      }
      
      if (tail == "less"){
        cl <- 1 - 2*alpha
        tcv <- qt(alpha,df)
        t.p <- pt(t.s,df)
        U <- max(abs(tcv),abs(t.s))+1
        L <- -1*U
        x <- seq(from = L, to = U, by = .01)
        s.df <- data.frame(x,y=dt(x,df))
        tp <- s.df %>% ggplot(aes(x,y))+geom_line()+
          geom_area(data=subset(s.df,x<=tcv),aes(y=y), fill ="red", alpha = .5) +
          geom_area(data=subset(s.df,x<=t.s),aes(y=y), fill ="blue", alpha = .5)
      }else if(tail == "greater"){
        cl <- 1 - 2*alpha
        tcv <- qt(1-alpha,df)
        t.p <- 1 - pt(t.s,df)
        U <- max(abs(tcv),abs(t.s))+1
        L <- -1*U
        x <- seq(from = L, to = U, by = .01)
        s.df <- data.frame(x,y=dt(x,df))
        tp <- s.df %>% ggplot(aes(x,y))+geom_line()+
          geom_area(data=subset(s.df,x>=tcv),aes(y=y), fill ="red", alpha = .5) +
          geom_area(data=subset(s.df,x>=t.s),aes(y=y), fill ="blue", alpha = .5)
      }else if(tail == "two.sided"){
        cl <- 1 - alpha
        tcv <- qt(alpha/2,df)
        t.p <- 2*(pt(-abs(t.s),df))
        U <- max(abs(tcv),abs(t.s))+2
        L <- -1*U
        x <- seq(from = L, to = U, by = .01)
        s.df <- data.frame(x,y=dt(x,df))
        tp <- s.df %>% ggplot(aes(x,y))+geom_line()+
          geom_area(data=subset(s.df,x <= -abs(tcv)),aes(y=y), fill ="red", alpha = .5) +
          geom_area(data=subset(s.df,x >= abs(tcv)),aes(y=y), fill ="red", alpha = .5) +
          geom_area(data=subset(s.df,x <= -abs(t.s)),aes(y=y), fill ="blue", alpha = .5) +
          geom_area(data=subset(s.df,x >= abs(t.s)),aes(y=y), fill ="blue", alpha = .5)
      } #Eif
    } #Eif
    
    m.e <- abs(tcv) * tse
    ttt <- matrix(formatC(c(df,t.s,t.p,tcv,tse,m.e),
                          format="f",digits = 6,drop0trailing = TRUE),ncol=6,nrow=1)
    colnames(ttt) <- c("df","t-score","P-Value","CV","SE","ME")
    
    tp <- tp + scale_x_continuous(sec.axis = sec_axis(~.*tse+mu, name = "A")) +
      theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
      labs(x = "t")
    
    
    upper <- tmean+m.e
    lower <- tmean-m.e
    CI.t <- paste(cl*100,"% confidence interval is (",lower,",",upper,")")
    output$ttgraph <- renderPlot({grid.arrange(tableGrob(ttt),tp,textGrob(CI.t),ncol=1)})
  })#EobserveEvent
  
  # <<<<<<<<<<<<< End of T-test Tab Server  
  
  ####################### Start of ANOVA tab Server
  
  ####### Stats or Data Anovoa ########
  
  observeEvent(input$anovastat, {
    if(input$anovastat == FALSE){
      output$anovat <- renderRHandsontable({rhandsontable(anova.in$values)})
    }else if(input$anovastat == TRUE){
      output$anovat <- renderRHandsontable({rhandsontable(anova.s.in$values)})
    } #Eif
  }) #EobserveEvent
  
  ####### Anova Plot Button ########
  
  observeEvent(input$anovaplot, {
    if(input$anovastat == FALSE){
      anova.in$values <- hot_to_r(input$anovat)
      anova.long <- pivot_longer(anova.in$values,cols = 1:6, names_to = "Factor")%>%na.omit()
      abp <- anova.long %>% ggplot(aes(x=Factor, y=value)) +geom_boxplot(color="darkblue", fill="lightblue", outlier.colour="red", outlier.shape=8, outlier.size=4) + 
        geom_jitter(width = .1) + theme_classic()
      output$anovabox <- renderPlot({abp})
      
      aqq <- anova.long %>% ggplot(aes(sample = value, fill = Factor)) +
        stat_qq_band() + stat_qq_line() + stat_qq_point() +
        facet_wrap(~Factor) + theme(legend.position = "none")   +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles") 
      output$anovaqq <- renderPlot({aqq})
      
      anova.int <- anova.long %>% group_by(Factor) %>% summarise(count = length(value),
                                                                 mean = mean(value),
                                                                 sd = sd(value),
                                                                 var = var(value))
      a.count <- anova.int$count
      a.mean <- anova.int$mean
      a.sd <- anova.int$sd
      a.var <- anova.int$var
      
      
    }else if(input$anovastat == TRUE){
      
      anova.stats <- hot_to_r(input$anovat) %>% select_if(~!all(is.na(.)))
      a.count <- as.numeric(anova.stats[1,])
      a.mean <- as.numeric(anova.stats[2,])
      a.sd <- as.numeric(anova.stats[3,])
      a.var <- a.sd^2
      
    } #Eif  
    
      all.mean <- sum(a.mean*a.count)/sum(a.count)
      a.dmb <- a.count*(a.mean-all.mean)^2
      ave.a.mean <- mean(a.mean)
      a.vw <- (a.count-1)*a.var
      
      anova.r <- rbind(a.count,a.mean,a.sd,a.var,a.dmb,a.vw)
      colnames(anova.r)<-LETTERS[1:length(a.count)]
      rownames(anova.r)<-c("count","mean","SD","Variance","Between","Within")
      
      anova.ut <- formatC(anova.r,format="f",digits = 6,drop0trailing = TRUE)
      
      output$anovaut <- renderTable({anova.ut},rownames = TRUE,colnames=TRUE)

      sum.between <- sum(a.dmb)
      sum.within <- sum(a.vw)
      sum.total <- sum(a.dmb) + sum(a.vw)
      df.between <- length(a.count) - 1
      df.within <- sum(a.count)-length(a.count)
      df.total <- sum(a.count) - 1 
      ms.between <- sum.between / df.between
      ms.within <- sum.within / df.within
      ms.total <- sum.total / df.total
      F.stat <- ms.between / ms.within
      F.pvalue <- pf(F.stat, df.between, df.within, lower.tail = FALSE)
      alpha <- input$F.alpha
      F.cv <- qf(1-alpha,df.between, df.within)
      
      Result.1 <- c(sum.between,df.between,ms.between,F.stat,F.pvalue)
      Result.2 <- c(sum.within,df.within,ms.within,0,0)
      Result.3 <- c(sum.total,df.total,ms.total,F.cv,alpha)
      
      anova.lt <- rbind(Result.1,Result.2,Result.3)
      colnames(anova.lt) <- c("Sum of Squares","df","Mean Square","F Stat","P-Value")
      rownames(anova.lt) <- c("Between (TR)","Within (E)","Total")
      anova.lt <- formatC(anova.lt,format="f",digits = 6,drop0trailing = TRUE)
      anova.lt[2,4] <- "F.CV"
      anova.lt[2,5] <- "alpha"
      output$anovalt <- renderTable({anova.lt},rownames = TRUE,colnames=TRUE)
      
      x <- seq(from = 0, to = max(F.stat,F.cv)+2, by = .01)
      s.df <- data.frame(x,y=df(x,df.between,df.within))
      fp <- s.df %>% ggplot(aes(x,y))+geom_line()+
          geom_area(data=subset(s.df,x>=F.cv),aes(y=y), fill ="red", alpha = .5) +
          geom_area(data=subset(s.df,x>=F.stat),aes(y=y), fill ="blue", alpha = .5) +
          theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
          labs(x = "F") 
      output$F.plot <- renderPlot({fp})
  }) #EobserveEvent
  
    ############################# End ANOVA Server #########################
    ############################# Proportions Server #######################
  
  observeEvent(c(input$ptest, input$ptail), {
    if(input$ptest == 0){return()}
    alpha <- input$pAlpha
    ptail <- input$ptail
    if(ptail != "two.tail"){cl <- 1-2*alpha}else if(ptail == "two.tail"){cl <- 1-alpha}
    x1 <- input$x1
    n1 <- input$n1
    ph0 <- input$ph0
    if(input$props == "1 Proportion"){
      prop.test <- prop.test(x1,n1,ph0,alternative = input$ptail, conf.level = cl)
      low <- prop.test$conf.int[1]
      high <- prop.test$conf.int[2]
      dat <- data.frame(set = c("Set 1","Null"),prop = c(x1/n1,ph0))
      pplot<- dat %>% ggplot(aes(y=prop))+
        geom_rect(aes(xmin=-.1, xmax=.1, ymin = 0, ymax = x1/n1), fill = "blue") +
        geom_rect(aes(xmin=.1, xmax=.3, ymin = 0, ymax = ph0), fill = "red") + 
        xlim(c(-.2,.4)) +
        geom_errorbar(aes(x = 0, ymin = low, ymax = high)) +
        geom_point(x=0,y=x1/n1)+
        theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
    }else if(input$props == "2 Proportions"){
      x2 <- input$x2
      n2 <- input$n2  
      pm <- matrix(c(x1,n1-x1,x2,n2-x2),byrow = FALSE, nrow = 2)
      prop.test <- prop.test(pm,ph0,alternative = input$ptail, conf.level = cl)
      low <- prop.test$conf.int[1]
      high <- prop.test$conf.int[2]
      dat <- data.frame(set = c("Set 1","Set 2"),prop = c(x1/n1,x2/n2))
      pplot<- dat %>% ggplot(aes(y=prop))+
        geom_rect(aes(xmin=-.1, xmax=.1,ymin=0, ymax = x1/n1), fill = "blue") +
        geom_rect(aes(xmin=-.1, xmax=.1,ymin=-x2/n2, ymax = 0), fill = "red") +
        xlim(c(-.2,.2)) +
        geom_errorbar(aes(x = 0, ymin = low, ymax = high)) +
        geom_point(x=0,y=x1/n1-x2/n2)+
        scale_fill_brewer(palette="Dark2") +
        theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
    } #endif
 
    output$pplot <- renderPlot({pplot})
  }) #EobserveEvent
  
    ########################## End Proportions Server ######################
    ########################## 
  
} #end of the server

# Run the application 
shinyApp(ui = ui, server = server)