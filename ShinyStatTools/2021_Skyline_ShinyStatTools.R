library(ggpmisc)
library(janitor)
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
library(ggExtra)
library(plotly)
theme_set(theme_bw())

chisq.test <- stats::chisq.test

data.descr <- data.frame(matrix(numeric(), nrow=500, ncol=1))
colnames(data.descr) <- "A"
data.ttest <- data.frame(matrix(numeric(), nrow=500, ncol=1))
colnames(data.ttest) <- "A"
data.anova <- data.frame(matrix(numeric(), nrow=300, ncol=6))
colnames(data.anova) <- c("A","B","C","D","E","F")
data.anova.s <- data.frame(matrix(numeric(), nrow=3, ncol=6))
colnames(data.anova.s) <- c("A","B","C","D","E","F")
rownames(data.anova.s) <- c("Count","Mean","SD")
data.Chi.ti <- data.frame(matrix(integer(), nrow=6, ncol=6))
colnames(data.Chi.ti) <- c("X1","X2","X3","X4","X5","X6")
rownames(data.Chi.ti) <- c("Y1","Y2","Y3","Y4","Y5","Y6")
data.Chi.gof <- data.frame(matrix(integer(), nrow=2, ncol=6))
colnames(data.Chi.gof) <- c("X1","X2","X3","X4","X5","X6")
rownames(data.Chi.gof) <- c("Obs","Exp")
data.lr <- data.frame(matrix(numeric(), nrow=500, ncol=2))
colnames(data.lr) <- c("x","y")
data.disc <- data.frame(matrix(numeric(), nrow= 2, ncol=5))
rownames(data.disc) <- c("X","prob(x)")
binwidth <- 1

# >>>>>>>>>>>>>>>Start of UI

ui <- dashboardPage(
  dashboardHeader(title = "Shiny Stat Tools v1.4",titleWidth = "450px",
                  tags$li(class = "dropdown",tags$a("by Ken Harmon")),
                  dropdownMenuOutput(outputId = "notifications")),
  
  # >>>>>>>>>>>>>>>Side Bar  
  
  dashboardSidebar(width = 150,
                   sidebarMenu(
                     menuItem("Descriptive Stats", tabName = "ds"),
                     menuItem("Normal", tabName = "normal"),
                     menuItem("All t-Tests", tabName = "tTest"),
                     menuItem("ANOVA", tabName = "anova"),
                     menuItem("Proportions", tabName = "props"),
                     menuItem("Chi-Square", tabName = "Chi"),
                     menuItem("Linear Regression", tabName = "LR"),
                     menuItem("Discrete", tabName = "disc"),
                     menuItem("Data Sets", tabName = "datasets")
                   ) #End sidebarMenu
  ), #End dashboardSidebar
  
  # <<<<<<<<<<<<<End Sidebar
  # >>>>>>>>>>>>>Dashboard Body
  
  dashboardBody(
    
    tabItems(
      
      # >>>>>>>>>>>>>> Descriptive Tab UI
      
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
                           plotlyOutput("hist")
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
      
      # <<<<<<<<<<<<< Descriptive Tab UI
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
                column(width = 4, offset = 0, style='padding:0px;',
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
                column(width = 3, offset = 0, style='padding:0px;',
                       box(title = "Graphs",width = 12, background = "blue", 
                           plotOutput("pplot")
                       ) #Ebox
                ), #Ecolumn Middle  
                column(width = 5, offset = 0, style='padding:0px;',
                       box(title = "Results",width = 12,  
                           splitLayout(
                             conditionalPanel(condition = "input.props == '1 Proportion'",numericInput("ph0","Null:",.5,width=NULL)),
                             numericInput("pAlpha","Alpha:",.05,width=NULL),
                             radioButtons("ptail","",c("Left Tail"="less","Two Tail"="two.sided","Right Tail"="greater"),inline = FALSE,width = "50%"),
                             actionButton("ptest","Test")
                           ), #EsplitLayout
                           plotOutput("ptgraph")
                       ) #Ebox
                ), #Ecolumn Right 
              ) #EfluidRow
      ), #EtabItem props
      
      ######################## End Proportions UI #############################
      ######################## Chi Square UI ##################################
      
      tabItem("Chi",
              fluidRow(
                column(width = 6, offset = 0, style='padding:0px;',
                       box(title = "Data Input", width = 12, 
                           splitLayout(
                             radioButtons("chic","",c("Independence","Goodness of Fit")),
                             numericInput("chi.alpha","Alpha:",.05, width = 100)
                           ), #EsplitLayout
                           actionButton("Chi.r", "Reset"),
                           actionButton("Chitest", "Test"),
                           rHandsontableOutput("Chi"),
                           tags$hr(style="border-color: blue;"),
                           textOutput("Chiexptitle"),
                           tableOutput("Chiexp"),
                           tags$hr(style="border-color: blue;"),
                           textOutput("ChiSquares"),
                           tableOutput("chicell"),
                       ), #Ebox
                ), #Ecolumn left
                column(width = 6, offset = 0, style='padding:0px;',
                       box(title = "Results", width = 12, 
                           tableOutput("Chiresult"),
                           tags$hr(style="border-color: blue;"),
                           plotOutput("Chi.plot")
                       ), #Ebox
                ), #Ecolumn right
              ) #EfluidRow
      ), #EtabItem Chi
      
      ######################## End Chi Square UI #############################
      ######################## Linear Regression UI #############################
      
      tabItem("LR",
              fluidRow(
                column(width = 3,
                       box(title = "Data Input",width = NULL,status = "primary",
                           actionButton("lr.reset", "Reset"),
                           actionButton("lr.test", "Test"),
                           rHandsontableOutput("lr.data")
                       ), #Ebox
                ), #Ecolumn
                column(width = 4,
                       box(title = "Graphs", width = NULL, background = "blue",
                           plotOutput("lr.big3"),
                           valueBoxOutput("lr.qqplot",width = 6)
                       ), #Ebox
                ), #Ecolumn
                column(width = 5,
                       box(title = "Test Results", width = NULL,
                           tableOutput("lr.stats"),
                           tableOutput("lr.rstats"),
                           tableOutput("lr.test"),
                           numericInput("lr.alpha","Alpha:",.05),
                           textOutput("ci.slope"),
                           splitLayout(
                             numericInput("lr.x","x:",26),
                             tableOutput("lr.point")
                           ), #EsplitLayout
                           textOutput("ci.meany"),
                           textOutput("pi.y")
                       ), #Ebox
                ) #Ecolumn
              ) #EfluidRow
      ), #EtabItem LR
      
      ######################## End Linear Rergression UI #####################
      ########################## Discrete Probability UI #######################
      
      tabItem("disc",
              fluidRow(
                column(width = 6,
                       box(title = "Discrete Probability",width = NULL,status = "primary",
                           rHandsontableOutput("disc.data"),
                           splitLayout(
                             actionButton("disc.reset","Clear"),
                             actionButton("disc.add","Add Column"),
                             actionButton("disc.run","Run"),
                             plotlyOutput("DPHist")
                           ), #EsplitLayout
                           tableOutput("disc.results")
                       ), #Ebox
                       box(title = "Binomial", width = NULL, status = "primary",
                           splitLayout(
                             numericInput("bip","P(Hit)",.5),
                             numericInput("bih", "Hits",0),
                             numericInput("bit", "Tries",5)
                           ),
                           actionButton("bi.run","Run"),
                           tableOutput("biout"),
                           plotOutput("biplot")
                       ) #Ebox
                ), #Ecolumn
                column(width = 6,
                       box(title = "Geometric", width = NULL, status = "primary",
                           splitLayout(
                             numericInput("gep","P(Hit)",.5),
                             numericInput("get", "Tries",5)
                           ),
                           actionButton("ge.run","Run"),
                           tableOutput("geout"),
                           plotOutput("geplot")
                       ), #Ebox
                       box(title = "Poisson", width = NULL, status = "primary",
                           splitLayout(
                             numericInput("poil","E(Hit)",2),
                             numericInput("poih", "Hits",5)
                           ),
                           actionButton("poi.run","Run"),
                           tableOutput("poiout"),
                           plotOutput("poiplot")
                       ), #Ebox
                ), #Ecolumn
              ) #EfluidRow
      ), #EtabItem LR
      
      ######################## End Discrete Probability UI #####################


      
      tabItem("datasets", "All the pre-built datasets will go here") #EtabItem Datasets
    ) #End tabItems
  ) #EdashboardBody
) #EdashboardPage

# >>>>>>>>>>> Start of the Server
server <- function(input, output, session) {
  
  # >>>>>>>>>> Variables
  
  desc.in <- reactiveValues(values = data.descr)
  hist.x <- reactiveValues(values = data.descr)
  t.in <- reactiveValues(values = data.ttest)
  anova.in <- reactiveValues(values = data.anova)
  anova.s.in <- reactiveValues(values = data.anova.s)
  chi.ti.in <- reactiveValues(values =  data.Chi.ti)
  chi.gof.in <- reactiveValues(values =  data.Chi.gof)
  lr.in <- reactiveValues(values =  data.lr)
  disc.in <- reactiveValues(values =  data.disc)
  
  #>>>>>>>>>> Descriptive Tab Server  
  
  output$dt <- renderRHandsontable({rhandsontable(desc.in$values)})
  
  observeEvent(eventExpr = input$plot, {
    desc.in$values <- hot_to_r(input$dt)
    if(sum(!is.na(desc.in$values[,1]))>1){
      hist.x <- desc.in$values[!is.na(desc.in$values)]
      count <- length(hist.x)
      bins <- ceiling(1+3.322*log10(count))
      if (count > 2) {binwidth <- (max(hist.x)-min(hist.x)+2)/(bins-2)}
      hist.df <- data.frame(hist.x)
      if(!is.null(tryCatch(ggplot(hist.df), error = function(e){}))){
        dd1 <- hist.df %>%
          ggplot() + 
          geom_histogram(aes(x=hist.df[,1]),color="darkblue", fill="lightblue",binwidth=binwidth)+
          labs(x="A") + xlim(c(min(hist.x)-binwidth,max(hist.x)+binwidth))
        dd1 <- ggplotly(dd1)
      } #Eif
      if(!is.null(tryCatch(ggplot(hist.df), error = function(e){}))){
        dd2 <- hist.df %>%
          ggplot() +
          geom_histogram(aes(y=hist.x),color="darkblue", fill="lightblue",binwidth=binwidth)+
          geom_boxplot(aes(x = "", y = hist.x),color="darkblue", fill="lightgreen", outlier.colour="red", outlier.shape=8, outlier.size=4) + 
          geom_jitter(aes(x = "", y = hist.x), color= "orange", width = .1) +
          coord_flip() +
          theme_classic() + ylim(c(min(hist.x)-binwidth,max(hist.x)+binwidth))
        dd3 <- ggplotly(dd2)
      } #Eif
      output$hist <- renderPlotly(dd3) #Eoutput$hist
      
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
    data.descr <- data.frame(matrix(NA_real_, nrow = 500, ncol = 1))
    colnames(data.descr) <- "A"
    desc.in <- reactiveValues(values = data.descr)
    output$dt <- renderRHandsontable({rhandsontable(desc.in$values)})
  }) #EobserveEvent
  observeEvent(eventExpr = input$goptile, {
    desc.in$values <- hot_to_r(input$dt)
    if(sum(!is.na(desc.in$values[,1]))>1){
      hist.x <- desc.in$values[!is.na(desc.in$values)]
    }
    ptileout <- quantile(hist.x, (input$ptile) / 100, type = 6)
    output$pptile <- renderText({paste("The ",input$ptile," percentile is: ",round(ptileout,2))})
  }) #EobserveEvent
  
  # <<<<<<<<<<<<<< End of descriptive Tab Server
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
      paste("Total Probability: ",tp)
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
    if(ptail != "two.sided"){cl <- 1-2*alpha}else if(ptail == "two.sided"){cl <- 1-alpha}
    pci.cv <- qnorm(mean(c(1,cl)))
    x1 <- input$x1
    n1 <- input$n1
    phat1 <- x1/n1
    ph0 <- input$ph0
    if(input$props == "1 Proportion"){
      pt.se <- sqrt(ph0*(1-ph0)/n1)
      pt.ts <- (phat1-ph0)/pt.se
      pci.se <- sqrt(phat1*(1-phat1)/n1)
      pci.me <- pci.cv*pci.se
      high <- phat1 + pci.me
      low <- phat1 - pci.me
      dat <- data.frame(set = c("Set 1","Null"),prop = c(phat1,ph0))
      pplot<- dat %>% ggplot(aes(y=prop))+
        geom_rect(aes(xmin=-.1, xmax=.1, ymin = 0, ymax = phat1), fill = "deepskyblue4") +
        geom_rect(aes(xmin=.1, xmax=.3, ymin = 0, ymax = ph0), fill = "deepskyblue2") + 
        xlim(c(-.15,.35)) +
        geom_errorbar(aes(x = 0, ymin = low, ymax = high),color="darkorange2") +
        geom_point(x=0,y=phat1)+ geom_text(aes(x=0,y=phat1/2,label="Set 1")) + geom_text(aes(x=.2,y=ph0/2,label="Null")) +
        theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
    }else if(input$props == "2 Proportions"){
      x2 <- input$x2
      n2 <- input$n2
      ph0 <- 0
      phat2 <- x2/n2
      phatp <- (x1+x2)/(n1+n2)
      pt.se <- sqrt(phatp*(1-phatp)*(1/n1+1/n2))
      pt.ts <- (phat1-phat2)/pt.se
      pci.se <- sqrt(phat1*(1-phat1)/n1+phat2*(1-phat2)/n2)
      pci.me <- pci.cv*pci.se 
      phatd <- phat1-phat2
      high <- phatd + pci.me
      low <- phatd - pci.me
      dat <- data.frame(set = c("Set 1","Set 2"),prop = c(phat1,phat2))
      pplot<- dat %>% ggplot(aes(y=prop))+
        geom_rect(aes(xmin=-.1, xmax=.1,ymin=0, ymax = phat1), fill = "deepskyblue4") +
        geom_rect(aes(xmin=-.1, xmax=.1,ymin=-phat2, ymax = 0), fill = "deepskyblue2") +
        xlim(c(-.15,.15)) +
        geom_errorbar(aes(x = 0, ymin = low, ymax = high),color="darkorange2") +
        geom_point(x=0,y=phatd)+ geom_text(aes(x=0,y=phat1/2,label="Set 1")) + geom_text(aes(x=0,y=-phat2/2,label="Set 2")) +
        scale_fill_brewer(palette="Dark2") +
        theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
    } #endif
    output$pplot <- renderPlot({pplot})
    
    x <- seq(from = -4, to = 4, by = .01)
    s.df <- data.frame(x,y=dnorm(x))
    if(ptail == "less"){
      pp <- s.df %>% ggplot(aes(x,y))+geom_line()+
        geom_area(data=subset(s.df,x <= -abs(pci.cv)),aes(y=y), fill ="red", alpha = .5) +
        geom_area(data=subset(s.df,x <= pt.ts),aes(y=y), fill ="blue", alpha = .5) 
      pt.pv <- pnorm(pt.ts)
    }else if(ptail == "greater"){
      pp <- s.df %>% ggplot(aes(x,y))+geom_line()+
        geom_area(data=subset(s.df,x >= abs(pci.cv)),aes(y=y), fill ="red", alpha = .5) +
        geom_area(data=subset(s.df,x >= pt.ts),aes(y=y), fill ="blue", alpha = .5)
      pt.pv <- 1-pnorm(pt.ts)
    }else if(ptail == "two.sided"){
      pp <- s.df %>% ggplot(aes(x,y))+geom_line()+
        geom_area(data=subset(s.df,x <= -abs(pci.cv)),aes(y=y), fill ="red", alpha = .5) +
        geom_area(data=subset(s.df,x >= abs(pci.cv)),aes(y=y), fill ="red", alpha = .5) +
        geom_area(data=subset(s.df,x <= -abs(pt.ts)),aes(y=y), fill ="blue", alpha = .5) +
        geom_area(data=subset(s.df,x >= abs(pt.ts)),aes(y=y), fill ="blue", alpha = .5)
      pt.pv <- 2*pnorm(-abs(pt.ts))
    } #Eif
    
    ptt <- matrix(formatC(c(pt.ts,pt.pv,pci.cv,pt.se,pci.se,pci.me),
                          format="f",digits = 6,drop0trailing = TRUE),ncol=6,nrow=1)
    colnames(ptt) <- c("z-score","P-Value","CV","Test SE","CI SE","ME")
    
    pp <- pp + scale_x_continuous(sec.axis = sec_axis(~.*pt.se+ph0, name = "P")) +
      theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
      labs(x = "Z")
    
    CI.p <- paste(cl*100,"% confidence interval is (",low,",",high,")")
    output$ptgraph <- renderPlot({grid.arrange(tableGrob(ptt),pp,textGrob(CI.p),ncol=1)})
  }) #EobserveEvent
  
  ########################## End Proportions Server ######################
  ########################## Chi Server ##################################
  
  observeEvent(c(input$Chi.r,input$chic),{
    if(input$chic == "Independence"){
      output$Chi <- renderRHandsontable({rhandsontable(chi.ti.in$values)}) 
    }else if(input$chic == "Goodness of Fit"){
      output$Chi <- renderRHandsontable({rhandsontable(chi.gof.in$values)})
    } #Eif
  }) #EobserveEvent
  
  observeEvent(input$Chitest,{
    if(input$chic == "Independence"){
      Chi.ti <- hot_to_r(input$Chi)%>% remove_empty(c("rows","cols"))
      c<-sum(substring(colnames(Chi.ti), 1, 1)== "X")
      r<-sum(substring(rownames(Chi.ti), 1, 1)== "Y")
      Chi.ti <- Chi.ti[1:r,1:c]
      chi.test <- chisq.test(Chi.ti)
      Chi.ti <- cbind(Chi.ti, Total = rowSums(Chi.ti, na.rm = TRUE))
      Chi.ti <- rbind(Chi.ti, Total = colSums(Chi.ti, na.rm = TRUE))
      Chi.ti <- cbind(Chi.ti,Perc = Chi.ti$Total / Chi.ti[r+1,c+1])
      Chi.ti <- rbind(Chi.ti,Perc = Chi.ti["Total",] / Chi.ti[r+1,c+1])
      Chi.ti[r+2,c+2] <- NA
      output$Chi <- renderRHandsontable({rhandsontable(Chi.ti)%>% hot_cols(format = "0", halign = "htCenter", digits = 5)%>% 
          hot_col(c("Total","Perc"), readOnly = TRUE) %>% hot_row(c(r+1,r+2), readOnly = TRUE)}) 
      output$Chiexptitle <- renderText({"Expected Values"})
      output$Chiexp <- renderTable({chi.test$expected},rownames = TRUE,colnames=TRUE, digits = 5)
      output$ChiSquares <- renderText({"Chi-Square Values"})
      ressq <- (chi.test$residuals)^2
      output$chicell <- renderTable({ressq},rownames = TRUE,colnames=TRUE, digits = 5)
      chi.cv <- qchisq(1-input$chi.alpha,chi.test$parameter)
      chit <- matrix(formatC(c(chi.test$statistic,chi.test$parameter,chi.cv,chi.test$p.value),
                             format="f",digits = 6,drop0trailing = TRUE),ncol=4,nrow=1)
      colnames(chit) <- c("Chi-score","df","Chi-CV","P-Value")
      output$Chiresult <- renderTable({chit})
      
      x <- seq(from = 0, to = max(chi.test$statistic,chi.cv)+5, by = .01)
      s.df <- data.frame(x,y=dchisq(x,chi.test$parameter))
      chip <- s.df %>% ggplot(aes(x,y))+geom_line()+
        geom_area(data=subset(s.df,x>=chi.cv),aes(y=y), fill ="red", alpha = .5) +
        geom_area(data=subset(s.df,x>=chi.test$statistic),aes(y=y), fill ="blue", alpha = .5) +
        theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
        labs(x = "Chi") 
      output$Chi.plot <- renderPlot({chip})
      
    }else if(input$chic == "Goodness of Fit"){
      Chi.gof <- hot_to_r(input$Chi)%>% remove_empty(c("cols"))
      c<-sum(substring(colnames(Chi.gof), 1, 1)== "X")
      Chi.gof.ob <- as.numeric(Chi.gof[1,1:c])
      Chi.gof.exp <- as.numeric(Chi.gof[2,1:c])
      chi.test <- chisq.test(x = Chi.gof.ob, p = Chi.gof.exp/sum(Chi.gof.ob))
      Chi.gof <- cbind(Chi.gof, Total = rowSums(Chi.gof, na.rm = TRUE))
      Chi.gof <- rbind(Chi.gof, Chi = chi.test$residuals^2)
      Chi.gof[3,c+1]<-NA
      output$Chi <- renderRHandsontable({rhandsontable(Chi.gof)%>% hot_cols(format = "0", halign = "htCenter")%>% 
          hot_col(ncol(Chi.gof), readOnly = TRUE) %>% hot_row(nrow(Chi.gof), readOnly = TRUE)}) 
      output$Chiexptitle <- renderText({""})
      output$Chiexp <- renderTable({})
      output$ChiSquares <- renderText({""})
      output$chicell <- renderTable({})
      chi.cv <- qchisq(1-input$chi.alpha,chi.test$parameter)
      chit <- matrix(formatC(c(chi.test$statistic,chi.test$parameter,chi.cv,chi.test$p.value),
                             format="f",digits = 6,drop0trailing = TRUE),ncol=4,nrow=1)
      colnames(chit) <- c("Chi-score","df","Chi-CV","P-Value")
      output$Chiresult <- renderTable({chit})
      
      x <- seq(from = 0, to = max(chi.test$statistic,chi.cv)+5, by = .01)
      s.df <- data.frame(x,y=dchisq(x,chi.test$parameter))
      chip <- s.df %>% ggplot(aes(x,y))+geom_line()+
        geom_area(data=subset(s.df,x>=chi.cv),aes(y=y), fill ="red", alpha = .5) +
        geom_area(data=subset(s.df,x>=chi.test$statistic),aes(y=y), fill ="blue", alpha = .5) +
        theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
        labs(x = "Chi") 
      output$Chi.plot <- renderPlot({chip})
    } #Eif
  })
  
  
  ########################## End Chi Server ##############################
  ########################## Linear Regression Server ####################
  
  output$lr.data <- renderRHandsontable({rhandsontable(lr.in$values)})
  
  observeEvent(input$lr.reset,{
    output$lr.data <- renderRHandsontable({rhandsontable(lr.in$values)})
  }) #EobserveEvent
  
  observeEvent(input$lr.test,{
    
    lr.data <- hot_to_r(input$lr.data)%>% remove_empty("rows")
    lr.plot <- lr.data %>% ggplot(aes(x=x, y=y)) + 
      geom_point(shape=18, color="blue")+
      geom_smooth(method=lm,formula = y ~ x, se=FALSE, color="darkred") +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   label.x.npc = "right", label.y.npc = 0.15,
                   formula = y ~ x, parse = TRUE, size = 3) +
      labs(title = "Scatterplot")
    
    fit <- lm(y~x, data=lr.data)
    res <- qplot(fitted(fit), resid(fit))+geom_hline(yintercept=0) +
      labs(title = "Residual Plot")
    
    qqres <- lr.data %>% ggplot(mapping = aes(sample = resid(fit))) +
      stat_qq_band() +
      stat_qq_line() +
      stat_qq_point() +
      labs(title= "Residual QQ Plot",x = "Theoretical Quantiles", y = "Sample Quantiles")
    output$lr.big3 <- renderPlot({grid.arrange(lr.plot,res,qqres,ncol = 1)})
    good <- shapiro.test(resid(fit))
    output$lr.qqplot <-renderValueBox({valueBox(tags$p(round(good$p.value,3), style = "font-size: 50%;"), subtitle = "p-value",width = 5,color = if (good$p.value < .05) {"red"} else {"green"})})
    
    lr.stats <- matrix(formatC(c(mean(lr.data$x),sd(lr.data$x),mean(lr.data$y),sd(lr.data$y)),
                               format="f",digits = 6,drop0trailing = TRUE),ncol=4,nrow=1)
    colnames(lr.stats) <- c("Mean x","SD x","Mean y","SD y")
    output$lr.stats <- renderTable({lr.stats})
    xxx <- summary(fit)
    slope <- xxx$coefficients[2,1]
    yint <- xxx$coefficients[1,1]
    r <- cor(lr.data$x,lr.data$y)
    r2 <- r^2
    see <- xxx$sigma
    lr.rstats <- matrix(formatC(c(r,r2,see),
                                format="f",digits = 6,drop0trailing = TRUE),ncol=3,nrow=1)
    colnames(lr.rstats) <- c("r","R sq","St Err est")
    output$lr.rstats <- renderTable({lr.rstats})
    xxxt <- xxx$coefficients
    rownames(xxxt) <- c("y-Int","Slope")
    output$lr.test <- renderTable({xxxt},digits = 6, rownames = TRUE)
    
    alpha <- input$lr.alpha
    lr.x <- input$lr.x
    cl <- 1 - alpha
    df <- xxx$df[2]
    slope.l <- round(slope + qt(alpha/2,df)*xxx$coefficients[2,2],6) 
    slope.u <- round(slope - qt(alpha/2,df)*xxx$coefficients[2,2],6) 
    output$ci.slope <- renderText({paste(cl*100,"% confidence interval for slope:(",slope.l,",",slope.u,")")})
    lr.y <- lr.x*slope + yint
    output$lr.point <- renderTable({rbind(c("y"),c(lr.y))},colnames = FALSE)
    semuhat <- see*sqrt(1/(df+2)+(lr.x-mean(lr.data$x))^2/((df+1)*(sd(lr.data$x))^2))
    cimu.l <- round(lr.y + qt(alpha/2,df)*semuhat,6)
    cimu.u <- round(lr.y - qt(alpha/2,df)*semuhat,6)
    output$ci.meany <- renderText({paste(cl*100,"% confidence interval for mean y: (",cimu.l,",",cimu.u,")")})
    seyhat <- see*sqrt(1+1/(df+2)+(lr.x-mean(lr.data$x))^2/((df+1)*(sd(lr.data$x))^2))
    piy.l <- round(lr.y + qt(alpha/2,df)*seyhat,6)
    piy.u <- round(lr.y - qt(alpha/2,df)*seyhat,6)
    output$pi.y <- renderText({paste(cl*100,"% prediction interval for y: (",piy.l,",",piy.u,")")})
  }) #EobserveEvent
  ########################## End Linear Regression Server ################
  ########################## Discrete Probability Server #################
  
  output$disc.data <- renderRHandsontable({rhandsontable(disc.in$values)})
  
  observeEvent(input$disc.reset,{
    output$disc.data <- renderRHandsontable({rhandsontable(disc.in$values)})
  }) #EobserveEvent
  
  observeEvent(input$disc.data,{	
    disc.data <- hot_to_r(input$disc.data)	
  }) #EobserveEvent
  
  observeEvent(input$disc.add,{
    disc.data <- hot_to_r(input$disc.data)
    data.disc <- cbind(disc.data,as.numeric(c(NA,NA)))
    lc <- ncol(data.disc)
    colnames(data.disc)[lc]<-paste("X",lc,sep = "")
    output$disc.data <- renderRHandsontable({rhandsontable(data.disc)})
  }) #End observeEvent disc.add
  
  observeEvent(input$disc.run,{
    set <- hot_to_r(input$disc.data)
    set <- set[,colSums(is.na(set)) == 0] #keep column with no NA
    x <- as.numeric(set[1,])
    px <- as.numeric(set[2,])
    m <- sum(x*px)
    pre <- (x-m)^2*px
    var <- sum(pre)
    sd <- sqrt(var)
    
    dp <- matrix(formatC(c(m,var,sd),
                         format="f",digits = 6,drop0trailing = TRUE),ncol=3,nrow=1)
    colnames(dp) <- c("Mean","Variance","SD")
    output$disc.results <- renderTable({dp})
    
    Prob <- px
    df <- data.frame(x=x, Prob=Prob)
    dph <- df %>% ggplot() + geom_histogram(aes(x=x, y=..density..,weight=Prob),
                                            bins = length(x), color = "blue", fill = "brown")
    gdph <- ggplotly(dph)
    
    output$DPHist <- renderPlotly(gdph) #Eoutput$DPHist
  })
  
  
observeEvent(input$bi.run,{
  bip <- input$bip
  bih <- input$bih
  bit <- input$bit
  bis <- dbinom(bih,bit,bip) 
  bisl <- pbinom(bih,bit,bip)
  bisg <- 1 + bis - bisl
  bimean <- bit*bip
  bisd <- sqrt(bit*bip*(1-bip))
  bt <- matrix(formatC(c(bis,bisl,bisg,bimean,bisd),
                       format="f",digits = 6,drop0trailing = TRUE),ncol=5,nrow=1)
  colnames(bt) <- c(paste("P=",bih),paste("P≤",bih),paste("P≥",bih),"Mean","SD")
  output$biout <- renderTable({bt})
  low <- floor(max(0, bimean - 3*bisd))
  high <- ceiling(min(bit, bimean + 3*bisd))
  biplot <- data.frame(hits = low:high, pmf = dbinom(x = low:high, size = bit, prob = bip)) %>%
    ggplot(aes(x = factor(hits), y = pmf)) + geom_col(fill="skyblue2") +
    geom_text(aes(label = round(pmf,3), y = pmf + 0.01),
              position = position_dodge(0.9), size = 3, vjust = .2, angle = 90) +
    labs(x = "Successes (x)",y = "probability") 
  output$biplot <- renderPlot({biplot})
}) #EobserveEvent
observeEvent(input$ge.run,{
  gep <- input$gep
  get <- input$get
  ges <- (1-gep)^(get-1)*gep 
  gesl <- 1-(1-gep)^get
  gesg <- 1 + ges - gesl
  gemean <- 1/gep
  gesd <- sqrt(1-gep)/gep
  gt <- matrix(formatC(c(ges,gesl,gesg,gemean,gesd),
                       format="f",digits = 6,drop0trailing = TRUE),ncol=5,nrow=1)
  colnames(gt) <- c(paste("P=",get),paste("P≤",get),paste("P≥",get),"Mean","SD")
  output$geout <- renderTable({gt})
  low <- 1
  high <- ceiling(gemean + 3*gesd)
  geplot <- data.frame(hits = low:high, pmf = dgeom(x = (low-1):(high-1), prob = gep)) %>%
    ggplot(aes(x = factor(hits), y = pmf)) + geom_col(fill="skyblue2") +
    geom_text(aes(label = round(pmf,3), y = pmf + 0.01),position = position_dodge(0.9),
              size = 3,vjust = 0.2, angle = 90) +
    labs(x = "First success (x)",y = "Probability") 
  output$geplot <- renderPlot({geplot})
}) #EobserveEvent
observeEvent(input$poi.run,{
  poil <- input$poil
  poih <- input$poih
  pois <- dpois(poih,poil) 
  poisl <- ppois(poih,poil)
  poisg <- 1 + pois - poisl
  poimean <- poil
  poisd <- sqrt(poil)
  pt <- matrix(formatC(c(pois,poisl,poisg,poimean,poisd),
                       format="f",digits = 6,drop0trailing = TRUE),ncol=5,nrow=1)
  colnames(pt) <- c(paste("P=",poih),paste("P≤",poih),paste("P≥",poih),"Mean","SD")
  output$poiout <- renderTable({pt})
  low <- floor(max(0,poimean - 3*poisd))
  high <- ceiling(poimean + 3*poisd)
  poiplot <- data.frame(hits = low:high, pmf = dpois(x = low:high, lambda = poil)) %>%
    ggplot(aes(x = factor(hits), y = pmf)) + geom_col(fill="skyblue2") +
    geom_text(aes(label = round(pmf,3), y = pmf + 0.01),position = position_dodge(0.9),
              size = 3,vjust = 0.2, angle = 90) +
    labs(x = "Hits (x)",y = "Probability") 
  output$poiplot <- renderPlot({poiplot})
}) #EobserveEvent
######################## End Discrete Probability Server #################

} #end of the server

# Run the application 
shinyApp(ui = ui, server = server)