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

data.discr <- data.frame(as.numeric(matrix(, nrow=500, ncol=1)))
colnames(data.discr) <- "A"
data.ttest <- data.frame(as.numeric(matrix(, nrow=500, ncol=1)))
colnames(data.ttest) <- "A"
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
      menuItem("One Sample t-Test", tabName = "tTest"),
      menuItem("Paired t-Test", tabName = "Pairedt"),
      menuItem("Two Sample t-Test",tabName = "2tTest"),
      menuItem("ANOVA", tabName = "ANOVA"),
      menuItem("One Prop z-Test", tabName = "1pzt"),
      menuItem("Two Prop z-Test", tabName = "2pzt"),
      menuItem("Chi-Square", tabName = "Chi"),
      menuItem("Linear Regression", tabName = "LR"),
      menuItem("Discrete", tabName = "Disc")
    ) #End sidebarMenu
  ), #End dashboardSidebar

# <<<<<<<<<<<<<End Sidebar
# >>>>>>>>>>>>>Dashboard Body

  dashboardBody(
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
            box(title = "qqplot", width = NULL, background = "aqua",
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
          column(width = 2,
            box(title = "Data Input",width = NULL,status = "primary",
              radioButtons("tchoice","Input:",c("Data","Statistics")),
              conditionalPanel(condition = "input.tchoice == 'Data'",
                 actionButton("cleart", "Clear"),
                 actionButton("plott", "Plot"),
                 rHandsontableOutput("dtt")
              ), #End of conditionalPanel
              conditionalPanel(condition = "input.tchoice == 'Statistics'",
                numericInput("tmean","Mean:",0),
                numericInput("tsd","Standard Deviation:",1),
                numericInput("tn","Count:",1)
              ), #End of conditionalPanel
            ), #Ebox
          ), #Ecolumn
          conditionalPanel(condition = "input.tchoice == 'Data'",
            column(width = 5,
               box(title = "Summary Statistics", width = NULL, background = "blue",
                 plotOutput("dsst"),
                 valueBoxOutput("qqalertt")
               ), #Ebox
            ), #Ecolumn
          ), #End of Conditional
          column(width = 5,
            box(title = "Hypothesis Test", width = NULL,
            splitLayout(
              numericInput("th0","Null:",0,width="60%"),
              numericInput("tAlpha","Alpha:",.05,width="60%"),
              radioButtons("ttail","",c("Left Tail"="less","Two Tail"="two.sided","Right Tail"="greater"),inline = FALSE,width = "50%"),
              actionButton("ttest","Test")
            ), #EsplitLayout
            plotOutput("ttgraph")
            ), #Ebox
          ) #Ecolumn
        ) #EfluidRow
      ), #EtabItem tTest

# <<<<<<<<<<<<<<<<< t-test TAB UI

      tabItem("tTestStats","tTestStats goes Here"), #EtabItem tTest
      tabItem("Pairedt","Pairedt goes Here"), #EtabItem Pairedt
      tabItem("2tTest","2tTest goes Here"), #EtabItem 2tTest
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

# >>>>>>>>>>> Start of the Server
server <- function(input, output, session) {
  
# >>>>>>>>>> Variables
  
  disc.in <- reactiveValues(values = data.discr)
  hist.x <- reactiveValues(values = data.discr)
  t.in <- reactiveValues(values = data.ttest)
  
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
  
  observeEvent(input$cleart, {
    data.ttest <- data.frame(matrix(NA_real_, nrow = 500, ncol = 1))
    colnames(data.ttest) <- "A"
    t.in <- reactiveValues(values = data.ttest)
    output$dtt <- renderRHandsontable({rhandsontable(t.in$values)})
  }) #EobserveEvent
  
  observeEvent(input$plott, {
    t.in$values <- hot_to_r(input$dtt)
    if(sum(!is.na(t.in$values[,1]))>1){
      t.x <- t.in$values[!is.na(t.in$values)]
      t.df <- data.frame(t.x)
      qqt <- t.df %>% ggplot(mapping = aes(sample = t.x)) +
        stat_qq_band() +
        stat_qq_line() +
        stat_qq_point() +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
      good <- shapiro.test(t.x)
      output$qqalertt <- renderValueBox({
        valueBox(round(good$p.value,3), subtitle = "p-value",width = 5,color = if (good$p.value < .05) {"red"} else {"green"})
      }) #Eoutput$qqalert      
      sx <- summary(t.x)
      dss <- matrix(formatC(c(length(t.x),sx[4],sd(t.x)),
                            format="f",digits = 7,drop0trailing = TRUE),ncol=3,nrow=1)
      colnames(dss) <- c("Count","Mean","Standard Dev")
      output$dsst <- renderPlot({grid.arrange(tableGrob(dss),qqt,ncol =1)})
    }#Eif
  }) #EobserveEvent
  observeEvent(input$ttest, {
    t.in$values <- hot_to_r(input$dtt)
    if(sum(!is.na(t.in$values[,1]))>1){
      t.x <- t.in$values[!is.na(t.in$values)]
    }else{t.x <- c(0,0,0,0,0,0,0,0)} #Eif
    alpha <- input$tAlpha
    mu <- input$th0
    tail <- input$ttail
    if(input$tchoice == "Data"){df <- length(t.x)-1}else{df <- input$tn - 1}
    if (tail == "less"){
      cl <- 1 - 2*alpha
      tcv <- qt(alpha,df)
      ttr <- t.test(t.x,alternative = tail,mu=mu, conf.level = cl)
      if(input$tchoice == "Data"){t.s <- ttr$statistic}else{t.s <- (input$tmean-mu)/(input$tsd/sqrt(df+1))}
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
      ttr <- t.test(t.x,alternative = tail,mu=mu, conf.level = cl)
      if(input$tchoice == "Data"){t.s <- ttr$statistic}else{t.s <- (input$tmean-mu)/(input$tsd/sqrt(df+1))}
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
      tcv <- qt(alpha/2,df)
      ttr <- t.test(t.x,alternative = tail,mu=mu, conf.level = cl)
      if(input$tchoice == "Data"){t.s <- ttr$statistic}else{t.s <- (input$tmean-mu)/(input$tsd/sqrt(df+1))}
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
    }
    ttt <- matrix(formatC(c(df,t.s,t.p,tcv),
                          format="f",digits = 7,drop0trailing = TRUE),ncol=4,nrow=1)
    colnames(ttt) <- c("df","t-score","P-Value","Critical Value")
    if(input$tchoice == "Data"){tmean <- mean(t.x)}else{tmean <- input$tmean}
    if(input$tchoice == "Data"){tsd <- sd(t.x)}else{tsd <- input$tsd}
    tp <- tp + scale_x_continuous(sec.axis = sec_axis(~.*tsd+tmean, name = "A")) +
      theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
      labs(x = "Z")
    m.e <- abs(tcv) * tsd/sqrt(df+1)
    upper <- tmean+m.e
    lower <- tmean-m.e
    CI.t <- paste(cl*100,"% confidence interval is (",lower,",",upper,")")
    output$ttgraph <- renderPlot({grid.arrange(tableGrob(ttt),tp,textGrob(CI.t),ncol=1)})
  })#EobserveEvent
  
# <<<<<<<<<<<<< End of T-test Tab Server  
  
} #end of the server

# Run the application 
shinyApp(ui = ui, server = server)