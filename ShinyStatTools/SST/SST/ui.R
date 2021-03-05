#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

# >>>>>>>>>>>>>>>Start of UI

ui <- dashboardPage(
    dashboardHeader(title = "Shiny Stat Tools v1.7",titleWidth = "450px",
                    tags$li(class = "dropdown",tags$a("by Ken Harmon")),
                    dropdownMenuOutput(outputId = "notifications")),
    
    # >>>>>>>>>>>>>>>Side Bar  
    
    dashboardSidebar(width = 150,
                     sidebarMenu(
                         menuItem("Descriptive Stats", tabName = "ds"),
                         menuItem("Normal", tabName = "normal"),
                         menuItem("Student's t", tabName = "student"),
                         menuItem("All t-Tests", tabName = "t_test_tab"),
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
            # >>>>>>>>>>>>>>>>> Student t TAB UI Start
            tabItem("student",
                    fluidRow(
                        column(width = 3,
                               box(title = "Selections", width = NULL, solidHeader = TRUE,
                                   radioButtons("st_way","",c("t to Prob","Prob to t")),
                                   numericInput("st_mu","Mean:",0),
                                   numericInput("st_sd", "Standard Dev:",1),
                                   numericInput("st_n", "Sample Size:",2),
                                   actionButton("st_reset","Reset")
                               ), #Ebox
                               conditionalPanel(condition = "input.st_way == 't to Prob'",
                                                helpText("Shade:"),
                                                checkboxInput("st_Left","Left"),
                                                checkboxInput("st_Center","Center"),
                                                checkboxInput("st_Right","Right"),
                                                helpText("t-Score Cut-offs"),
                                                checkboxInput("st_sym","Symmetric"),
                               ), #EconditionalPanel
                               conditionalPanel(condition = "input.st_way == 'Prob to t'",
                                                radioButtons("st_shade","Shade", choices = c("Left","Center","Right"))
                               ), #EconditionalPanel
                        ), #Ecolumn left
                        column(width = 9,
                               conditionalPanel(condition = "input.st_way == 't to Prob'",
                                                splitLayout(
                                                    numericInput("lt","Left t-Score",-1,width="25%",step = .1),
                                                    actionButton("f_t2p","Find Probability"),
                                                    numericInput("rt","Right t-Score",1,width="25%", step = .1)
                                                ), #EsplitLayout
                                                plotOutput("st_t2p_plot"),
                                                textOutput("st_t2p_text")
                               ), #EconditionalPanel
                               conditionalPanel(condition = "input.st_way == 'Prob to t'",
                                                splitLayout(
                                                    numericInput("tprob","Percent",40,width="25%"),
                                                    actionButton("f_p2t","Find Probability")
                                                ), #EsplitLayout
                                                plotOutput("st_p2t_plot"),
                                                textOutput("st_p2t_text")
                               ) #EconditionalPanel
                        ) #Ecolumn Main
                    ) #EfluidRow student Tab
            ), #EtabItem student
            # <<<<<<<<<<<<<<<<< Student t TAB UI End
            # >>>>>>>>>>>>>>>>> tTest TAB UI
            
            tabItem("t_test_tab",
                    fluidRow(
                        column(width = 3,
                               box(title = "Data Input",width = NULL,status = "primary",
                                   checkboxInput("Stat_check","Statistics"),
                                   conditionalPanel(condition = "input.Stat_check == 0",
                                                    radioButtons("t_choice","Input:",c("Single Data","Paired Data","2 Sample t-Test")),
                                                    actionButton("clear_t", "Clear"),
                                                    actionButton("plot_t", "Plot"),
                                                    rHandsontableOutput("dt_t")
                                   ), #End of conditionalPanel
                                   conditionalPanel(condition = "input.Stat_check == 1",
                                                    radioButtons("t_choice","Input:",c("Single Data","2 Sample t-Test")),
                                   ), #End of conditionalPanel
                                   conditionalPanel(condition = "input.Stat_check == 1 && input.t_choice == 'Single Data'",
                                                    numericInput("t_xbar","Mean (xbar):",0),
                                                    numericInput("t_sd","Standard Deviation (s):",10),
                                                    numericInput("t_n","Count (Sample Size):",2)
                                   ), #End of conditionalPanel
                                   conditionalPanel(condition = "input.Stat_check == 1 && input.t_choice == '2 Sample t-Test'",
                                                    numericInput("t_xbar.a","Mean A:",0),
                                                    numericInput("t_sd.a","Standard Deviation A:",1),
                                                    numericInput("t_n.a","Count A:",1),
                                                    numericInput("t_xbar.b","Mean B:",0),
                                                    numericInput("t_sd.b","Standard Deviation B:",1),
                                                    numericInput("t_n.b","Count B:",1)
                                   ), #End of conditionalPanel
                               ), #Ebox
                        ), #Ecolumn
                        conditionalPanel(condition = "input.Stat_check == 0",
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
                                       numericInput("t_h0","Null:",0,width=NULL),
                                       numericInput("t_alpha","Alpha:",.05,width=NULL),
                                       radioButtons("t_tail","",c("Left Tail"="less","Two Tail"="two.sided","Right Tail"="greater"),inline = FALSE,width = "50%"),
                                       actionButton("t_test_btn","Test")
                                   ), #EsplitLayout
                                   conditionalPanel(condition = "input.t_choice == '2 Sample t-Test'",checkboxInput("eqvar","Equal Variances",TRUE)),
                                   plotOutput("t_test_graph")
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
                                       actionButton("disc.run","Run")
                                   ), #EsplitLayout
                                   plotlyOutput("DPHist"),
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
            
