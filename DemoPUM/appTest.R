library(shiny) # for basic templates
library(shinyBS) # for popovers and tool tips
library(shinycssloaders) # for ui elements showing shiny loading
library(magrittr) # piping operator
library(ggplot2) # loading ggplot for the plot
library(pum) # our pum library

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Nav Bar to construct the set of options we have along with the name of the title
  
  titlePanel(title = "Power Under Multiplicity", windowTitle = "Power Under Multiplicity"), 
             tabsetPanel( id = "mainMenu",
               tabPanel("Home"),
               tabPanel("Educational Resources"),
               tabPanel("2_Level_Blocked_i1_2cfr", tabsetPanel( #2LBI for 2 Level Blocked I1
               tabPanel("Power Calculation", tabsetPanel( id = "subMenu",# To host Single and Explorer
                 tabPanel("Single Scenario",
                          sidebarLayout(
                            sidebarPanel(
                          # css to center the progress bar
                          tags$head(
                            tags$style(
                              HTML(".shiny-notification {
                              height: 50px;
                              width: 600px;
                              position:fixed;
                              top: calc(50% - 50px);
                              left: calc(45%);
                              right: calc(15%);
                              font-size: 100%;
                              text-align: center;
                              
                              }
                              .progress-message {
                              
                              padding-top: 0px;
                              padding-right: 3px;
                              padding-bottom: 3px;
                              padding-left: 10px;
                              font-weight: normal !important;
                              font-style: italic !important;
                              font-size: 15px;
                              }
                              
                              .progress-detail {
                              
                              padding-top: 0px;
                              padding-right: 3px;
                              padding-bottom: 3px;
                              padding-left: 3px;
                              font-weight: normal;
                              font-style: italic !important;
                              font-size: 15px;
                              }
                              
                              "
                              ) # html bracket
                            ) # css styling tag
                          ), # The header tag
                              
                          fluidRow(
                                  column(10,
                                         div(style = "display: inline-block, vertical-align:top;", 
                                             selectInput("designP2LBISS", "What Research Design is this for?", 
                                             choices = list("constantEffects" = "d2.1_m2fc", 
                                                            "fixedEffects" = "d2.1_m2ff", 
                                                            "randomEffects" = "d2.1_m2fr"))) # select input buttons div
                                  ), # column for inputs
                                  
                                  column(2, 
                                         div(style ="display: inline-block,vertical-align:top;",
                                             actionButton("question_designP2LBISS",
                                             label = "", 
                                             icon = icon("question"),
                                             style = "font-size: 10px;
                                                      margin-top: 28px;")) #div for button ends
                                  ) # column for buttons
                                  
                                ), # fluid Row to contain the question mark issue 
                                
                                bsPopover(id = "question_designP2LBISS", 
                                          title = NULL,
                                          content = paste0("For more information on different designs, please click!"),
                                          placement = "right", 
                                          trigger = "hover", 
                                          options = list(container = "body")), # the bsPopover for the more information section of the Shiny App
                                
                                fluidRow(
                                  column(10,
                                         div(style = "display: inline-block, vertical-align:top;", 
                                             selectInput("MTPP2LBISS", "Which MTP do you plan to use?", 
                                             choices = list("Bonferroni" = "Bonferroni", 
                                                            "Holm" = "Holm", 
                                                            "Benjamini-Hochberg" = "BH", 
                                                            "Westfall-Young-Single-Step" = "WY-SS", 
                                                            "Westfall-Young-Step-Down" = "WY-SD"),
                                                            multiple = TRUE)) # select input buttons div
                                  ), # column for inputs
                                  
                                  column(2, 
                                         div(style ="display: inline-block, 
                                             vertical-align:top;",
                                             actionButton("question_mtpP2LBISS",
                                                          label = "", 
                                                          icon = icon("question"),
                                                          style = "font-size: 10px;
                                                                   margin-top: 28px;")) #div for button ends
                                  ) # column for buttons
                                  
                                ), # fluid Row to contain the question mark issue 
                                
                                bsPopover(id = "question_mtpP2LBISS", 
                                          title = NULL,
                                          content = paste0("For more information on MTP, please click!"),
                                          placement = "right", 
                                          trigger = "hover", 
                                          options = list(container = "body")), # the bsPopover for the more information section of the Shiny App
                                
                                fluidRow(
                                  
                                  column(12,
                                         numericInput("MP2LBISS", 
                                                      "Number of Outcomes", 
                                                      min = 1, 
                                                      max = 10, 
                                                      value = 5, 
                                                      step = 1)
                                  ) # column for number of outcomes
                                
                                ), # number of outcomes and mdes
                                
                                fluidRow(
                                  
                                  column(10,
                                         textInput("MDESP2LBISS", 
                                                   "Enter MDES vector (comma delimited)", 
                                                   value = "0.125,0.125,0.125, 0,0")
                                         
                                  ), # column for MDES
                                  
                                  column(2, 
                                         div(style ="display: inline-block, 
                                             vertical-align:top;",
                                             actionButton("question_mdesP2LBISS",
                                                          label = "", 
                                                          icon = icon("question"),
                                                          style = "font-size: 10px;
                                                                   margin-top: 28px;")) #div for button ends
                                  ) # column for buttons
                                  
                                ), # fluid Row to contain the question mark issue 
                                
                                bsPopover(id = "question_mdesP2LBISS", 
                                          title = NULL,
                                          content = paste0("For more information on MTP, please click!"),
                                          placement = "right", 
                                          trigger = "hover", 
                                          options = list(container = "body")
                                          ), # the bsPopover for the more information section of the Shiny App
                                
                                fluidRow(
                                  
                                  column(12,
                                         
                                         numericInput("KP2LBISS", 
                                                      "Number of Districts", 
                                                      min = 1, 
                                                      max = 100, 
                                                      value = 1, 
                                                      step = 1))
                                ), # number of districts
                                
                                fluidRow(
  
                                  column(6,
  
                                         numericInput("JP2LBISS", 
                                                      "Number of blocks", 
                                                      min = 1, 
                                                      max = 100, 
                                                      value = 50, 
                                                      step = 1)
  
                                  ), # number of blocks
  
                                  column(6,
  
                                         numericInput("nbarP2LBISS",
                                                      "Number of units per block", 
                                                      min = 2, 
                                                      max = 100, 
                                                      value = 20, 
                                                      step = 1)
  
                                  ) # number of units per blocks
  
                                ), # Nmber of blocks and number of units per block
                                
                                fluidRow(
                                  
                                  column(10,
                                         textInput("R2.1P2LBISS", 
                                                   "Enter R2 vector (comma delimited)", 
                                                   value = "0.2, 0.2, 0.2, 0.2, 0.2")
                                         
                                  ), # column for MDES
                                  
                                  column(2, 
                                         div(style ="display: inline-block, 
                                             vertical-align:top;",
                                             actionButton("R2.1P2LBISS",
                                                          label = "", 
                                                          icon = icon("question"),
                                                          style = "font-size: 10px;
                                                                   margin-top: 28px;")) #div for button ends
                                  ) # column for buttons
                                  
                                ), # fluid Row to contain the question mark issue 
                                
                                bsPopover(id = "R2.1P2LBISS", 
                                          title = NULL,
                                          content = paste0("For more information on MTP, please click!"),
                                          placement = "right", 
                                          trigger = "hover", 
                                          options = list(container = "body")
                                ), # the bsPopover for the more information section of the Shiny App
                                
                                fluidRow(
                                  column(12,
                                         
                                         numericInput("rhoP2LBISS", 
                                                      "Correlation between outcomes", 
                                                      min = 0, 
                                                      max = 1, 
                                                      value = 0.5, 
                                                      step = 0.1 )
                                         
                                  ) # Number of Level 1 covariates
                                  
                                ), #fluid row for block level covariate inputs
                                
                                fluidRow(
                                  column(12,
                                         
                                         numericInput("numCovar.1P2LBISS", 
                                                      "Number of Level 1 Covariates", 
                                                      min = 0, 
                                                      max = 10, 
                                                      value = 1, 
                                                      step = 1 )
                                         
                                  )# Number of Level 1 Covariates
                                  
                                ), # column correlation btw tests & intraclass correlation!
                                
                                fluidRow(
                                  
                                  column(12,
                                         
                                         numericInput("tbarP2LBISS", 
                                                      "Proportion of Treatment assignment", 
                                                      min = 0.001, 
                                                      max = 1.0, 
                                                      value = 0.5, 
                                                      step = 0.001)
                                         
                                  ) # proportion of treatment assignment
                                ), # proprtion of treatement as assignment
                                
                                fluidRow(  
                                  
                                  column(12,
                                         
                                         numericInput("alphaP2LBISS", 
                                                      "Significance Level of Tests (alpha)", 
                                                      min = 0.001, 
                                                      max = 0.9, 
                                                      value = 0.05, 
                                                      step = 0.001)
                                         
                                  ) #Significance Level of Tests
                                  
                                ), # proportion of treatment assignment and significance level of tests
                                
                                fluidRow(
                                  
                                  column(6,
                                         actionButton("goButtonP2LBISS", "Go!") # Action Button to trigger other reactive values
                                  )
                                )
                            ), # Power calculation sidebarPanel
                          
                              mainPanel(
                              br(),    
                              br(),
                              
                              fluidRow(
                                column(8,
                                       plotOutput("powercalcGraphP2LBISS"))
                              ), # end of Fluid Row
                                    
                                br(), # To create spaces between Table and Plots
                                br(), # To create spaces between Table and Plots
                                br(), # To create spaces between Table and Plots
                                br(), # To create spaces between Table and Plots
                                br(), # To create spaces between Table and Plots
                                br(), # To create spaces between Table and Plots
                              
                              fluidRow(
                                column(12,
                                       tableOutput("powercalcTableP2LBISS")) #The power calculation table output
                              ) #fluidRow for first half of the page
                              
                              ) # Power calculation Main Panel
                              
                            ) # Power Calculation sidebar Layout
                          
                    ), # Single Scenario
                 
                 tabPanel("Explorer",
                          sidebarLayout(
                            sidebarPanel(
                              # css to center the progress bar
                              tags$head(
                                tags$style(
                                  HTML(".shiny-notification {
                              height: 50px;
                              width: 600px;
                              position:fixed;
                              top: calc(50% - 50px);
                              left: calc(45%);
                              right: calc(15%);
                              font-size: 100%;
                              text-align: center;
                              
                              }
                              .progress-message {
                              
                              padding-top: 0px;
                              padding-right: 3px;
                              padding-bottom: 3px;
                              padding-left: 10px;
                              font-weight: normal !important;
                              font-style: italic !important;
                              font-size: 15px;
                              }
                              
                              .progress-detail {
                              
                              padding-top: 0px;
                              padding-right: 3px;
                              padding-bottom: 3px;
                              padding-left: 3px;
                              font-weight: normal;
                              font-style: italic !important;
                              font-size: 15px;
                              }
                              
                              "
                                  ) # html bracket
                                ) # css styling tag
                              ), # The header tag
                              
                              fluidRow(
                                
                                column(10,
                                       div(style = "display: inline-block, vertical-align:top;", 
                                           selectInput("explorerP2LBIE", "What Parameter would you like to vary?", 
                                                       choices = list("MDES", 
                                                                      "R2"))) # select input buttons div
                                ), # column for inputs
                                
                                column(2, 
                                       div(style ="display: inline-block,vertical-align:top;",
                                           actionButton("question_explorerP2LBIE",
                                                        label = "", 
                                                        icon = icon("question"),
                                                        style = "font-size: 10px;
                                                      margin-top: 28px;")) #div for button ends
                                ) # column for buttons
                                
                              ), # fluid Row for selection of which variables to explore
                              
                              
                              conditionalPanel(condition = "input.explorerP2LBIE == 'MDES'",
                              
                                  fluidRow(
                                    column(10,
                                           div(style = "display: inline-block, vertical-align:top;", 
                                               selectInput("designP2LBIEMDES", "What Research Design is this for?", 
                                                           choices = list("constantEffects" = "d2.1_m2fc", 
                                                                          "fixedEffects" = "d2.1_m2ff", 
                                                                          "randomEffects" = "d2.1_m2fr"))) # select input buttons div
                                    ), # column for inputs
                                    
                                    column(2, 
                                           div(style ="display: inline-block,vertical-align:top;",
                                               actionButton("question_designP2LBIEMDES",
                                                            label = "", 
                                                            icon = icon("question"),
                                                            style = "font-size: 10px;
                                                          margin-top: 28px;")) #div for button ends
                                    ) # column for buttons
                                    
                                  ), # fluid Row to contain the question mark issue 
                                  
                                  bsPopover(id = "question_designP2LBIEMDES", 
                                            title = NULL,
                                            content = paste0("For more information on different designs, please click!"),
                                            placement = "right", 
                                            trigger = "hover", 
                                            options = list(container = "body")), # the bsPopover for the more information section of the Shiny App
                                  
                                  fluidRow(
                                    column(10,
                                           div(style = "display: inline-block, vertical-align:top;", 
                                               selectInput("MTPP2LBIEMDES", "Which MTP do you plan to use?", 
                                                           choices = list("Bonferroni" = "Bonferroni", 
                                                                          "Holm" = "Holm", 
                                                                          "Benjamini-Hochberg" = "BH", 
                                                                          "Westfall-Young-Single-Step" = "WY-SS", 
                                                                          "Westfall-Young-Step-Down" = "WY-SD"),
                                                           multiple = TRUE)) # select input buttons div
                                    ), # column for inputs
                                    
                                    column(2, 
                                           div(style ="display: inline-block, 
                                                 vertical-align:top;",
                                               actionButton("question_mtpP2LBIEMDES",
                                                            label = "", 
                                                            icon = icon("question"),
                                                            style = "font-size: 10px;
                                                                       margin-top: 28px;")) #div for button ends
                                    ) # column for buttons
                                    
                                  ), # fluid Row to contain the question mark issue 
                                  
                                  bsPopover(id = "question_mtpP2LBIEMDES", 
                                            title = NULL,
                                            content = paste0("For more information on MTP, please click!"),
                                            placement = "right", 
                                            trigger = "hover", 
                                            options = list(container = "body")), # the bsPopover for the more information section of the Shiny App
                                  
                                  fluidRow(
                                    
                                    column(12,
                                           numericInput("MP2LBIEMDES", 
                                                        "Number of Outcomes", 
                                                        min = 1, 
                                                        max = 10, 
                                                        value = 5, 
                                                        step = 1)
                                    ) # column for number of outcomes
                                    
                                  ), # number of outcomes and mdes
                                  
                                  fluidRow(
                                    
                                    column(10,
                                           textInput("MDESP2LBIEMDES", 
                                                     "Vary MDES vector (comma delimited)", 
                                                     value = "0.125,0.125,0.125, 0,0")
                                           
                                    ), # column for MDES
                                    
                                    column(2, 
                                           div(style ="display: inline-block, 
                                                 vertical-align:top;",
                                               actionButton("question_mdesP2LBIEMDES",
                                                            label = "", 
                                                            icon = icon("question"),
                                                            style = "font-size: 10px;
                                                                       margin-top: 28px;")) #div for button ends
                                    ) # column for buttons
                                    
                                  ), # fluid Row to contain the question mark issue 
                                  
                                  bsPopover(id = "question_mdesP2LBIEMDES", 
                                            title = NULL,
                                            content = paste0("For more information on MTP, please click!"),
                                            placement = "right", 
                                            trigger = "hover", 
                                            options = list(container = "body")
                                  ), # the bsPopover for the more information section of the Shiny App
                                  
                                  fluidRow(
                                    
                                    column(12,
                                           
                                           numericInput("KP2LBIEMDES", 
                                                        "Number of Districts", 
                                                        min = 1, 
                                                        max = 100, 
                                                        value = 1, 
                                                        step = 1))
                                  ), # number of districts
                                  
                                  fluidRow(
                                    
                                    column(6,
                                           
                                           numericInput("JP2LBIEMDES", 
                                                        "Number of blocks", 
                                                        min = 1, 
                                                        max = 100, 
                                                        value = 50, 
                                                        step = 1)
                                           
                                    ), # number of blocks
                                    
                                    column(6,
                                           
                                           numericInput("nbarP2LBIEMDES",
                                                        "Number of units per block", 
                                                        min = 2, 
                                                        max = 100, 
                                                        value = 20, 
                                                        step = 1)
                                           
                                    ) # number of units per blocks
                                    
                                  ), # Nmber of blocks and number of units per block
                                  
                                  fluidRow(
                                    
                                    column(10,
                                           textInput("R2.1P2LBIEMDES", 
                                                     "Only 1 R Value allowed!", 
                                                     value = "0.2, 0.2, 0.2, 0.2, 0.2")
                                           
                                    ), # column for MDES
                                    
                                    column(2, 
                                           div(style ="display: inline-block, 
                                                 vertical-align:top;",
                                               actionButton("R2.1P2LBIEMDES",
                                                            label = "", 
                                                            icon = icon("question"),
                                                            style = "font-size: 10px;
                                                                       margin-top: 28px;")) #div for button ends
                                    ) # column for buttons
                                    
                                  ), # fluid Row to contain the question mark issue 
                                  
                                  bsPopover(id = "R2.1P2LBIEMDES", 
                                            title = NULL,
                                            content = paste0("For more information on MTP, please click!"),
                                            placement = "right", 
                                            trigger = "hover", 
                                            options = list(container = "body")
                                  ), # the bsPopover for the more information section of the Shiny App
                                  
                                  fluidRow(
                                    column(12,
                                           
                                           numericInput("rhoP2LBIEMDES", 
                                                        "Correlation between outcomes", 
                                                        min = 0, 
                                                        max = 1, 
                                                        value = 0.5, 
                                                        step = 0.1 )
                                           
                                    ) # Number of Level 1 covariates
                                    
                                  ), #fluid row for block level covariate inputs
                                  
                                  fluidRow(
                                    column(12,
                                           
                                           numericInput("numCovar.1P2LBIEMDES", 
                                                        "Number of Level 1 Covariates", 
                                                        min = 0, 
                                                        max = 10, 
                                                        value = 1, 
                                                        step = 1 )
                                           
                                    )# Number of Level 1 Covariates
                                    
                                  ), # column correlation btw tests & intraclass correlation!
                                  
                                  fluidRow(
                                    
                                    column(12,
                                           
                                           numericInput("tbarP2LBIEMDES", 
                                                        "Proportion of Treatment assignment", 
                                                        min = 0.001, 
                                                        max = 1.0, 
                                                        value = 0.5, 
                                                        step = 0.001)
                                           
                                    ) # proportion of treatment assignment
                                  ), # proprtion of treatement as assignment
                                  
                                  fluidRow(  
                                    
                                    column(12,
                                           
                                           numericInput("alphaP2LBIEMDES", 
                                                        "Significance Level of Tests (alpha)", 
                                                        min = 0.001, 
                                                        max = 0.9, 
                                                        value = 0.05, 
                                                        step = 0.001)
                                           
                                    ) #Significance Level of Tests
                                    
                                  ), # proportion of treatment assignment and significance level of tests
                                  
                                  fluidRow(
                                    
                                    column(6,
                                           actionButton("goButtonP2LBIEMDES", "Go!") # Action Button to trigger other reactive values
                                    )
                                  ) # goButtonP2LBIE
                                  
                              ), # end of MDES condtional Panel
                              
                        conditionalPanel(condition = "input.explorerP2LBIE == 'R2'",
                                               
                                  fluidRow(
                                      
                                    column(10,
                                           div(style = "display: inline-block, vertical-align:top;", 
                                           selectInput("designP2LBIER2", "What Research Design is this for?", 
                                                        choices = list("constantEffects" = "d2.1_m2fc", 
                                                                       "fixedEffects" = "d2.1_m2ff", 
                                                                       "randomEffects" = "d2.1_m2fr"))) # select input buttons div
                                          ), # column for inputs
                                                 
                                     column(2, 
                                           div(style ="display: inline-block,vertical-align:top;",
                                            actionButton("question_designP2LBIER2",
                                                          label = "", 
                                                          icon = icon("question"),
                                                          style = "font-size: 10px;
                                                          margin-top: 28px;")) #div for button ends
                                            ) # column for buttons
                                                 
                                         ), # fluid Row to contain the question mark issue 
                                               
                                      bsPopover(id = "question_designP2LBIER2", 
                                                title = NULL,
                                                content = paste0("For more information on different designs, please click!"),
                                                placement = "right", 
                                                trigger = "hover", 
                                                options = list(container = "body")), # the bsPopover for the more information section of the Shiny App
                                               
                                  fluidRow(
                                    
                                     column(10,
                                           div(style = "display: inline-block, vertical-align:top;", 
                                           selectInput("MTPP2LBIER2", "Which MTP do you plan to use?", 
                                                        choices = list("Bonferroni" = "Bonferroni", 
                                                                        "Holm" = "Holm", 
                                                                        "Benjamini-Hochberg" = "BH", 
                                                                        "Westfall-Young-Single-Step" = "WY-SS", 
                                                                        "Westfall-Young-Step-Down" = "WY-SD"),
                                                        multiple = TRUE)) # select input buttons div
                                        ), # column for inputs
                                                 
                                     column(2, 
                                          div(style ="display: inline-block, 
                                                 vertical-align:top;",
                                          actionButton("question_mtpP2LBIER2",
                                          label = "", 
                                          icon = icon("question"),
                                          style = "font-size: 10px;
                                          margin-top: 28px;")) #div for button ends
                                          ) # column for buttons
                                                 
                                       ), # fluid Row to contain the question mark issue 
                                               
                                      bsPopover(id = "question_mtpP2LBIER2", 
                                                title = NULL,
                                                content = paste0("For more information on MTP, please click!"),
                                                placement = "right", 
                                                trigger = "hover", 
                                                options = list(container = "body")), # the bsPopover for the more information section of the Shiny App
                                               
                                      fluidRow(
                                                 
                                          column(12,
                                            numericInput("MP2LBIER2", 
                                                          "Number of Outcomes", 
                                                          min = 1, 
                                                          max = 10, 
                                                          value = 5, 
                                                          step = 1)
                                            ) # column for number of outcomes
                                                 
                                        ), # number of outcomes and mdes
                                               
                                      fluidRow(
                                                 
                                          column(10,
                                            textInput("MDESP2LBIER2", 
                                                      "Only 1 MDES value allowed!", 
                                                      value = "0.125,0.125,0.125, 0,0")
                                                        
                                          ), # column for MDES
                                                 
                                          column(2, 
                                             div(style ="display: inline-block, 
                                                 vertical-align:top;",
                                            actionButton("question_mdesP2LBIER2",
                                                          label = "", 
                                                          icon = icon("question"),
                                                          style = "font-size: 10px;
                                                                  margin-top: 28px;")) #div for button ends
                                              ) # column for buttons
                                                 
                                          ), # fluid Row to contain the question mark issue 
                                               
                                          bsPopover(id = "question_mdesP2LBIER2", 
                                                    title = NULL,
                                                    content = paste0("For more information on MTP, please click!"),
                                                    placement = "right", 
                                                    trigger = "hover", 
                                                    options = list(container = "body")
                                      ), # the bsPopover for the more information section of the Shiny App
                                               
                                      fluidRow(
                                                 
                                          column(12,
                                            numericInput("KP2LBIER2", 
                                                          "Number of Districts", 
                                                            min = 1, 
                                                            max = 100, 
                                                            value = 1, 
                                                            step = 1))
                                               ), # number of districts
                                               
                                     fluidRow(
                                                 
                                         column(6,
                                            numericInput("JP2LBIER2", 
                                                          "Number of blocks", 
                                                            min = 1, 
                                                            max = 100, 
                                                            value = 50, 
                                                            step = 1)
                                                        
                                              ), # number of blocks
                                                 
                                         column(6,
                                            numericInput("nbarP2LBIER2",
                                                         "Number of units per block", 
                                                            min = 2, 
                                                            max = 100, 
                                                            value = 20, 
                                                            step = 1)
                                                        
                                          ) # number of units per blocks
                                                 
                                    ), # Nmber of blocks and number of units per block
                                               
                                    fluidRow(
                                                 
                                        column(10,
                                            textInput("R2.1P2LBIER2", 
                                                      "Vary R2 vector (comma delimited)", 
                                                      value = "0.2, 0.2, 0.2, 0.2, 0.2")
                                                        
                                             ), # column for MDES
                                                 
                                        column(2, 
                                            div(style ="display: inline-block, 
                                                 vertical-align:top;",
                                              actionButton("R2.1P2LBIER2",
                                                            label = "", 
                                                            icon = icon("question"),
                                                            style = "font-size: 10px;
                                                margin-top: 28px;")) #div for button ends
                                             ) # column for buttons
                                                 
                                     ), # fluid Row to contain the question mark issue 
                                               
                                       bsPopover(id = "R2.1P2LBIER2", 
                                                 title = NULL,
                                                 content = paste0("For more information on MTP, please click!"),
                                                 placement = "right", 
                                                 trigger = "hover", 
                                                 options = list(container = "body")
                                     ), # the bsPopover for the more information section of the Shiny App
                                               
                                      fluidRow(
                                        
                                            column(12,
                                                numericInput("rhoP2LBIER2", 
                                                             "Correlation between outcomes", 
                                                              min = 0, 
                                                              max = 1, 
                                                              value = 0.5, 
                                                              step = 0.1 )
                                                        
                                                 ) # Number of Level 1 covariates
                                                 
                                     ), #fluid row for block level covariate inputs
                                               
                                      fluidRow(
                                        
                                            column(12,
                                                numericInput("numCovar.1P2LBIER2", 
                                                             "Number of Level 1 Covariates", 
                                                              min = 0, 
                                                              max = 10, 
                                                              value = 1, 
                                                              step = 1 )
                                                        
                                                 )# Number of Level 1 Covariates
                                                 
                                     ), # column correlation btw tests & intraclass correlation!
                                               
                                     fluidRow(
                                                 
                                           column(12,
                                               numericInput("tbarP2LBIER2", 
                                                             "Proportion of Treatment assignment", 
                                                              min = 0.001, 
                                                              max = 1.0, 
                                                              value = 0.5, 
                                                              step = 0.001)
                                                        
                                                 ) # proportion of treatment assignment
                                           
                                    ), # proprtion of treatement as assignment
                                               
                                    fluidRow(  
                                                 
                                          column(12,
                                            numericInput("alphaP2LBIER2", 
                                                          "Significance Level of Tests (alpha)", 
                                                            min = 0.001, 
                                                            max = 0.9, 
                                                            value = 0.05, 
                                                            step = 0.001)
                                                        
                                                 ) #Significance Level of Tests
                                                 
                                    ), # proportion of treatment assignment and significance level of tests
                                               
                                    fluidRow(
                                                 
                                          column(6,
                                             actionButton("goButtonP2LBIER2", "Go!") # Action Button to trigger other reactive values
                                                 )
                                          ) # goButtonP2LBIE
                                               
                              ) # Conditional Panel for R2
                              
                            ), # Power calculation sidebarPanel
                            
                            mainPanel(
                              
                            
                              conditionalPanel(condition = "input.explorerP2LBIE == 'MDES'",
                                               
                                  br(),    
                                  br(),
                                  
                                  fluidRow(
                                    column(8,
                                           plotOutput("powercalcGraphP2LBIEMDES"))
                                  ), # end of Fluid Row
                              
                                  br(), # To create spaces between Table and Plots
                                  br(), # To create spaces between Table and Plots
                                  br(), # To create spaces between Table and Plots
                                  br(), # To create spaces between Table and Plots
                                  br(), # To create spaces between Table and Plots
                                  br(), # To create spaces between Table and Plots
                                  
                                  fluidRow(
                                    column(12,
                                           tableOutput("powercalcTableP2LBIEMDES")) #The power calculation table output
                                  ) #fluidRow for first half of the page
                                  
                              ), # conditional panel results for MDES
                              
                              conditionalPanel(condition = "input.explorerP2LBIE == 'R2'",
                                               
                                               br(),    
                                               br(),
                                               
                                               fluidRow(
                                                 column(8,
                                                        plotOutput("powercalcGraphP2LBIER2"))
                                               ), # end of Fluid Row
                                               
                                               br(), # To create spaces between Table and Plots
                                               br(), # To create spaces between Table and Plots
                                               br(), # To create spaces between Table and Plots
                                               br(), # To create spaces between Table and Plots
                                               br(), # To create spaces between Table and Plots
                                               br(), # To create spaces between Table and Plots
                                               
                                               fluidRow(
                                                 column(12,
                                                        tableOutput("powercalcTableP2LBIER2")) #The power calculation table output
                                               ) #fluidRow for first half of the page
                                               
                              ) # conditional panel results for R2
                            
                            ) # Power calculation Main Panel
                            
                          ) # Power Calculation sidebar Layout
                          
                 ) # Explorer
                  ) # Power Tablset Panel to hose Single and Explorer
                 
                 ) # Power Calculation Main and Side Bar Tab     
                   
               ), # End of Power Tabset
                 
                 # tabPanel("MDES Calculation",
                 #          sidebarLayout(
                 #            sidebarPanel(
                 #              
                 #              # css to center the progress bar
                 #              tags$head(
                 #                tags$style(
                 #                  HTML(".shiny-notification {
                 #         height: 50px;
                 #         width: 600px;
                 #         position:fixed;
                 #         top: calc(50% - 50px);
                 #         left: calc(45%);
                 #         right: calc(15%);
                 #         font-size: 100%;
                 #         text-align: center;
                 #         
                 #        }
                 #          .progress-message {
                 #         
                 #         padding-top: 0px;
                 #         padding-right: 3px;
                 #         padding-bottom: 3px;
                 #         padding-left: 10px;
                 #         font-weight: normal !important;
                 #         font-style: italic !important;
                 #         font-size: 15px;
                 #        }
                 #    
                 #          .progress-detail {
                 # 
                 #         padding-top: 0px;
                 #         padding-right: 3px;
                 #         padding-bottom: 3px;
                 #         padding-left: 3px;
                 #         font-weight: normal;
                 #         font-style: italic !important;
                 #         font-size: 15px;
                 #        }
                 #      "
                 #                  ) # html bracket
                 #                ) # css styling tag
                 #              ), # The header tag
                 #              
                 #              fluidRow(
                 #                column(6,
                 #                       div(style = "display: inline-block, vertical-align:top;", 
                 #                           selectInput("MTP_mdes", "MTP", 
                 #                                       choices = list("Bonferroni" = "BF", 
                 #                                                      "Benjamini-Hocheberg" = "BH", 
                 #                                                      "Holms" = "HO", 
                 #                                                      "Westfall-Young-SS" = "WY-SS", 
                 #                                                      "Westfall-Young-SD" = "WY-SD"),
                 #                                       selected = "BF")) # select input buttons div
                 #                       
                 #                ), # MTP, the Mutliple Testing Procedure in Use
                 #                
                 #                column(6,
                 #                       numericInput("M_mdes", "Number of Outcomes", min = 1, max = 10, value = 5, step = 1)
                 #                ) # M, the number of outcomes in use
                 #              ), # Fluid Row for MTP and M outcomes
                 #              
                 #              fluidRow(
                 #                
                 #                column(10,
                 #                       numericInput("numFalse_mdes", "Number of Outcomes with an expected non-zero effects", value = 3, min = 0, max = 10, step = 1)
                 #                ), # column for outcomes with actual effect size
                 #                
                 #                column(2,
                 #                       div(style ="display: inline-block, vertical-align:top;",actionButton("question_mdes_mdes",label = "", icon = icon("question"))) #div for button ends                            
                 #                ) # column for action button
                 #                
                 #              ), # MDES and number of outcomes with expected actual effect
                 #              
                 #              fluidRow(
                 #                column(6,
                 #                       numericInput("J_mdes", "Number of blocks", min = 1, max = 100, value = 50, step = 1)
                 #                ), # Number of Blocks
                 #                
                 #                column(6,
                 #                       numericInput("n.j_mdes","Number of units per block", min = 2, max = 100, value = 20, step = 1)     
                 #                       
                 #                ) # Fluid Row for Number of units per block and Number of units
                 #                
                 #              ), # Number of Block and Sample Size
                 #              
                 #              fluidRow(
                 #                column(6,
                 #                       numericInput("alpha_mdes", "Alpha value", min = 0.001, max = 0.9, value = 0.05, step = 0.001)
                 #                ), # alpha value
                 #                
                 #                column(6,
                 #                       numericInput("me_mdes", "Margin of error", min = 0.001, max = 0.9, value = 0.05, step = 0.001)
                 #                ) # Margins of error
                 #              ), # For Alpha and Margin of Error
                 #              
                 #              fluidRow(
                 #                
                 #                column(6,
                 #                       numericInput("numCovar.1_mdes", "Level 1 Covariates", min = 0, max = 10, value = 1, step = 1 )
                 #                ), # number of covariates at level 1
                 #                
                 #                column(6,
                 #                       numericInput("R2.1_mdes", "Level 1 R2", value = 0.2, min = 0, max = 1.0, step = 0.01)
                 #                ) # R2 (explanatory power at level 1)
                 #              ), # Number of Level 1 covariates and R^2 explanatory power
                 #              
                 #              fluidRow(
                 #                
                 #                column(6,
                 #                       numericInput("power_mdes", "Power Value", min = 0.001, max = 1.0, value = 0.75, step = 0.001)
                 #                ), # Power value
                 #                
                 #                column(6,
                 #                       uiOutput("power") #Dynamic Selector User Interface for power
                 #                ) # Choice of Power and Power Definition
                 #              ), # Fluid Row for Proportion of treatment assignment
                 #              
                 #              fluidRow(
                 #                
                 #                column(12,
                 #                       numericInput("p_mdes", "Proportion of Treatment assignment", min = 0.001, max = 1.0, value = 0.5, step = 0.001)
                 #                ) # Proportion of treatment assignment
                 #              ), # fluid row for Proportion of Treatment assignment
                 #              
                 #              fluidRow(
                 #                
                 #                column(12,
                 #                       actionButton("goButton_mdes", "Go!") # Action Button to trigger other reactive values
                 #                ) # Column for action button
                 #                
                 #              ) # fluid row for Action Button
                 #              
                 #            ), # Side Bar Panel
                 #            
                 #            mainPanel(
                 #              
                 #              fluidRow(
                 #                
                 #                column(12,
                 #                       tableOutput("mdes") # MDES part of a spinner
                 #                ) # Full column
                 #                
                 #              ) #fluidRow for first half of the page
                 #            ) # Main Panel Layout
                 #          ) # Sidebar Panel       
                 # ), # Tabpanel MDES calculation
                 # tabPanel("Sample Size Calculation",
                 #          sidebarLayout(
                 #            sidebarPanel(
                 #              
                 #              # css to center the progress bar
                 #              tags$head(
                 #                tags$style(
                 #                  HTML(".shiny-notification {
                 #             height: 50px;
                 #             width: 600px;
                 #             position:fixed;
                 #             top: calc(50% - 50px);
                 #             left: calc(45%);
                 #             right: calc(15%);
                 #             font-size: 100%;
                 #             text-align: center;
                 #             
                 #             }
                 #             .progress-message {
                 #             
                 #             padding-top: 0px;
                 #             padding-right: 3px;
                 #             padding-bottom: 3px;
                 #             padding-left: 10px;
                 #             font-weight: normal !important;
                 #             font-style: italic !important;
                 #             font-size: 15px;
                 #             }
                 #             
                 #             .progress-detail {
                 #             
                 #             padding-top: 0px;
                 #             padding-right: 3px;
                 #             padding-bottom: 3px;
                 #             padding-left: 3px;
                 #             font-weight: normal;
                 #             font-style: italic !important;
                 #             font-size: 15px;
                 #             }
                 #             
                 #             "
                 #                  ) # html bracket
                 #                ) # css styling tag
                 #              ), # The header tag
                 #              
                 #              fluidRow(
                 #                column(6,
                 #                       div(style = "display: inline-block, vertical-align:top;", 
                 #                           selectInput("MTP_sample", "MTP", 
                 #                                       choices = list("Bonferroni" = "BF", 
                 #                                                      "Benjamini-Hocheberg" = "BH", 
                 #                                                      "Holms" = "HO", 
                 #                                                      "Westfall-Young-SS" = "WY-SS", 
                 #                                                      "Westfall-Young-SD" = "WY-SD"),
                 #                                       selected = "BF")) # select input buttons div
                 #                       
                 #                ), # MTP, the Mutliple Testing Procedure in Use
                 #                
                 #                column(6,
                 #                       numericInput("M_sample", "Number of Outcomes", min = 1, max = 10, value = 5, step = 1)
                 #                ) # M, the number of outcomes in use
                 #              ), # Fluid Row for MTP and M outcomes
                 #              
                 #              fluidRow(
                 #                
                 #                column(10,
                 #                       numericInput("numFalse_sample", "Number of False Nulls", value = 3, min = 0, max = 10, step = 1)
                 #                ), # column for outcomes with actual effect size
                 #                
                 #                column(2,
                 #                       div(style ="display: inline-block, vertical-align:top;",actionButton("question_sample_sample",label = "", icon = icon("question"))) #div for button ends                            
                 #                ) # column for action button
                 #                
                 #              ), # MDES and number of outcomes with expected actual effect
                 #              
                 #              fluidRow(
                 #                
                 #                column(12,
                 #                       div(style = "display: inline-block, vertical-align:top;", 
                 #                           selectInput("typesample", "Which type of sample would you like to estimate ?", 
                 #                                       choices = list("Number of Blocks" = "J", "Number of Samples within block" = "n.j"))) # select input buttons div
                 #                ) # Choice of type of Sample we want to estimate 
                 #                
                 #              ), #fluidRow for radio button choice
                 #              
                 #              fluidRow(
                 #                column(6,
                 #                       numericInput("J_sample", "Number of blocks", min = 1, max = 100, value = 50, step = 1)
                 #                ), # Number of Blocks
                 #                
                 #                column(6,
                 #                       numericInput("nbar_sample","Number of units per block", min = 2, max = 100, value = 20, step = 1)     
                 #                       
                 #                ) # Fluid Row for Number of units per block and Number of units
                 #                
                 #              ), # Number of Block and Sample Size
                 #              
                 #              fluidRow(
                 #                column(6,
                 #                       numericInput("alpha_sample", "Alpha value", min = 0.001, max = 0.9, value = 0.05, step = 0.001)
                 #                ), # alpha value
                 #                
                 #                column(6,
                 #                       numericInput("me_sample", "Margin of error", min = 0.001, max = 0.9, value = 0.05, step = 0.001)
                 #                ) # Margins of error, #me = margin of error
                 #              ), # For Alpha and Margin of Error
                 #              
                 #              fluidRow(
                 #                
                 #                column(6,
                 #                       numericInput("numCovar.1_sample", "Level 1 Covariates", min = 0, max = 10, value = 1, step = 1 )
                 #                ), # number of covariates at level 1
                 #                
                 #                column(6,
                 #                       numericInput("R2.1_sample", "Level 1 R2", value = 0.2, min = 0, max = 1.0, step = 0.01)
                 #                ) # R2 (explanatory power at level 1)
                 #              ), # Number of Level 1 covariates and R^2 explanatory power
                 #              
                 #              fluidRow(
                 #                
                 #                column(6,
                 #                       numericInput("power_samples", "Power Value", min = 0.001, max = 1.0, value = 0.75, step = 0.001)
                 #                ), # Power value
                 #                
                 #                column(6,
                 #                       uiOutput("power_sample") #Dynamic Selector User Interface for power
                 #                ) # Choice of Power and Power Definition
                 #              ), # Fluid Row for Proportion of treatment assignment
                 #              
                 #              fluidRow(
                 #                
                 #                column(6,
                 #                       numericInput("p_sample", "Proportion of Treatment assignment", min = 0.001, max = 1.0, value = 0.5, step = 0.001)
                 #                ), # Proportion of treatment assignment
                 #                
                 #                column(6,
                 #                       numericInput("MDES_sample", "Minimum effect size", value = 0.125, min = 0, max = 5, step = 0.001)
                 #                ) # column for Minimum detectable effect size
                 #                
                 #              ), # fluid row for Proportion of Treatment assignment
                 #              
                 #              fluidRow(
                 #                
                 #                column(12,
                 #                       actionButton("goButton_sample", "Go!") # Action Button to trigger other reactive values
                 #                ) # Column for action button
                 #                
                 #              ) # fluid row for Action Button
                 #              
                 #            ), # Side Bar Panel
                 #            
                 #            mainPanel(
                 #              
                 #              fluidRow(
                 #                
                 #                column(12,
                 #                       tableOutput("sample") # Sample Table
                 #                ) # Full column
                 #                
                 #              )
                 #              
                 #            ) # Main Panel Layout
                 #          ) # Sidebar Panel       
                 # )  # Tabpanel Sample calculation 
    ) # Inner Tabset Panel
              
              # ), # 2 Level Block RCT Design Tab set
              # 
              # tabPanel("2-Level-Cluster", tabsetPanel(
              #   tabPanel("Cluster Power Calculation",
              #            sidebarLayout(
              #              sidebarPanel(
              #                # css to center the progress bar
              #                tags$head(
              #                  tags$style(
              #                    HTML(".shiny-notification {
              #                 height: 50px;
              #                 width: 600px;
              #                 position:fixed;
              #                 top: calc(50% - 50px);
              #                 left: calc(45%);
              #                 right: calc(15%);
              #                 font-size: 100%;
              #                 text-align: center;
              #                 
              #                 }
              #                 .progress-message {
              #                 
              #                 padding-top: 0px;
              #                 padding-right: 3px;
              #                 padding-bottom: 3px;
              #                 padding-left: 10px;
              #                 font-weight: normal !important;
              #                 font-style: italic !important;
              #                 font-size: 15px;
              #                 }
              #                 
              #                 .progress-detail {
              #                 
              #                 padding-top: 0px;
              #                 padding-right: 3px;
              #                 padding-bottom: 3px;
              #                 padding-left: 3px;
              #                 font-weight: normal;
              #                 font-style: italic !important;
              #                 font-size: 15px;
              #                 }
              #                 
              #                 "
              #                    ) # html bracket
              #                  ) # css styling tag
              #                ), # The header tag
              #                
              #                fluidRow(
              #                  column(10,
              #                         div(style = "display: inline-block, vertical-align:top;", selectInput("designCluster", "What Research Design is this for?", 
              #                                                                                               choices = list("simpleRandomEffects" = "simple_c2_2r")) # select input buttons div
              #                  ), # column for inputs
              #                  
              #                  column(2, 
              #                         div(style ="display: inline-block, vertical-align:top;",actionButton("question_designCluster",label = "", icon = icon("question"),
              #                                                                                              style = "font-size: 10px;
              #                                                                                               margin-top: 28px;")) #div for button ends
              #                  ) # column for buttons
              #                  
              #                ), # fluid Row to contain the question mark issue 
              #                
              #                bsPopover(id = "question_designCluster", title = NULL,
              #                          content = paste0("For more information on different designs, please click!"),
              #                          placement = "right", 
              #                          trigger = "hover", 
              #                          options = list(container = "body")), # the bsPopover for the more information section of the Shiny App
              #                
              #                fluidRow(
              #                  column(10,
              #                         div(style = "display: inline-block, vertical-align:top;", selectInput("MTPCluster", "Which MTP do you plan to use?", 
              #                                                                                               choices = list("Bonferroni" = "Bonferroni", 
              #                                                                                                              "Holm" = "Holm", 
              #                                                                                                              "Benjamini-Hochberg" = "BH", 
              #                                                                                                              "Westfall-Young-Single-Step" = "WY-SS", 
              #                                                                                                              "Westfall-Young-Step-Down" = "WY-SD"))) # select input buttons div
              #                  ), # column for inputs
              #                  
              #                  column(2, 
              #                         div(style ="display: inline-block, vertical-align:top;",actionButton("question_mtpCluster",label = "", icon = icon("question"))) #div for button ends
              #                  ) # column for buttons
              #                  
              #                ), # fluid Row to contain the question mark issue 
              #                
              #                bsPopover(id = "question_mtpCluster", title = NULL,
              #                          content = paste0("For more information on MTP, please click!"),
              #                          placement = "right", 
              #                          trigger = "hover", 
              #                          options = list(container = "body")), # the bsPopover for the more information section of the Shiny App
              #                
              #                fluidRow(
              #                  
              #                  column(12,
              #                         numericInput("MCluster", "Number of Outcomes", min = 1, max = 10, value = 5, step = 1)
              #                  ) # column for number of outcomes
              #                  
              #                ), # number of outcomes and mdes
              #                
              #                fluidRow(
              #                  
              #                  column(12,
              #                         textInput("MDESCluster", "Enter a vector (comma delimited)", value = "0.125,0.125,0.125, 0,0"))
              #                ), # column for Minimum detectable effect size
              #                
              #                
              #                fluidRow(
              #                  
              #                  column(12,
              #                         
              #                         numericInput("KCluster", "Number of Districts", min = 1, max = 100, value = 1, step = 1))
              #                ), # number of districts
              #                
              #                fluidRow(
              #                  
              #                  column(6,
              #                         
              #                         numericInput("JCluster", "Number of blocks", min = 1, max = 100, value = 50, step = 1)
              #                         
              #                  ), # number of blocks
              #                  
              #                  column(6,
              #                         
              #                         numericInput("nbarCluster","Number of units per block", min = 2, max = 100, value = 20, step = 1)     
              #                         
              #                  ) # number of units per blocks
              #                  
              #                ), # Nmber of blocks and number of units per block
              #                
              #                fluidRow(
              #                  
              #                  column(6,
              #                         
              #                         numericInput("R2.1Cluster", "Level 1 R2", value = 0.2, min = 0, max = 1.0, step = 0.01)
              #                         
              #                  ), # R square for level 1
              #                  
              #                  column(6,
              #                         
              #                         numericInput("numCovar.1Cluster", "Number of Level 1 Covariates", min = 0, max = 10, value = 1, step = 1 )
              #                         
              #                  )# Number of Level 1 Covariates
              #                  
              #                ), # column correlation btw tests & intraclass correlation!
              #                
              #                fluidRow(
              #                  
              #                  column(12,
              #                         
              #                         numericInput("tbarCluster", "Proportion of Treatment assignment", min = 0.001, max = 1.0, value = 0.5, step = 0.001)
              #                         
              #                  ) # proportion of treatment assignment
              #                ), # proprtion of treatement as assignment
              #                
              #                fluidRow(  
              #                  
              #                  column(12,
              #                         
              #                         numericInput("alphaCluster", "Significance Level of Tests (alpha)", min = 0.001, max = 0.9, value = 0.05, step = 0.001)
              #                         
              #                  ) #Significance Level of Tests
              #                  
              #                ), # proportion of treatment assignment and significance level of tests
              #                
              #                fluidRow(
              #                  column(12,
              #                         
              #                         numericInput("rhoCluster", "Correlation between outcomes", min = 0, max = 1, value = 0.5, step = 0.1 )
              #                         
              #                  ) # Number of Level 1 covariates
              #                  
              #                ), #fluid row for block level covariate inputs
              #                
              #                fluidRow(
              #                  
              #                  column(12,
              #                         actionButton("goButton_powerCluster", "Go!") # Action Button to trigger other reactive values
              #                  ) # Column for action button
              #                  
              #                )
              #                
              #              ), #sidebar Panel
              #              mainPanel (
              #                tableOutput("powercalcCluster") #The power calculation table output
              #              ) #main panel
              #              
              #            ) #sidebar Layout
              #            
              #   )  
              # ) # Tabset for Power for Cluster
            
            )# 2 Level Cluster RCT

  )      
 # Fluid Page
# set counter outside of the server call

counter <<- 0


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session = FALSE) {
  
  
  ############################################
  # Power Calculation Server Side Begins
  ############################################
  
  #Observing the action button click and switching to a different Tab. Passing the session to keep the info from previous tab.
  observeEvent(input$question_designP2LBI,{
    updateTabsetPanel(session, "mainMenu", selected = "Educational Resources" )
  }) # Action button that switch tabs
  
  # power <- reactive({
  #   
  #   power_blocked_i1_2c(M = input$M, MDES = input$MDES, numFalse = input$numFalse, J = input$J, n.j = input$n.j, R2.1 = input$R2.1, p = input$p, alpha = input$alpha, 
  #                       numCovar.1 = input$numCovar.1, numCovar.2 = NULL, ICC = NULL, tnum = 10000, snum = 10)
  #   
  # }) # reactive expression for power
  
  # observe Event for power calculation: Using observeEvent instead of eventReactive as we want to see the immediate side effect
  # observe Event for Single Scenario
  observeEvent(input$goButtonP2LBISS,{
    
      # set a Reactive Value for Power Table
      reactPowerTable <- reactiveVal()
      
      # Rendering a reactive object table from the power function
      output$powercalcTableP2LBISS <- renderTable({
        
        # Creating a progress bar
        progress <- shiny::Progress$new()
        progress$set(message = "Calculating Power", value = 0)
        # Close the progress bar when this reactive expression is done (even if there is an error)
        on.exit(progress$close())
        
        # Update Progress Bar Callback function
        updateProgress <- function(value = NULL, detail = NULL, message = NULL){
          
          if (is.null(value)){
            
            value <- progress$getValue()
            value <- value + (progress$getMax() - value)/5
            
          } # Progess bar in terms of values' increments
          
          progress$set(value = value, detail = detail, message = message)
          
        } # End of Callback Progress Function
        
        # data frame output for the results
        dat <- as.data.frame(
        isolate(pum::pump_power(design = input$designP2LBISS,
                           MTP = as.character(unlist(strsplit(input$MTPP2LBISS," "))),
                           MDES = as.numeric(unlist(strsplit(input$MDESP2LBISS, ","))),
                           M = input$MP2LBISS, # The number of hypotheses/outcomes
                           J = input$JP2LBISS, # The number of schools
                           K = input$KP2LBISS, # The number of districts
                           nbar = input$nbarP2LBISS, # The number of units per block
                           Tbar = input$tbarP2LBISS, # The proportion of samples that are assigned to the treatment
                           alpha = input$alphaP2LBISS,
                           numCovar.1 = input$numCovar.1P2LBISS,
                           numCovar.2 = 0,
                           numCovar.3 = 0,
                           R2.1 = rep(input$R2.1P2LBISS,
                                      input$MP2LBISS),
                           R2.2 = rep(0.1, input$MP2LBISS),
                           R2.3 = rep(0.1, input$MP2LBISS),
                           ICC.2 = rep(0, input$MP2LBISS),
                           ICC.3 = rep(0.2, input$MP2LBISS) ,
                           rho = input$rhoP2LBISS,
                           omega.2 = 0,
                           omega.3 = 0.1,
                           tnum = 10000, 
                           B = 100, 
                           cl = NULL,
                           updateProgress = updateProgress)
                      ))
        
        # Save the reactive Power Table
        reactPowerTable(dat)
        {reactPowerTable()}
            }, include.rownames = TRUE)# Wrapping a reactive expression to a reactive table object for output view
  
      # Rendering a reactive object table from the power function
      output$powercalcGraphP2LBISS <- renderPlot({
        
        dat <- reactPowerTable()
        dat %>%
          dplyr::select_all() %>%
          dplyr::select(-indiv.mean) %>%
          tidyr::pivot_longer(!MTP, names_to = "powerType", values_to = "power") %>%
          ggplot(aes(x = powerType, 
                     y = power, 
                     shape = MTP,
                     colour = MTP)) + 
          geom_point(size = 5) +
          scale_y_continuous(limits = c(0,1)) +
          ggtitle("Adjusted Power values across different Power Definitions") +
          theme(plot.title = element_text(size = 16,
                                          face = "bold",
                                          vjust = 1,
                                          hjust = 0.5),
                axis.text = element_text(size = 14),
                axis.title = element_text(size = 14))
        
    }) # ggplot for power graph
  
}) # observe Event go Button for power for Single Scenario
  
  # observe Event for Explorer
  observeEvent(input$goButtonP2LBIEMDES,{
    
        # set a Reactive Value for Power Table
    reactPowerTable <- reactiveVal()
    
    # Rendering a reactive object table from the power function
    output$powercalcTableP2LBIEMDES <- renderTable({
      
      # Creating a progress bar
      progress <- shiny::Progress$new()
      progress$set(message = "Calculating Power", value = 0)
      # Close the progress bar when this reactive expression is done (even if there is an error)
      on.exit(progress$close())
      
      # Update Progress Bar Callback function
      updateProgress <- function(value = NULL, detail = NULL, message = NULL){
        
        if (is.null(value)){
          
          value <- progress$getValue()
          value <- value + (progress$getMax() - value)/5
          
        } # Progess bar in terms of values' increments
        
        progress$set(value = value, detail = detail, message = message)
        
      } # End of Callback Progress Function
      
      #browser()
      # data frame output for the results
      dat <- as.data.frame(
        isolate(pum::pump_power_grid(design = input$designP2LBIEMDES,
                           MTP = as.character(unlist(strsplit(input$MTPP2LBIEMDES," "))),
                           MDES = as.numeric(unlist(strsplit(input$MDESP2LBIEMDES, ","))),
                           M = input$MP2LBIEMDES, # The number of hypotheses/outcomes
                           J = input$JP2LBIEMDES, # The number of schools
                           K = input$KP2LBIEMDES, # The number of districts
                           nbar = input$nbarP2LBIEMDES, # The number of units per block
                           Tbar = input$tbarP2LBIEMDES, # The proportion of samples that are assigned to the treatment
                           alpha = input$alphaP2LBIEMDES,
                           numCovar.1 = input$numCovar.1P2LBIEMDES,
                           numCovar.2 = 0,
                           numCovar.3 = 0,
                           R2.1 = rep(input$R2.1P2LBIEMDES,
                                      input$MP2LBIEMDES),
                           R2.2 = NULL,
                           R2.3 = NULL,
                           ICC.2 = 0,
                           ICC.3 = NULL,
                           rho = input$rhoP2LBIEMDES,
                           omega.2 = NULL,
                           omega.3 = NULL,
                           updateProgress = updateProgress)
        ))
      
      # Save the reactive Power Table
      reactPowerTable(dat)
      {reactPowerTable()}
    })# Wrapping a reactive expression to a reactive table object for output view
    
    # Rendering a reactive object table from the power function
    output$powercalcGraphP2LBIEMDES <- renderPlot({
      
      # Grab the number of outcomes
      M <- input$MP2LBIEMDES
      
      # End of Minimum Power 
      minEnd <- M - 1
      
      # Create color gradient for minimum power
      mincolours <- scales::seq_gradient_pal(low = "gray80", high = "gray30", space = "Lab")(1:minEnd/minEnd)
      mincolours <- sort(mincolours)
      
      # Add complete, individual color on top of the minimum colors
      allcolors <- c("#90ee90", "#ADD8E6", mincolours)
      
      dat <- reactPowerTable()
      dat <- as.data.frame(dat)
      withoutIndivPower <- 
        dat %>%
          dplyr::select_all() %>%
          dplyr::select(-design, -adjustment) %>%
          tidyr::pivot_longer(!c(MDES,MTP), names_to = "powerType", values_to = "power") %>%
          dplyr::filter(!stringr::str_detect(powerType,"D")) 
  
      # converting Power Type to a factor
      withoutIndivPower$powerType <- factor(withoutIndivPower$powerType, ordered = TRUE)
      
      #Pulling out Power Type Levels to match with all colors
      powerTypeLevels <- levels(withoutIndivPower$powerType)
      
      # create value for scale color manual by matching color and Power Type
      allcolorsvalues <- setNames(allcolors, powerTypeLevels)
      
      withoutIndivPower %>%
        ggplot(aes(x = as.factor(MDES),
                   y = power,
                   colour = powerType)) +
        geom_point(size = 5) +
        scale_y_continuous(limits = c(0,1)) +
        ggtitle("Adjusted Power values across different Power Definitions & MDES values") +
        theme(plot.title = element_text(size = 16,
                                        face = "bold",
                                        vjust = 1,
                                        hjust = 0.5),
              axis.text = element_text(size = 14),
              axis.title = element_text(size = 14)) +
        scale_colour_manual(values = allcolorsvalues) +
        labs(x = "Different MDES Scenarios")

    }) # ggplot for power graph

  }) # Observe Event for Explorer MDES
  
  # observe Event for Explorer R2
  observeEvent(input$goButtonP2LBIER2,{
    
    # set a Reactive Value for Power Table
    reactPowerTable <- reactiveVal()
    
    # Rendering a reactive object table from the power function
    output$powercalcTableP2LBIER2 <- renderTable({
      
      # Creating a progress bar
      progress <- shiny::Progress$new()
      progress$set(message = "Calculating Power", value = 0)
      # Close the progress bar when this reactive expression is done (even if there is an error)
      on.exit(progress$close())
      
      # Update Progress Bar Callback function
      updateProgress <- function(value = NULL, detail = NULL, message = NULL){
        
        if (is.null(value)){
          
          value <- progress$getValue()
          value <- value + (progress$getMax() - value)/5
          
        } # Progess bar in terms of values' increments
        
        progress$set(value = value, detail = detail, message = message)
        
      } # End of Callback Progress Function
      
      # data frame output for the results
      dat <- as.data.frame(
        isolate(pum::pump_power_grid(design = input$designP2LBIER2,
                                     MTP = as.character(unlist(strsplit(input$MTPP2LBIER2," "))),
                                     MDES = as.numeric(unlist(strsplit(input$MDESP2LBIER2, ","))),
                                     M = input$MP2LBIER2, # The number of hypotheses/outcomes
                                     J = input$JP2LBIER2, # The number of schools
                                     K = input$KP2LBIER2, # The number of districts
                                     nbar = input$nbarP2LBIER2, # The number of units per block
                                     Tbar = input$tbarP2LBIER2, # The proportion of samples that are assigned to the treatment
                                     alpha = input$alphaP2LBIER2,
                                     numCovar.1 = input$numCovar.1P2LBIER2,
                                     numCovar.2 = 0,
                                     numCovar.3 = 0,
                                     R2.1 = rep(input$R2.1P2LBIER2,
                                                input$MP2LBIER2),
                                     R2.2 = NULL,
                                     R2.3 = NULL,
                                     ICC.2 = 0,
                                     ICC.3 = NULL,
                                     rho = input$rhoP2LBIER2,
                                     omega.2 = NULL,
                                     omega.3 = NULL,
                                     updateProgress = updateProgress)
        ))
      
      # Save the reactive Power Table
      reactPowerTable(dat)
      {reactPowerTable()}
    }, include.rownames = TRUE)# Wrapping a reactive expression to a reactive table object for output view
    
    # Rendering a reactive object table from the power function
    output$powercalcGraphP2LBIER2 <- renderPlot({
      
      dat <- reactPowerTable()
      dat %>%
        dplyr::select_all() %>%
        dplyr::select(-indiv.mean, -adjustment, -design) %>%
        tidyr::pivot_longer(!MDES, names_to = "powerType", values_to = "power") %>%
        ggplot(aes(x = as.factor(MDES),
                   y = power,
                   shape = powerType,
                   colour = powerType)) +
        geom_point(size = 5) +
        scale_y_continuous(limits = c(0,1)) +
        ggtitle("Adjusted Power values across different Power Definitions & MDES values") +
        theme(plot.title = element_text(size = 16,
                                        face = "bold",
                                        vjust = 1,
                                        hjust = 0.5),
              axis.text = element_text(size = 14),
              axis.title = element_text(size = 14))
      
    }) # ggplot for power graph
    
  }) # Observe Event for Explorer R2
  

  
  
  
    
  
  
  
  
  
  
  ############################################
  # MDES Server Side Calculation Begins
  ############################################
  
  #power selection, reactive choices. This is different from the mdes reactive values which are being isolated out.
  
  getPower <- reactive({
    
    if (input$M_mdes == "1"){
      return(c( "Individual" = "indiv",
                "Complete" = "complete"
      ))
    } # end of if statement
    else if (input$M_mdes == "2"){
      return(c( "Individual" = "indiv",
                "Complete" = "complete",
                "1-minimal" = "min1"
      ))
    }# first else if
    else if (input$M_mdes == "3"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2"
      ))
    }# second else if
    else if (input$M_mdes == "4"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3"
      ))
    }# third else if
    else if (input$M_mdes == "5"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3",
               "4-minimal" = "min4"
      ))
    }# fourth else if
    else if (input$M_mdes == "6"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3",
               "4-minimal" = "min4",
               "5-minimal" = "min5"
      ))
    }# fifth else if
    else if (input$M_mdes == "7"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3",
               "4-minimal" = "min4",
               "5-minimal" = "min5",
               "6-minimal" = "min6" 
      ))
    }# sixth else if
    else if (input$M_mdes == "8"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3",
               "4-minimal" = "min4",
               "5-minimal" = "min5",
               "6-minimal" = "min6",
               "7-minmal"  = "min7"
      ))
    }# seventh else if
    else if (input$M_mdes == "9"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3",
               "4-minimal" = "min4",
               "5-minimal" = "min5",
               "6-minimal" = "min6",
               "7-minmal"  = "min7",
               "8-minmal"  = "min8"
      ))
    }# eight else if
    else if (input$M_mdes == "10"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3",
               "4-minimal" = "min4",
               "5-minimal" = "min5",
               "6-minimal" = "min6",
               "7-minmal"  = "min7",
               "8-minmal"  = "min8",
               "9-minmal" = "min9"
      ))
    }# tenth else if
  }) # end of Power Reactive reactive expression
  
  #RenderUI for the selector input for different power definitions
  output$power <- renderUI({
    
    powerlist = as.list(getPower())
    # browser()
    div(style = "display: inline-block, vertical-align:top;", 
        selectInput("pdefn_mdes", "Power Defn", 
                    choices = powerlist,
                    selected = powerlist[[1]]))
    
  }) #power select input options
  
  #mdes calculation reactive expression
  mdes <- reactive({
    
    mdes_blocked_i1_2c(M = input$M_mdes, numFalse = input$numFalse_mdes, J = input$J_mdes, n.j = input$n.j_mdes, power=input$power_mdes, power.definition = input$pdefn_mdes, MTP=input$MTP_mdes, marginError = input$me_mdes, p = input$p_mdes, alpha = input$alpha_mdes, numCovar.1=input$numCovar.1_mdes, numCovar.2=NULL, R2.1=input$R2.1_mdes, R2.2=0, ICC=0,
                       mod.type="constant", omega=NULL,
                       tnum = 10000, snum=2, ncl=2, updateProgress)
    
  }) # reactive expression for mdes, Note: Need to manage the sigma. Current: Decomissioned temporarily
  
  
  #observe Event for mdes calculation: Using observeEvent instead of eventReactive as we want to see the immediate side effect
  observeEvent(input$goButton_mdes,{
    output$mdes <- renderTable({
      
      #Creating a progress bar
      progress <- shiny::Progress$new()
      progress$set(message = "Calculating MDES", value = 0)
      # Close the progress bar when this reactive expression is done (even if there is an error)
      on.exit(progress$close())
      
      #Update Progress Bar Callback function
      updateProgress <- function(value = NULL, detail = NULL, message = NULL){
        
        if (is.null(value)){
          
          value <- progress$getValue()
          value <- value + (progress$getMax() - value)/20
          
        } # Progess bar in terms of values' increments
        
        progress$set(value = value, detail = detail, message = message)
        
      } # End of Callback Progress Function
      
      #The MDES calculation function
      isolate(mdes_blocked_i1_2c(M = input$M_mdes, numFalse = input$numFalse_mdes, J = input$J_mdes, n.j = input$n.j_mdes, power=input$power_mdes, power.definition = input$pdefn_mdes, MTP=input$MTP_mdes, marginError = input$me_mdes, p = input$p_mdes, alpha = input$alpha_mdes, numCovar.1=input$numCovar.1_mdes, numCovar.2=NULL, R2.1=input$R2.1_mdes, R2.2=0, ICC=0,
                                 mod.type="constant", omega=NULL,
                                 tnum = 10000, snum=2, ncl=2, updateProgress = updateProgress)) #data table that is isolated
      
    }) # end of isolate. We do not want 
  }) # ObserveEvent for the Go Button ends here
  
  ############################################
  # Sample  Server Side Calculation Begins
  ############################################
  
  #power definition selection, reactive choices. This is different from the mdes reactive values which are being isolated out.
  
  getPower_sample <- reactive({
    
    if (input$M_sample == "1"){
      return(c( "Individual" = "indiv",
                "Complete" = "complete"
      ))
    } # end of if statement
    else if (input$M_sample == "2"){
      return(c( "Individual" = "indiv",
                "Complete" = "complete",
                "1-minimal" = "min1"
      ))
    }# first else if
    else if (input$M_sample == "3"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2"
      ))
    }# second else if
    else if (input$M_sample == "4"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3"
      ))
    }# third else if
    else if (input$M_sample == "5"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3",
               "4-minimal" = "min4"
      ))
    }# fourth else if
    else if (input$M_sample == "6"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3",
               "4-minimal" = "min4",
               "5-minimal" = "min5"
      ))
    }# fifth else if
    else if (input$M_sample == "7"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3",
               "4-minimal" = "min4",
               "5-minimal" = "min5",
               "6-minimal" = "min6" 
      ))
    }# sixth else if
    else if (input$M_sample == "8"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3",
               "4-minimal" = "min4",
               "5-minimal" = "min5",
               "6-minimal" = "min6",
               "7-minmal"  = "min7"
      ))
    }# seventh else if
    else if (input$M_sample == "9"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3",
               "4-minimal" = "min4",
               "5-minimal" = "min5",
               "6-minimal" = "min6",
               "7-minmal"  = "min7",
               "8-minmal"  = "min8"
      ))
    }# eight else if
    else if (input$M_sample == "10"){
      return(c("Individual" = "indiv",
               "Complete" = "complete",
               "1-minimal" = "min1",
               "2-minimal" = "min2",
               "3-minimal" = "min3",
               "4-minimal" = "min4",
               "5-minimal" = "min5",
               "6-minimal" = "min6",
               "7-minmal"  = "min7",
               "8-minmal"  = "min8",
               "9-minmal" = "min9"
      ))
    }# tenth else if
  }) # end of Power Reactive reactive expression
  
  #RenderUI for the selector input for different power definitions
  output$power_sample <- renderUI({
    
    powerlist_sample = as.list(getPower_sample())
    # browser()
    div(style = "display: inline-block, vertical-align:top;", 
        selectInput("pdefn_sample", "Power Defn", 
                    choices = powerlist_sample,
                    selected = powerlist_sample[[1]]))
    
  }) #power select input options
  
  #observe Event for mdes calculation: Using observeEvent instead of eventReactive as we want to see the immediate side effect
  observeEvent(input$goButton_sample,{
    output$sample <- renderTable({
      
      #Creating a progress bar
      progress <- shiny::Progress$new()
      progress$set(message = "Calculating Sample Size", value = 0)
      # Close the progress bar when this reactive expression is done (even if there is an error)
      on.exit(progress$close())
      
      #Update Progress Bar Callback function
      updateProgress <- function(value = NULL, detail = NULL, message = NULL){
        
        if (is.null(value)){
          
          value <- progress$getValue()
          value <- value + (progress$getMax() - value)/40
          
        } # Progess bar in terms of values' increments
        
        progress$set(value = value, detail = detail, message = message)
        
      } # End of Callback Progress Function
      
      #The Sample calculation function
      isolate(sample_blocked_i1_2c(M = input$M_sample, numFalse = input$numFalse_sample, typesample = input$typesample, J = input$J_sample, 
                                   n.j = input$n.j_sample,J0 = 10, n.j0 = 10,MDES = input$MDES_sample, power=input$power_samples, 
                                   power.definition = input$pdefn_sample, MTP=input$MTP_sample, 
                                   marginError = input$me_sample, p = input$p_sample, alpha = input$alpha_sample, 
                                   numCovar.1 = input$numCovar.1_sample, numCovar.2 = NULL, R2.1 = input$R2.1_sample, R2.2 = 0, ICC = 0,
                                   mod.type = "constant", omega = NULL,
                                   tnum = 10000, snum = 2000, ncl = 8, updateProgress = updateProgress)) #data table that is isolated
      
    }) # end of isolate. We do not want 
  }) # Sample calculation
  
}) #Server side actions

# Run the application 
shinyApp(ui = ui, server = server)

# sample_BH_indiv_nj_50 <- sample_blocked_i1_2c(M = 3, numFalse = 3, MTP = "BH", typesample = "n.j", J = 20, n.j = NULL, 
#                                              J0 = NULL, n.j0 = 50, MDES = 0.125, power = power_M3_BH_MDES0125["BH", "indiv"] ,
#                                              power.definition = "indiv", marginError = 0.05,
#                                              p = 0.5,alpha = 0.05,numCovar.1 = 5,numCovar.2 = 1,
#                                              R2.1 = 0.5,tnum = 10000, snum = 2000, ncl = 8)
# print(sample_BH_indiv_nj_50)







