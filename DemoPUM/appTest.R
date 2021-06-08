library(shiny) # for basic templates
library(shinyBS) # for popovers and tool tips
library(shinycssloaders) # for ui elements showing shiny loading
library(magrittr) # piping operator
library(pum) # our pum lirbary

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Nav Bar to construct the set of options we have along with the name of the title
  
  titlePanel(title = "Power Under Multiplicity", windowTitle = "Power Under Multiplicity"), 
             tabsetPanel( id = "mainMenu",
               tabPanel("Home"),
               tabPanel("Educational Resources"),
               tabPanel("2_Level_Blocked_i1_2cfr", tabsetPanel( #2LBI for 2 Level Blocked I1
                 tabPanel("Power Calculation",
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
                                           selectInput("designP2LBI", "What Research Design is this for?", 
                                           choices = list("constantEffects" = "blocked_i1_2c", 
                                                          "fixedEffects" = "blocked_i1_2f", 
                                                          "randomEffects" = "blocked_i1_2r"))) # select input buttons div
                                ), # column for inputs
                                
                                column(2, 
                                       div(style ="display: inline-block,vertical-align:top;",
                                           actionButton("question_designP2LBI",
                                           label = "", 
                                           icon = icon("question"),
                                           style = "font-size: 10px;
                                                    margin-top: 28px;")) #div for button ends
                                ) # column for buttons
                                
                              ), # fluid Row to contain the question mark issue 
                              
                              bsPopover(id = "question_designP2LBI", 
                                        title = NULL,
                                        content = paste0("For more information on different designs, please click!"),
                                        placement = "right", 
                                        trigger = "hover", 
                                        options = list(container = "body")), # the bsPopover for the more information section of the Shiny App
                              
                              fluidRow(
                                column(10,
                                       div(style = "display: inline-block, vertical-align:top;", 
                                           selectInput("MTPP2LBI", "Which MTP do you plan to use?", 
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
                                           actionButton("question_mtpP2LBI",
                                                        label = "", 
                                                        icon = icon("question"),
                                                        style = "font-size: 10px;
                                                                 margin-top: 28px;")) #div for button ends
                                ) # column for buttons
                                
                              ), # fluid Row to contain the question mark issue 
                              
                              bsPopover(id = "question_mtpP2LBI", 
                                        title = NULL,
                                        content = paste0("For more information on MTP, please click!"),
                                        placement = "right", 
                                        trigger = "hover", 
                                        options = list(container = "body")), # the bsPopover for the more information section of the Shiny App
                              
                              fluidRow(
                                
                                column(12,
                                       numericInput("MP2LBI", 
                                                    "Number of Outcomes", 
                                                    min = 1, 
                                                    max = 10, 
                                                    value = 5, 
                                                    step = 1)
                                ) # column for number of outcomes
                              
                              ), # number of outcomes and mdes
                              
                              fluidRow(
                                
                                column(10,
                                       textInput("MDESP2LBI", "Enter MDES vector (comma delimited)", value = "0.125,0.125,0.125, 0,0")
                                       
                                ), # column for MDES
                                
                                column(2, 
                                       div(style ="display: inline-block, 
                                           vertical-align:top;",
                                           actionButton("question_mdesP2LBI",
                                                        label = "", 
                                                        icon = icon("question"),
                                                        style = "font-size: 10px;
                                                                 margin-top: 28px;")) #div for button ends
                                ) # column for buttons
                                
                              ), # fluid Row to contain the question mark issue 
                              
                              bsPopover(id = "question_mdesP2LBI", 
                                        title = NULL,
                                        content = paste0("For more information on MTP, please click!"),
                                        placement = "right", 
                                        trigger = "hover", 
                                        options = list(container = "body")
                                        ), # the bsPopover for the more information section of the Shiny App
                              
                              fluidRow(
                                
                                column(12,
                                       
                                       numericInput("KP2LBI", "Number of Districts", min = 1, max = 100, value = 1, step = 1))
                              ), # number of districts
                              
                              fluidRow(
                                
                                column(6,
                                       
                                       numericInput("JP2LBI", "Number of blocks", min = 1, max = 100, value = 50, step = 1)
                                       
                                ), # number of blocks
                                
                                column(6,
                                       
                                       numericInput("nbarP2LBI","Number of units per block", min = 2, max = 100, value = 20, step = 1)     
                                       
                                ) # number of units per blocks
                                
                              ), # Nmber of blocks and number of units per block
                              
                              fluidRow(
                                
                                column(6,
                                       
                                       numericInput("R2.1P2LBI", "Level 1 R2", value = 0.2, min = 0, max = 1.0, step = 0.01)
                                       
                                ), # R square for level 1
                                
                                column(6,
                                       
                                       numericInput("numCovar.1P2LBI", "Number of Level 1 Covariates", min = 0, max = 10, value = 1, step = 1 )
                                       
                                )# Number of Level 1 Covariates
                                
                              ), # column correlation btw tests & intraclass correlation!
                              
                              fluidRow(
                                
                                column(12,
                                       
                                       numericInput("tbarP2LBI", "Proportion of Treatment assignment", min = 0.001, max = 1.0, value = 0.5, step = 0.001)
                                       
                                ) # proportion of treatment assignment
                              ), # proprtion of treatement as assignment
                              
                              fluidRow(  
                                
                                column(12,
                                       
                                       numericInput("alphaP2LBI", "Significance Level of Tests (alpha)", min = 0.001, max = 0.9, value = 0.05, step = 0.001)
                                       
                                ) #Significance Level of Tests
                                
                              ), # proportion of treatment assignment and significance level of tests
                              
                              fluidRow(
                                column(12,
                                       
                                       numericInput("rhoP2LBI", "Correlation between outcomes", min = 0, max = 1, value = 0.5, step = 0.1 )
                                       
                                ) # Number of Level 1 covariates
                                
                              ), #fluid row for block level covariate inputs
                              
                              fluidRow(
                                
                                column(12,
                                       actionButton("goButtonP2LBI", "Go!") # Action Button to trigger other reactive values
                                ) # Column for action button
                                
                              )
                              
                            ), #sidebar Panel
                            mainPanel (
                              tableOutput("powercalcP2LBI") #The power calculation table output
                            ) #main panel
                            
                          ) #sidebar Layout
                          
                 ), # Calculation Tabset Panel
                 
                 tabPanel("MDES Calculation",
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
                                column(6,
                                       div(style = "display: inline-block, vertical-align:top;", 
                                           selectInput("MTP_mdes", "MTP", 
                                                       choices = list("Bonferroni" = "BF", 
                                                                      "Benjamini-Hocheberg" = "BH", 
                                                                      "Holms" = "HO", 
                                                                      "Westfall-Young-SS" = "WY-SS", 
                                                                      "Westfall-Young-SD" = "WY-SD"),
                                                       selected = "BF")) # select input buttons div
                                       
                                ), # MTP, the Mutliple Testing Procedure in Use
                                
                                column(6,
                                       numericInput("M_mdes", "Number of Outcomes", min = 1, max = 10, value = 5, step = 1)
                                ) # M, the number of outcomes in use
                              ), # Fluid Row for MTP and M outcomes
                              
                              fluidRow(
                                
                                column(10,
                                       numericInput("numFalse_mdes", "Number of Outcomes with an expected non-zero effects", value = 3, min = 0, max = 10, step = 1)
                                ), # column for outcomes with actual effect size
                                
                                column(2,
                                       div(style ="display: inline-block, vertical-align:top;",actionButton("question_mdes_mdes",label = "", icon = icon("question"))) #div for button ends                            
                                ) # column for action button
                                
                              ), # MDES and number of outcomes with expected actual effect
                              
                              fluidRow(
                                column(6,
                                       numericInput("J_mdes", "Number of blocks", min = 1, max = 100, value = 50, step = 1)
                                ), # Number of Blocks
                                
                                column(6,
                                       numericInput("n.j_mdes","Number of units per block", min = 2, max = 100, value = 20, step = 1)     
                                       
                                ) # Fluid Row for Number of units per block and Number of units
                                
                              ), # Number of Block and Sample Size
                              
                              fluidRow(
                                column(6,
                                       numericInput("alpha_mdes", "Alpha value", min = 0.001, max = 0.9, value = 0.05, step = 0.001)
                                ), # alpha value
                                
                                column(6,
                                       numericInput("me_mdes", "Margin of error", min = 0.001, max = 0.9, value = 0.05, step = 0.001)
                                ) # Margins of error
                              ), # For Alpha and Margin of Error
                              
                              fluidRow(
                                
                                column(6,
                                       numericInput("numCovar.1_mdes", "Level 1 Covariates", min = 0, max = 10, value = 1, step = 1 )
                                ), # number of covariates at level 1
                                
                                column(6,
                                       numericInput("R2.1_mdes", "Level 1 R2", value = 0.2, min = 0, max = 1.0, step = 0.01)
                                ) # R2 (explanatory power at level 1)
                              ), # Number of Level 1 covariates and R^2 explanatory power
                              
                              fluidRow(
                                
                                column(6,
                                       numericInput("power_mdes", "Power Value", min = 0.001, max = 1.0, value = 0.75, step = 0.001)
                                ), # Power value
                                
                                column(6,
                                       uiOutput("power") #Dynamic Selector User Interface for power
                                ) # Choice of Power and Power Definition
                              ), # Fluid Row for Proportion of treatment assignment
                              
                              fluidRow(
                                
                                column(12,
                                       numericInput("p_mdes", "Proportion of Treatment assignment", min = 0.001, max = 1.0, value = 0.5, step = 0.001)
                                ) # Proportion of treatment assignment
                              ), # fluid row for Proportion of Treatment assignment
                              
                              fluidRow(
                                
                                column(12,
                                       actionButton("goButton_mdes", "Go!") # Action Button to trigger other reactive values
                                ) # Column for action button
                                
                              ) # fluid row for Action Button
                              
                            ), # Side Bar Panel
                            
                            mainPanel(
                              
                              fluidRow(
                                
                                column(12,
                                       tableOutput("mdes") # MDES part of a spinner
                                ) # Full column
                                
                              ) #fluidRow for first half of the page
                            ) # Main Panel Layout
                          ) # Sidebar Panel       
                 ), # Tabpanel MDES calculation
                 tabPanel("Sample Size Calculation",
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
                                column(6,
                                       div(style = "display: inline-block, vertical-align:top;", 
                                           selectInput("MTP_sample", "MTP", 
                                                       choices = list("Bonferroni" = "BF", 
                                                                      "Benjamini-Hocheberg" = "BH", 
                                                                      "Holms" = "HO", 
                                                                      "Westfall-Young-SS" = "WY-SS", 
                                                                      "Westfall-Young-SD" = "WY-SD"),
                                                       selected = "BF")) # select input buttons div
                                       
                                ), # MTP, the Mutliple Testing Procedure in Use
                                
                                column(6,
                                       numericInput("M_sample", "Number of Outcomes", min = 1, max = 10, value = 5, step = 1)
                                ) # M, the number of outcomes in use
                              ), # Fluid Row for MTP and M outcomes
                              
                              fluidRow(
                                
                                column(10,
                                       numericInput("numFalse_sample", "Number of False Nulls", value = 3, min = 0, max = 10, step = 1)
                                ), # column for outcomes with actual effect size
                                
                                column(2,
                                       div(style ="display: inline-block, vertical-align:top;",actionButton("question_sample_sample",label = "", icon = icon("question"))) #div for button ends                            
                                ) # column for action button
                                
                              ), # MDES and number of outcomes with expected actual effect
                              
                              fluidRow(
                                
                                column(12,
                                       div(style = "display: inline-block, vertical-align:top;", 
                                           selectInput("typesample", "Which type of sample would you like to estimate ?", 
                                                       choices = list("Number of Blocks" = "J", "Number of Samples within block" = "n.j"))) # select input buttons div
                                ) # Choice of type of Sample we want to estimate 
                                
                              ), #fluidRow for radio button choice
                              
                              fluidRow(
                                column(6,
                                       numericInput("J_sample", "Number of blocks", min = 1, max = 100, value = 50, step = 1)
                                ), # Number of Blocks
                                
                                column(6,
                                       numericInput("nbar_sample","Number of units per block", min = 2, max = 100, value = 20, step = 1)     
                                       
                                ) # Fluid Row for Number of units per block and Number of units
                                
                              ), # Number of Block and Sample Size
                              
                              fluidRow(
                                column(6,
                                       numericInput("alpha_sample", "Alpha value", min = 0.001, max = 0.9, value = 0.05, step = 0.001)
                                ), # alpha value
                                
                                column(6,
                                       numericInput("me_sample", "Margin of error", min = 0.001, max = 0.9, value = 0.05, step = 0.001)
                                ) # Margins of error, #me = margin of error
                              ), # For Alpha and Margin of Error
                              
                              fluidRow(
                                
                                column(6,
                                       numericInput("numCovar.1_sample", "Level 1 Covariates", min = 0, max = 10, value = 1, step = 1 )
                                ), # number of covariates at level 1
                                
                                column(6,
                                       numericInput("R2.1_sample", "Level 1 R2", value = 0.2, min = 0, max = 1.0, step = 0.01)
                                ) # R2 (explanatory power at level 1)
                              ), # Number of Level 1 covariates and R^2 explanatory power
                              
                              fluidRow(
                                
                                column(6,
                                       numericInput("power_samples", "Power Value", min = 0.001, max = 1.0, value = 0.75, step = 0.001)
                                ), # Power value
                                
                                column(6,
                                       uiOutput("power_sample") #Dynamic Selector User Interface for power
                                ) # Choice of Power and Power Definition
                              ), # Fluid Row for Proportion of treatment assignment
                              
                              fluidRow(
                                
                                column(6,
                                       numericInput("p_sample", "Proportion of Treatment assignment", min = 0.001, max = 1.0, value = 0.5, step = 0.001)
                                ), # Proportion of treatment assignment
                                
                                column(6,
                                       numericInput("MDES_sample", "Minimum effect size", value = 0.125, min = 0, max = 5, step = 0.001)
                                ) # column for Minimum detectable effect size
                                
                              ), # fluid row for Proportion of Treatment assignment
                              
                              fluidRow(
                                
                                column(12,
                                       actionButton("goButton_sample", "Go!") # Action Button to trigger other reactive values
                                ) # Column for action button
                                
                              ) # fluid row for Action Button
                              
                            ), # Side Bar Panel
                            
                            mainPanel(
                              
                              fluidRow(
                                
                                column(12,
                                       tableOutput("sample") # Sample Table
                                ) # Full column
                                
                              ) #fluidRow for first half of the page
                              
                            ) # Main Panel Layout
                          ) # Sidebar Panel       
                 )  # Tabpanel Sample calculation 
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
              
          ) # Tabset Panel
  
)# fluid page

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
  
  #observe Event for power calculation: Using observeEvent instead of eventReactive as we want to see the immediate side effect
  observeEvent(input$goButtonP2LBI,{
    
    #Rendering a reactive object table from the power function
    output$powercalcP2LBI <- renderTable({
      
      #Creating a progress bar
      progress <- shiny::Progress$new()
      progress$set(message = "Calculating MDES", value = 0)
      # Close the progress bar when this reactive expression is done (even if there is an error)
      on.exit(progress$close())
      
      #Update Progress Bar Callback function
      updateProgress <- function(value = NULL, detail = NULL, message = NULL){
        
        if (is.null(value)){
          
          value <- progress$getValue()
          value <- value + (progress$getMax() - value)/5
          
        } # Progess bar in terms of values' increments
        
        progress$set(value = value, detail = detail, message = message)
        
      } # End of Callback Progress Function
      
      #displaying based on the number of sample sizes
      #pump_power <- function(
      #  design, MTP, MDES, M, J, K = 1, nbar, Tbar, alpha, numCovar.1 = 0, numCovar.2 = 0,
      #  numCovar.3 = 0, R2.1, R2.2 = NULL, R2.3 = NULL, ICC.2, ICC.3 = NULL,
      #  rho, omega.2, omega.3 = NULL,
      #  tnum = 10000, B = 1000, cl = NULL, updateProgress = NULL
      #)
     #browser()
      
      isolate(pump_power(design = input$designP2LBI,
                         MTP = as.character(unlist(strsplit(input$MTPP2LBI," "))),
                         MDES = as.numeric(unlist(strsplit(input$MDESP2LBI, ","))),
                         M = input$MP2LBI, # The number of hypotheses/outcomes
                         J = input$JP2LBI, # The number of schools
                         K = input$KP2LBI, # The number of districts
                         nbar = input$nbarP2LBI, # The number of units per block
                         Tbar = input$tbarP2LBI, # The proportion of samples that are assigned to the treatment
                         alpha = input$alphaP2LBI,
                         numCovar.1 = input$numCovar.1P2LBI,
                         numCovar.2 = 0,
                         numCovar.3 = 0,
                         R2.1 = rep(input$R2.1P2LBI,input$MP2LBI),
                         R2.2 = rep(0.1, input$MP2LBI),
                         R2.3 = rep(0.1, input$MP2LBI),
                         ICC.2 = rep(0, input$MP2LBI),
                         ICC.3 = rep(0.2, input$MP2LBI) ,
                         rho = input$rhoP2LBI,
                         omega.2 = 0,
                         omega.3 = 0.1,
                         tnum = 10000, 
                         B = 100, 
                         cl = NULL,
                         updateProgress = updateProgress)
                    )
    }, include.rownames = TRUE)# Wrapping a reactive expression to a reactive table object for output view
    
  }) # observe Event go Button for power
  
  #observe Event for power calculation: Using observeEvent instead of eventReactive as we want to see the immediate side effect
  observeEvent(input$goButton_powerCluster,{
    
    #Rendering a reactive object table from the power function
    output$powercalcCluster <- renderTable({
      
      #Creating a progress bar
      progress <- shiny::Progress$new()
      progress$set(message = "Calculating MDES", value = 0)
      # Close the progress bar when this reactive expression is done (even if there is an error)
      on.exit(progress$close())
      
      #Update Progress Bar Callback function
      updateProgress <- function(value = NULL, detail = NULL, message = NULL){
        
        if (is.null(value)){
          
          value <- progress$getValue()
          value <- value + (progress$getMax() - value)/5
          
        } # Progess bar in terms of values' increments
        
        progress$set(value = value, detail = detail, message = message)
        
      } # End of Callback Progress Function
      
      #displaying based on the number of sample sizes
      #pump_power <- function(
      #  design, MTP, MDES, M, J, K = 1, nbar, Tbar, alpha, numCovar.1 = 0, numCovar.2 = 0,
      #  numCovar.3 = 0, R2.1, R2.2 = NULL, R2.3 = NULL, ICC.2, ICC.3 = NULL,
      #  rho, omega.2, omega.3 = NULL,
      #  tnum = 10000, B = 1000, cl = NULL, updateProgress = NULL
      #)
      
      isolate(pump_power(design = input$design,
                         MTP = input$MTP,
                         MDES = as.numeric(unlist(strsplit(input$MDES, ","))),
                         M = input$M, # The number of hypotheses/outcomes
                         J = input$J, # The number of schools
                         K = input$K, # The number of districts
                         nbar = input$nbar, # The number of units per block
                         Tbar = input$tbar, # The proportion of samples that are assigned to the treatment
                         alpha = input$alpha,
                         numCovar.1 = input$numCovar.1,
                         numCovar.2 = 0,
                         numCovar.3 = 0,
                         R2.1 = rep(input$R2.1,input$M),
                         R2.2 = rep(0.1, input$M),
                         R2.3 = NULL,
                         ICC.2 = rep(0, input$M),
                         ICC.3 = NULL ,
                         rho = input$rho,
                         omega.2 = 0,
                         omega.3 = NULL,
                         tnum = 10000, 
                         B = 100, 
                         cl = NULL,
                         updateProgress = updateProgress)
      )
    }, include.rownames = TRUE)# Wrapping a reactive expression to a reactive table object for output view
    
  }) # observe Event go Button for power Cluster
  
  
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







