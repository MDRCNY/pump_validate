library(shiny) # for basic templates
library(shinyBS) # for popovers and tool tips
library(shinycssloaders) # for ui elements showing shiny loading
library(magrittr) # piping operator
library(pum) # our pum lirbary

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Nav Bar to construct the set of options we have along with the name of the title
   
   navbarPage(title = "Power Under Multiplicity", id = "mainmenu", 
      tabPanel("Home"),
      tabPanel("MHT"),
      tabPanel("MTP",
              
          h2("Types of Multiple Testing Procedures", style = "color:blue")
               
      ), # Content within MTP
      tabPanel("Power Primer"),
      tabPanel("Power Effects"),
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
                      div(style = "display: inline-block, vertical-align:top;", selectInput("MTP", "Which MTP do you plan to use?", 
                               choices = list("Bonferroni" = "BF", "Holm" = "HO", "Westfall-Young" = "WY","Benjamini-Hochberg" = "BH"))) # select input buttons div
                    ), # column for inputs

                    column(2, 
                      div(style ="display: inline-block, vertical-align:top;",actionButton("question_mtp",label = "", icon = icon("question"))) #div for button ends
                    ) # column for buttons
                                          
                   ), # fluid Row to contain the question mark issue 
                   
                   bsPopover(id = "question_mtp", title = NULL,
                             content = paste0("For more information on MTP, please click!"),
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")), # the bsPopover for the more information section of the Shiny App
                   
                   fluidRow(
                      
                     column(6,
                        numericInput("M", "Number of Outcomes", min = 1, max = 10, value = 5, step = 1)
                     ), # column for number of outcomes
                      
                     column(6,
                            numericInput("MDES", "Minimum effect size", value = 0.125, min = 0, max = 5, step = 0.001)
                     ) # column for Minimum detectable effect size
                     
                   ), # number of outcomes and mdes

                   
                   fluidRow(

                     column(10,
                        numericInput("numFalse", "Number of Outcomes with an expected non-zero effects", value = 3, min = 0, max = 10, step = 1)
                     ), # column for outcomes with actual effect size
                     
                     column(2,
                            div(style ="display: inline-block, vertical-align:top;",actionButton("question_mdes",label = "", icon = icon("question"))) #div for button ends                            
                     ) # column for action button
                     
                   ), # MDES and number of outcomes with expected actual effect
                   
                   bsPopover(id = "question_mdes", title = NULL,
                             content = paste0("For MDES, you would want to consider, etc etc"),
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")),
                
                   fluidRow(
                     
                     column(6,
                        
                        numericInput("J", "Number of blocks", min = 1, max = 100, value = 50, step = 1)
                        
                     ), # number of blocks
                     
                     column(6,
                    
                        numericInput("n.j","Number of units per block", min = 2, max = 100, value = 20, step = 1)     
                            
                     ) # number of units per blocks
    
                   ), # Nmber of blocks and number of units per block
                   
                   fluidRow(
                     
                     column(12,
                        
                        numericInput("R2.1", "Level 1 R2", value = 0.2, min = 0, max = 1.0, step = 0.01)
                            
                    ) # R square for level 1
      
                   ), # column correlation btw tests & intraclass correlation!
                   
                   fluidRow(
                     
                     column(6,
                    
                        numericInput("p", "Proportion of Treatment assignment", min = 0.001, max = 1.0, value = 0.5, step = 0.001)
                            
                    ), # proportion of treatment assignment
                     
                     column(6,
                            
                        numericInput("alpha", "Significance Level of Tests (alpha)", min = 0.001, max = 0.9, value = 0.05, step = 0.001)
                          
                    ) #Significance Level of Tests
                     
                   ), # proportion of treatment assignment and significance level of tests
                  
                fluidRow(
                  column(12,
                         
                    numericInput("numCovar.1", "Number of Level 1 Covariates", min = 0, max = 10, value = 1, step = 1 )
                  
                  ) # Number of Level 1 covariates
        
                ), #fluid row for block level covariate inputs
                
                fluidRow(
                  
                  column(12,
                         actionButton("goButton_power", "Go!") # Action Button to trigger other reactive values
                  ) # Column for action button
                  
                )
            
                 ), #sidebar Panel
              mainPanel (
                       tableOutput("powercalc") #The power calculation table output
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
                        choices = list("Bonferroni" = "BF", "Benjamini-Hocheberg" = "BH", "Holms" = "HO", "Westfall-Young-SS" = "WY-SS", "Westfall-Young-SD" = "WY-SD"),
                        selected = "BF")) # select input buttons div
                    
                ), # MTP, the Mutliple Testing Procedure in Use
                
                column(6,
                    numericInput("M_mdes", "Number of Outcomes", min = 1, max = 10, value = 5, step = 1)
                ) # M, the number of outcomes in use
              ), # Fluid Row for MTP and M outcomes
              
              fluidRow(
                
                column(10,
                       numericInput("Aimpact_mdes", "Number of Outcomes with an expected non-zero effects", value = 3, min = 0, max = 10, step = 1)
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
                                           choices = list("Bonferroni" = "BF", "Benjamini-Hocheberg" = "BH", "Holms" = "HO", "Westfall-Young-SS" = "WY-SS", "Westfall-Young-SD" = "WY-SD"),
                                           selected = "BF")) # select input buttons div
                           
                    ), # MTP, the Mutliple Testing Procedure in Use
                    
                    column(6,
                           numericInput("M_sample", "Number of Outcomes", min = 1, max = 10, value = 5, step = 1)
                    ) # M, the number of outcomes in use
                  ), # Fluid Row for MTP and M outcomes
                  
                  fluidRow(
                    
                    column(10,
                           numericInput("NumFalse_sample", "Number of False Nulls", value = 3, min = 0, max = 10, step = 1)
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
                           numericInput("n.j_sample","Number of units per block", min = 2, max = 100, value = 20, step = 1)     
                           
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
     
  )# navBarPage
  
)# fluidPage

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session = FALSE) {
  
  ############################################
  # Power Calculation Server Side Begins
  ############################################
  
  #Observing the action button click and switching to a different Tab. Passing the session to keep the info from previous tab.
  observeEvent(input$question_mtp,{
    updateTabsetPanel(session, "mainmenu", selected = "MTP" )
  }) # Action button that switch tabs
  
  # power <- reactive({
  #   
  #   power.blockedRCT.2(M = input$M, MDES = input$MDES, numFalse = input$numFalse, J = input$J, n.j = input$n.j, R2.1 = input$R2.1, p = input$p, alpha = input$alpha, 
  #                       numCovar.1 = input$numCovar.1, numCovar.2 = NULL, ICC = NULL, tnum = 10000, snum = 10)
  #   
  # }) # reactive expression for power
  
  #observe Event for power calculation: Using observeEvent instead of eventReactive as we want to see the immediate side effect
  observeEvent(input$goButton_power,{
  
    #Rendering a reactive object table from the power function
    output$powercalc <- renderTable({
      
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
      isolate(power.blockedRCT.2(M = input$M, MDES = input$MDES, numFalse = input$numFalse, J = input$J, n.j = input$n.j, R2.1 = input$R2.1, p = input$p, alpha = input$alpha, 
                                numCovar.1 = input$numCovar.1, numCovar.2 = NULL, ICC = NULL, tnum = 10000, snum = 10, updateProgress = updateProgress))
      
    }, include.rownames = TRUE)# Wrapping a reactive expression to a reactive table object for output view
    
}) # observe Event go Button for power
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

     MDES.blockedRCT.2(M = input$M_mdes, numFalse = input$M_mdes, Ai_mdes = input$Aimpact_mdes, J = input$J_mdes, n.j = input$n.j_mdes, power=input$power_mdes, power.definition = input$pdefn_mdes, MTP=input$MTP_mdes, marginError = input$me_mdes, p = input$p_mdes, alpha = input$alpha_mdes, numCovar.1=input$numCovar.1_mdes, numCovar.2=NULL, R2.1=input$R2.1_mdes, R2.2=0, ICC=0,
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
      isolate(MDES.blockedRCT.2(M = input$M_mdes, numFalse = input$M_mdes, Ai_mdes = input$Aimpact_mdes, J = input$J_mdes, n.j = input$n.j_mdes, power=input$power_mdes, power.definition = input$pdefn_mdes, MTP=input$MTP_mdes, marginError = input$me_mdes, p = input$p_mdes, alpha = input$alpha_mdes, numCovar.1=input$numCovar.1_mdes, numCovar.2=NULL, R2.1=input$R2.1_mdes, R2.2=0, ICC=0,
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
      isolate(SS.blockedRCT.2(M = input$M_sample, numFalse = input$NumFalse_sample, typesample = input$typesample, J = input$J_sample, 
                              n.j = input$n.j_sample,J0 = 10, n.j0 = 10,MDES = input$MDES_sample, power=input$power_samples, 
                              power.definition = input$pdefn_sample, MTP=input$MTP_sample, 
                              marginError = input$me_sample, p = input$p_sample, alpha = input$alpha_sample, 
                              numCovar.1=input$numCovar.1_sample, numCovar.2=NULL, R2.1=input$R2.1_sample, R2.2=0, ICC=0,
                              mod.type="constant", omega=NULL,
                              tnum = 10000, snum=2, ncl=2, updateProgress = updateProgress)) #data table that is isolated
      
    }) # end of isolate. We do not want 
  }) # Sample calculation
  
}) #Server side actions

# Run the application 
shinyApp(ui = ui, server = server)

