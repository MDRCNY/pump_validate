library(shiny) # for basic templates
library(shinyBS) # for popovers and tool tips
library(shinycssloaders) # for ui elements showing shiny loading
library(magrittr) # piping operator
#library(pum)
#For testing purposes
#source("../powerFunctions_demo.R")
source("../blockrct2_power.R")



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
                        numericInput("Aimpact", "Number of Outcomes with an expected non-zero effects", value = 3, min = 0, max = 10, step = 1)
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
        
                ) #fluid row for block level covariate inputs
                
                 ), #sidebar Panel
              mainPanel (
                       tableOutput("view") %>% withSpinner() #The view table output
              ) #main panel
              
          ) #sidebar Layout
              
      ), # Calculation Tabset Panel
      
      tabPanel("MDES Calculation",
          sidebarLayout(
            sidebarPanel(
              fluidRow(
                column(6,
                    div(style = "display: inline-block, vertical-align:top;", 
                        selectInput("MTP_mdes", "MTP", 
                        choices = list("Bonferroni" = "BF", "Benjamini-Hocheberg" = "BH"))) # select input buttons div
                    
                ), # MTP, the Mutliple Testing Procedure in Use
                
                column(6,
                    numericInput("M_mdes", "Number of Outcomes", min = 1, max = 10, value = 5, step = 1)
                ) # M, the number of outcomes in use
              ), # Fluid Row for MTP and M outcomes
              
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
                    div(style = "display: inline-block, vertical-align:top;", 
                           selectInput("pdefn_mdes", "Power Defn", 
                                       choices = list("Inidvidual" = "indiv", "Complete" = "complete")))
                ) # Choice of Power and Power Definition
              ), # Fluid Row for Proportion of treatment assignment
              
              fluidRow(
                
                column(12,
                      numericInput("p_mdes", "Proportion of Treatment assignment", min = 0.001, max = 1.0, value = 0.5, step = 0.001)
                ) # Proportion of treatment assignment
              ) # fluid row
              
            ), # Side Bar Panel
          
          mainPanel(
            
            textOutput("mdes") %>% withSpinner() #Textoutput of the Spinner
            
            
            
          ) # Main Panel Layout
        ) # Sidebar Panel       
     ) # MDES calculation panel
  
  )# navBarPage
  
)# fluidPage

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session = FALSE) {
  
  #Observing the action button click and switching to a different Tab. Passing the session to keep the info from previous tab.
  observeEvent(input$question_mtp,{
    updateTabsetPanel(session, "mainmenu", selected = "MTP" )
  })
  
  
  ##Data Generating Process attached with 5 input parameters of 
  ### N, number of sample
  ### beta_0, the intercept
  ### beta_1, the coefficient of covariate X1
  ### seed, random number generating process
  ### assignment prob, the assignment probabilities
  
  #A reactive expression for the power function
  
  # power.blockedRCT.2<-function(M, MDES, J, n.j,rho,
  #                             p, alpha, numCovar.1, numCovar.2, R2.1, R2.2, ICC, 
  #                            omega = NULL,
  #                            tnum, snum, ncl)
  
  power <- reactive({
    
    power.blockedRCT.2(M = input$M, MDES = input$MDES, Ai = input$Aimpact, J = input$J, n.j = input$n.j, R2.1 = input$R2.1, p = input$p, alpha = input$alpha, 
                        numCovar.1 = input$numCovar.1, numCovar.2 = NULL, ICC = NULL, tnum = 10000, snum = 10)
    
  }) # reactive expression for power
  
  #Rendering a reactive object table from the power function
  output$view <- renderTable({
    #displaying based on the number of sample sizes
    power()
    
  }, include.rownames = TRUE)# Wrapping a reactive expression to a reactive table object for output view

  
  mdes <- reactive({

     MDES.blockedRCT.2(M = input$M_mdes, numFalse = input$M_mdes, J = input$J_mdes, n.j = input$n.j_mdes, power=input$power_mdes, power.definition = input$pdefn_mdes, MTP=input$MTP_mdes, marginError = input$me_mdes, p = input$p_mdes, alpha = input$alpha_mdes, numCovar.1=input$numCovar.1_mdes, numCovar.2=NULL, R2.1=input$R2.1_mdes, R2.2=0, ICC=0,
                       mod.type="constant", omega=NULL,
                       tnum = 10000, snum=2, ncl=2)

    #testzarni (M = input$M)
    
  }) # reactive expression for mdes Note: Need to manage the sigma
  
  #Rendering a reactive vector object from the
  output$mdes <- renderText({
  
    mdes()
    
  }) # mdes output
  
  
}) #Server side actions

# Run the application 
shinyApp(ui = ui, server = server)

