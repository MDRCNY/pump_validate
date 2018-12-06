library(shiny) # for basic templates
library(shinyBS) # for popovers and tool tips
library(pum)
#For testing purposes
source("../pum-p/R/blockrct2_power.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("PUM: Power Under Multiplicity"),
   
   # Nav Bar to construct the set of options we have
   
   navbarPage(title = NULL, id = "mainmenu", 
      tabPanel("Landing Page"),
      tabPanel("MHT"),
      tabPanel("MTP",
              
          h2("Types of Multiple Testing Procedures", style = "color:blue")
               
      ), # Content within MTP
      tabPanel("Power-I"),
      tabPanel("Power-II"),
      tabPanel("Calculations",
              sidebarLayout(
                 sidebarPanel(
                   fluidRow(
                    column(10,
                      div(style = "display: inline-block, vertical-align:top;", selectInput("MTP", "What MTP do you plan to use?", 
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
                      
                     column(5,
                        numericInput("M", "Number of Outcomes", min = 1, max = 10, value = 5, step = 1)
                     ), # column for number of outcomes
                      
                     column(5,
                        numericInput("MDES", "Minimum effect size", value = 0.125, min = 0, max = 5, step = 0.001)
                     ), # column for MDES
                     
                     column(2,
                            div(style ="display: inline-block, vertical-align:top;",actionButton("question_mdes",label = "", icon = icon("question"))) #div for button ends                            
                     ) # column for action button
              
                   ), # number of outcomes and mdes

                   bsPopover(id = "question_mdes", title = NULL,
                             content = paste0("For MDES, you would want to consider, etc etc"),
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")),
                
                   fluidRow(
                     
                     column(6,
                        
                        numericInput("J", "Number of blocks", min = 1, max = 10, value = 10, step = 1)
                        
                     ), # number of blocks
                     
                     column(6,
                    
                        numericInput("n.j","Number of units per block", min = 2, max = 100, value = 20, step = 1)     
                            
                     ) # number of units per blocks
    
                   ), # Nmber of blocks and number of units per block
                   
                   fluidRow(
                     
                     column(6,
                        
                        numericInput("rho", "Correlation between tests", value = 0.3, min = 0, max = 1.0, step = 0.01)
                            
                    ), # corrleation btw tests
                     
                     column(6,
                    
                        sliderInput("ICC", "Intraclass correlation",min = 0, max = 1, value = 0.3, step = 0.1, animate=animationOptions(interval=100, loop=TRUE))     
                            
                    ) #intraclass correlation
                   ), # column correlation btw tests & intraclass correlation!
                   
                   fluidRow(
                     
                     column(6,
                    
                        numericInput("p", "Proportion of Treatment assignment", min = 0.001, max = 1.0, value = 0.25, step = 0.001)
                            
                    ), # proportion of treatment assignment
                     
                     column(6,
                            
                        numericInput("alpha", "Significance Level of Tests (alpha)", min = 0.001, max = 0.9, value = 0.05, step = 0.001)
                          
                    ) #Significance Level of Tests
                     
                   ), # proportion of treatment assignment and significance level of tests
                  
                fluidRow(
                  column(6,
                         
                    numericInput("numCovar.1", "Number of Level 1 Covariates", min = 0, max = 10, value = 5, step = 1 )
                  
                  ),
                  
                  column(6,
                         
                    numericInput("numCovar.2", "Number of Level 2 Covariates", min = 0, max = 10, value = 5, step =1)
                  
                  )
                ), #fluid row for block level covariate inputs
            
                fluidRow(
                  
                  column(6,
                         
                    numericInput("tnum", "Numer of Permutation", min = 5000, max = 50000, value = 500, step = 500)
                  
                  ), # Permutation column
                  
                  column(6,
                    
                    numericInput("snum", "Number of Samples", min = 10, max = 500, value = 10, step = 1)
                  
                  ) # Number of Sample column
                ) # fluid row for permutation and sample ssize
                
                
                 ), #sidebar Panel
              mainPanel (
                       tableOutput("view") #The view table output
              ) #main panel
              
          ) #sidebar Layout
              
      )# Calculation Tabset Panel
              
  )# navBarPage
  
)# fluidPage

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session = TRUE) {
  
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
  
  random_block <- reactive({
    
    power.blockedRCT.2( M = input$M, MDES = input$MDES, J = input$J, n.j = input$n.j, rho = input$rho, p = input$p, alpha = input$alpha, 
                       numCovar.1 = input$numCovar.1, numCovar.2 = input$numCovar.2, ICC = input$ICC, tnum = input$tnum, snum = input$snum)
    
  })#random data block
  
  #Rendering a reactive object table for the Data Generating Process
  output$view <- renderTable({
    #displaying based on the number of sample sizes
    random_block()
    
  }, include.rownames = TRUE)# Wrapping a reactive expression to a reactive table object for output view
  
}) #Server side actions

# Run the application 
shinyApp(ui = ui, server = server)

