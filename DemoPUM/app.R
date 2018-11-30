library(shiny) # for basic templates
library(shinyBS) # for popovers and tool tips
source("../dgp.R") # Testing data generating function. This will be replaced by a library package
source("../powerFunctions_demo.R") # Power functions for demoing purposes

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("PUM: Power Under Multiplicity"),
   
   # Nav Bar to construct the set of options we have
   
   navbarPage(title = NULL, id = "mainmenu", 
      tabPanel("Landing Page"),
      tabPanel("MHT"),
      tabPanel("MTP"),
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
                   
                   #'     # estimates power, MDES or sample size (either number of blocks or number of units per block)
                   #'     # for either constant, fixed or random effects
                   #' 
                   #'     # M is the number of hypothesis tests (outcomes)
                   #'     # MDES = vector of MDES's of length M - This is to specify the minimum detectable effect size
                   #'     # J = number of blocks
                   #'     # n.j = units per block
                   #'     # p = the proportion of samples that are assigned the treatment
                   #'     # power = power (assumed same for all tests)
                   #'     # alpha = significance level
                   #'     # numCovar.1 = number of Level 1 baseline covariates (not including block dummies)
                   #'     # numCovar.2 = number of Level 2 baseline covariates
                   #'     # R2.1 = a vector of R^2 for level 1 variables
                   #'     # R2.2 = a vector of R^2 for level 2 variables
                   #'     # ICC = intraclass correlation
                   #'     # omega =
                   #'     # sigma = correlation matrix for 
                   #'      between test statistics
                   #'     # mod. type = "c" for constant effects, "f" for fixed effects, "r" for random effects
                   #'     # tnum = number of test statistics (samples) for all procedures other than WY & number of permutations for WY
                   #'     # snum is number of samples for WY
                
                   # power.blockedRCT.2<-function(M, MDES, J, n.j,rho,
                   #                             p, alpha, numCovar.1, numCovar.2, R2.1, R2.2, ICC, 
                   #                            omega = NULL,
                   #                            tnum, snum, ncl)
                   
                   sliderInput("M", "Number of Outcomes", min = 1, max = 10, value = 5, step = 1),
                   
                   fluidRow(
                      
                     column(10,
                        numericInput("MDES", "Minimum detect effect size",value = 0.125, min = 0, max = 5, step = 0.001)
                     ), # column for the MDES input
                    
                     column(2, 
                             div(style ="display: inline-block, vertical-align:top;",actionButton("question_mdes",label = "", icon = icon("question"))) #div for button ends
                     ) # column for buttons
                   ), #fluid row to contain just a pop-up
                
                   bsPopover(id = "question_mdes", title = NULL,
                             content = paste0("For MDES, you would want to consider, etc etc"),
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")),
                
                   sliderInput("J", "Number of blocks", min = 1, max = 10, value = 10, step = 1),
                   sliderInput("n.j","Number of units per block", min = 2, max = 100, value = 20, step = 1),
                   numericInput("rho", "Correlation between tests", value = 0.3, min = 0, max = 1.0, step = 0.01),
                   numericInput("p", "Proportion of Treatment assignment", min = 0.001, max = 1.0, value = 0.25, step = 0.001),
                   numericInput("alpha", "Significance Level of Tests (alpha)", min = 0.001, max = 0.9, value = 0.05, step = 0.001),
                
                fluidRow(
                  column(6,
                    numericInput("numCovar.1", "Number of Level 1 Covariates", min = 0, max = 10, value = 5, step = 1 )
                  ),
                  
                  column(6,
                    numericInput("numCovar.2", "Number of Level 2 Covariates", min = 0, max = 10, value = 5, step =1)
                  )
                ), #fluid row for block level covariate inputs
              
                #fluidRow(
                # column(6,
                #         numericInput("R2.1", "A vector of M R-squared for level 1 variables", min = 0, max = 10, value = 3, step = 1 )
                #  ),
                  
                #  column(6,
                #         numericInput("R2.2", "A vector of M R-squared for level 2 variables", min = 0, max = 10, value = 3, step =1)
                #  )
                # ), #fluid row for block level covariate inputs 
                
                   sliderInput("ICC", "intraclass correlation",min = 0, max = 1, value = 0.3, step = 0.1, animate=animationOptions(interval=100, loop=TRUE)),     
                   #sliderInput("nblocks", "Number of blocks", min = 10, max = 60, value = 10,step=10, animate=animationOptions(interval=100, loop=TRUE)),     
                   #sliderInput("nindiv", "Number of individuals per block",min = 10, max = 60, value = 10,step=10,animate=animationOptions(interval=100, loop=TRUE)),     
                   #sliderInput("R2", "R2", min = 0, max = 1, value = 0.1 ,step=0.1, animate=animationOptions(interval=100, loop=TRUE)),
                   numericInput("tnum", "Number of Permutation for non Westfall Young Test Statistics", min = 5000, max = 50000, value = 500, step = 500),
                   numericInput("snum", "Number of Permutation for Westfall Young", min = 5, max = 10000, value = 50, step = 100)
                
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
  
  #Observing the action button click and switching to a different Tab
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
    
  })# Wrapping a reactive expression to a reactive table object for output view
  
}) #Server side actions

# Run the application 
shinyApp(ui = ui, server = server)

