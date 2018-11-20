library(shiny) # for basic templates
library(shinyBS) # for popovers and tool tips
source("../dgp.R") # Testing data generating function. This will be replaced by a library package


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
                   selectInput("plot.what", "What do you want on the X-axis?", 
                               choices = list("Correlation between outcomes" = "cor", "Number of Blocks" = "nblocks", "Number of Individuals" = "nindiv","R2" = "R2")),
                   
                   fluidRow(
                    column(10,
                      div(style = "display: inline-block, vertical-align:top;", selectInput("MTP", "What MTP do you plan to use?", 
                               choices = list("Bonferroni" = "BF", "Holm" = "HO", "Westfall-Young" = "WY","Benjamini-Hochberg" = "BH"))) # select input buttons div
                    ), # column for inputs

                    column(2, 
                      div(style ="display: inline-block, vertical-align:top;",actionButton("question",label = "", icon = icon("question"))) #div for button ends
                    ) # column for buttons
                                          
                   ), # fluid Row to contain the question mark issue 
                   
                   bsPopover(id = "question", title = NULL,
                             content = paste0("For more information on MTP, please click!"),
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")), # the bsPopover for the more information section of the Shiny App
                   
                   selectInput("ntests", "Number of tests", choices = list("3" = 3, "6" = 6, "9" = 9,"12" = 12)),
                   sliderInput("cor", "Correlation between outcomes",min = 0, max = 1, value = 0.1, step = 0.1, animate=animationOptions(interval=100, loop=TRUE)),     
                   sliderInput("nblocks", "Number of blocks", min = 10, max = 60, value = 10,step=10, animate=animationOptions(interval=100, loop=TRUE)),     
                   sliderInput("nindiv", "Number of individuals per block",min = 10, max = 60, value = 10,step=10,animate=animationOptions(interval=100, loop=TRUE)),     
                   sliderInput("R2", "R2", min = 0, max = 1, value = 0.1 ,step=0.1, animate=animationOptions(interval=100, loop=TRUE))
                 ), #sidebar Panel
              mainPanel (
                fluidRow(
                  
                  box(
                    title = "Calculation Answers", width = 12, height = 300
                  ), # box for answers
                  
                  box( title = "Data Views", width = 12, height = 300,
                       tableOutput("view") #The view table output
                  ) # box for data views
                  
                ) # fluid Row 

              ) #main panel
              
          ) #sidebar Layout
              
      )# Calculation Tabset Panel
              
  )# navBarPage
  
)# fluidPage

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session = TRUE) {
  
  #Observing the action button click and switching to a different Tab
  observeEvent(input$question,{
    updateTabsetPanel(session, "mainmenu", selected = "MTP" )
  })
  
  
  ##Data Generating Process attached with 5 input parameters of 
  ### N, number of sample
  ### beta_0, the intercept
  ### beta_1, the coefficient of covariate X1
  ### seed, random number generating process
  ### assignment prob, the assignment probabilities
  
  #A reactive expression for the data generating process
  
  random_block <- reactive({
    #data generating process 
    dgp(N = input$nblocks, beta_0 = input$cor, beta_1 = input$nindiv, seed = input$R2, tau = 5)
    
  })#random data block
  
  #Rendering a reactive object table for the Data Generating Process
  output$view <- renderTable({
    #displaying based on the number of sample sizes
    random_block()
    
  })# Wrapping a reactive expression to a reactive table object for output view
  
}) #Server side actions

# Run the application 
shinyApp(ui = ui, server = server)

