####################################################################################
#####
##
#                 Power Primer Shiny App
##
#####
###################################################################################

# Libraries loaded here
library(shiny)
library(dropR)
library(shinyBS)
library(shinydashboard)
library(DT)

# Sourcing external functions
source("dgp.r")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Power Panel Primer App"),
   
   # Sidebar with several inputs for a dgp and others
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30), #binsliderInpiut
         
         numericInput("n", "N", value = 100, min = 100, max = 100000), #input for Sample Size
         
         sliderInput("beta_0", "Beta_0", min = 5, max = 15, value = 10, step = 1), # beta_0/intercept input
         
         sliderInput("beta_1", "Beta_1", min = 1, max = 5, value = 3.5, step = 0.5), # beta_1/effect size input
         
         numericInput("seed", "Seed", value = 1234, min = 1000, max = 2000) #setting the seed of the DGP
         
      ), #Sidebar Panel
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         tableOutput("view") #The data table output
      )#Main Panel
   
  )#SidebarLayout
   
)#UI

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })# The default histogram plot given by R Shiny template
   
   ##Data Generating Process attached with 5 input parameters of 
   ### N, number of sample
   ### beta_0, the intercept
   ### beta_1, the coefficient of covariate X1
   ### seed, random number generating process
   ### assignment prob, the assignment probabilities
   
   #A reactive expression for the data generating process
   
   random_block <- reactive({
     
     #data generating process 
     dgp(N = input$n, beta_0 = input$beta_0, beta_1 = input$beta_1, seed = input$seed, tau = 5)
     
     
   })#random data block
   
   #Rendering a reactive object table for the Data Generating Process
   output$view <- renderTable({
     #displaying based on the number of sample sizes
     random_block()
     
   })# Wrapping a reactive expression to a reactive table object for output view
   
}#Server

# Run the application 
shinyApp(ui = ui, server = server)

