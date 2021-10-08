#######################
# Loading R Libraries #
#######################
library(shiny) # for basic templates
library(shinyBS) # for popovers and tool tips
library(shinycssloaders) # for elements showing shiny loading
library(magrittr) # piping operator
library(ggplot2) # loading ggplot for the plot
library(pum) # our pum library
library(DT) # make nice shiny tables
library(plotly) # Plotly for ggplot to make graphs downloadable

##########################
# Loading source R files #
##########################
source("ui_elements.R")

ui <- fluidPage(
  
  titlePanel(title = "Power Under Multiplicity", windowTitle = "Power Under Multiplicity"), 
  tabsetPanel(id = "mainMenu",
              tabPanel("Home"),
              tabPanel("Educational Resources"),
              tabPanel("Power Calculation", 
                tabsetPanel(id = "subMenu",
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
                                             choices = list("Two Level Blocked RCT - Constant Effects" = "d2.1_m2fc", 
                                                            "Two Level Blocked RCT - Fixed Effects" = "d2.1_m2ff", 
                                                            "Two Level Blocked RCT - Random Effects" = "d2.1_m2fr"))) # select input buttons div
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
                              column(12,
                                     uiOutput("MTP"))
                            ),
                              
                            fluidRow(
                              column(6,
                                    actionButton("goButtonP2LBISS", "Go!") # Action Button to trigger other reactive values
                                    )
                              ) # end of fluid Row
              ), # Power calculation sidebarPanel
                                       
              mainPanel(
                br(),    
                br(),
                                         
                            fluidRow(
                              column(8, align = "center",
                                        offset = 2,
                                        plotlyOutput("powercalcGraphP2LBISS"))
                            ), # end of Fluid Row
                                         
                              br(), # To create spaces between Table and Plots
                              br(), # To create spaces between Table and Plots
                              br(), # To create spaces between Table and Plots
                              br(), # To create spaces between Table and Plots
                              br(), # To create spaces between Table and Plots
                              br(), # To create spaces between Table and Plots
                                         
                            fluidRow(
                              column(12, align = "center",
                                     DT::dataTableOutput("powercalcTableP2LBISS")) #The power calculation table output
                            ) #fluidRow for first half of the page
                                         
            ) # Power calculation Main Panel
                                       
          ) # Power Calculation sidebar Layout
        ) # Single Scenario
      ) # End of Power Panel                     
    ) # Two sub Menus possible
               
  ) # End of main tabset Panel
) # end of Fluid Page


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session = FALSE) {
  
  # get reactive expression for design
  getDesign <- reactive({
    
    if(input$designP2LBISS == "d2.1_m2ff") {
      
      return(c("d2.1_m2ff"))
      
    } else if(input$designP2LBISS == "d2.1_m2fr"){
      
      return(c("d2.1_m2fr"))
      
    }
  
  })
  
  #RenderUI for the selector input for different power definitions
  output$MTP <- renderUI({
    
    theDesign = as.character(getDesign())
    # browser()
    div(style = "display: inline-block, vertical-align:top;", 
        mtpInput(design = theDesign , scenario = "singleblock"))
    
  }) #power select input options
  
  
  
}) # end of server side

# Run the application 
shinyApp(ui = ui, server = server)
