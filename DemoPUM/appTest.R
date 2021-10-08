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
                  tabPanel(id = "Single Scenario",
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
                                                            "Two Level Blocked RCT - Random Effects" = "d2.1_m2fr"),
                                             selected = "d2.1_m2ff")) # select input buttons div
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
                            ), # MTP shared by all designs
                        
                           fluidRow(
                                column(12,
                                 uiOutput("M"))
                           ), # Number of Outcomes
                        
                           # fluidRow(
                           #      column(12,
                           #       uiOutput("MDES"))
                           # ), # Minimum Detectable Effect Size
      
                           conditionalPanel(condition = c("input.designP2LBISS == 'd2.1_m2ff' || input.designP2LBISS == 'd2.1_m2fr'"),
                              fluidRow(
                                column(12,
                                  uiOutput("R2.1"))
                              ) # Adding R2.1 which does not exist in 1 level constant effect
   
                           ), # conditional Panel for the 2 level constant & fixed effects
                              
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
  
  ##########################################################
  # Get reactive expression for experimental chosen design #
  ##########################################################
  
  getDesign <- reactive({
    
    if(input$designP2LBISS == "d2.1_m2ff") {
      
      print("d2.1_m2ff")
      return(c("d2.1_m2ff"))
      
    } else if(input$designP2LBISS == "d2.1_m2fr"){
      
      print("d2.1_m2fr")
      return(c("d2.1_m2fr"))
      
    } else if(input$designP2LBISS == "d2.1_m2fc") {
      
      print("d2.1_m2fc")
      return(c("d2.1_m2fc"))
      
    }
  
  }) # getDesign
  
  ######################################################
  # Rendering Variable Objects to UI for chosen design #
  ######################################################
  
  output$MTP <- renderUI({
    
    theDesign = as.character(getDesign())
    div(style = "display: inline-block, vertical-align:top;", 
        mtpInput(design = theDesign , scenario = "singlescenario"))
    
  }) # MTP for chosen design
  
  output$M <- renderUI({
    
    theDesign = as.character(getDesign())
    div(style = "display: inline-block, vertical-align:top;", 
        mInput(design = theDesign, scenario = "singlescenario"))
  }) # Number of Outcomes 
  
  # output$MDES <- renderUI({
  #   
  #   theDesign = as.character(getDesign())
  #   div(style = "display: inline-block, vertical-align:top:",
  #       mdesInput(design = theDesign, scenario = "singlescenario"))
  # }) # Minimum detectable effect size

  output$R2.1 <- renderUI({
    
    theDesign = as.character(getDesign())
    div(style = "display: inline-block, vertical-align:top;", 
        r2.1Input(design = theDesign, scenario = "singlescenario"))
  
  }) # R2.1 element for chosen and required designs
  
  
  observeEvent(input$goButtonP2LBISS,{
    
    # set a Reactive Value for Power Table
    reactPowerTable <- reactiveVal()
    
    #########################
    # Creating Progress bar #
    #########################
    
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
    
    ############################
    # Generating Power Results #
    ############################
    
    #browser()
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
    
    #################################################
    # Rendering Single Scenario Power Table Results #
    #################################################
    
    # Rendering a reactive object table from the power function
    output$powercalcTableP2LBISS <- renderDataTable({
      
      DT::datatable(dat,
                    extensions = 'Buttons',
                    options = list(
                      paging = TRUE,
                      pageLength = 5,
                      scrollY = TRUE,
                      dom = 'Bfrtip',
                      buttons = c('csv', 'excel')
                    ))
    })# Wrapping a reactive expression to a reactive table object for output view
    
    ############################
    # Data preparation for plot #
    ############################
    dat <- reactPowerTable()
    
    singleScenario2LevelBlockedDatLong <- 
      dat %>%
      dplyr::select_all() %>%
      dplyr::select(-indiv.mean) %>%
      tidyr::pivot_longer(!MTP, names_to = "powerType", values_to = "power")
    
    ################################################
    # Rendering Single Scenario Power Table Graphs #
    ################################################
    
    output$powercalcGraphP2LBISS <- renderPlotly({
      
      pg <- plotly::ggplotly(
        ggplot(
          data = singleScenario2LevelBlockedDatLong,
          aes(x = powerType, 
              y = power, 
              shape = MTP,
              colour = MTP)) + 
          geom_point(size = 2) +
          scale_y_continuous(limits = c(0,1)) +
          ggtitle("Adjusted Power values across different Power Definitions") +
          theme(plot.title = element_text(size = 16,
                                          face = "bold",
                                          vjust = 1,
                                          hjust = 0.5),
                axis.text = element_text(size = 14),
                axis.title = element_text(size = 14)
          )
        + labs(colour = "",
               shape = "")
      ) # End of ggplot
      
      pg <- layout(pg, 
                   #title = "<b>Adjusted Power values across different \n Power Definitions & MDES values </b>",
                   margin=list(t = 75),
                   legend = list(x = 100,
                                 orientation = "v",
                                 xanchor = "center",
                                 y = 0.5,
                                 title = list(text = '<b> MTP Type </b>')))
      
      pg %>%
        config(displaylogo = FALSE,
               collaborate = FALSE,
               displayModeBar = TRUE,
               modeBarButtonsToRemove = list(
                 'sendDataToCloud',
                 'autoScale2d',
                 'resetScale2d',
                 'hoverClosestCartesian',
                 'hoverCompareCartesian',
                 'zoom2d',
                 'pan2d',
                 'select2d',
                 'lasso2d',
                 'zoomIn2d',
                 'zoomOut2d',
                 'toggleSpikelines'
               ))
      
    }) # plotly graph
    
  }) # observe Event go Button for power for Single Scenario
  
  
  
  
  
  
  
}) # end of server side

# Run the application 
shinyApp(ui = ui, server = server)
