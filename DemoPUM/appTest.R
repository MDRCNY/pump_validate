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
library(shinyFeedback)
##########################
# Loading source R files #
##########################
source("singlescenario_uielements.R")
source("explorer_uielements.R")

ui <- fluidPage(
  
  useShinyFeedback(),
  titlePanel(title = "Power Under Multiplicity", windowTitle = "Power Under Multiplicity"), 
  tabsetPanel(id = "tabset", type = "tabs",
              tabPanel(title = "Home", value = "home_tab"),
              tabPanel(title = "Educational Resources", value = "edu_tab"),
              tabPanel(title = "Single Scenario", value = "single_scenario_tab", 
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
                            
                            div(style = "display: inline-block, vertical-align:top;", 
                            column(12,
                              selectInput("estimationSs", "What would you like to estimate?",
                                          choices = list("Power" = "power",
                                                         "Minimum detectable effect size" = "mdes",
                                                         "Sample size" = "sample"),
                                          selected = "power")
                              
                            )) # User Mode of Exploration

                          ), # picking the type of exploration you would like to run
                          
                          fluidRow(
                            
                            div(style = "display: inline-block, vertical-align:top;", 
                            column(12,
                             selectInput("designSs", "What research design is this for?", 
                                        choices = list("Design: 1 level, Randomization: level 1 - Constant effects" = "d1.1_m2cc",
                                                       "Design: 2 levels, Randomization: level 1 - Constant effects" = "d2.1_m2fc", 
                                                       "Design: 2 levels, Randomization: level 1 - Fixed effects" = "d2.1_m2ff", 
                                                       "Design: 2 levels, Randomization: level 1 - Random effects" = "d2.1_m2fr",
                                                       "Design: 3 levels, Randomization: level 1 - Random effects" = "d3.1_m3rr2rr",
                                                       "Design: 2 levels, Randomization: level 2 - Random effects" = "d2.2_m2rc",
                                                       "Design: 3 levels, Randomization: level 3 - Random effects" = "d3.3_m3rc2rc",
                                                       "Design: 3 levels, Randomization: level 2 - Fixed effects" = "d3.2_m3ff2rc",
                                                       "Design: 3 levels, Randomization: level 2 - Random effects" = "d3.2_m3rr2rc"
                                                       ),
                                        selected = "d2.1_m2ff")     

                            )) # selecting designs
                            
                          ), # picking the research design
                        
                          fluidRow(
                            
                            div(style = "display: inline-block, vertical-align:top;",
                            column(12,
                              numericInput("numOutcomesSs", "Number of Outcomes", 
                                           min = 1, 
                                           max = 10, 
                                           value = 5, 
                                           step = 1)
                              ) # number of Outcomes    
                            ) # div
                            
                          ), # number of outcomes selection 
                        
                           fluidRow(
                             
                            column(6,
                            uiOutput("nbar")),
                             
                            column(6,
                            uiOutput("j"))
                             
                           ), # Units per block and number of blocks  
                          
                           fluidRow(
                             
                            column(12,
                            uiOutput("mtp"))
                            
                            ), # MTP shared by all designs
                      
                           fluidRow(
                             
                            column(12,
                            uiOutput("mdes"))
                            
                           ), # Minimum Detectable Effect Size
                        
                           fluidRow(
                             
                            column(12,
                            uiOutput("rho"))
                            
                           ), # correlation between outcomes
                           
                           fluidRow(
                             
                             column(12,
                             uiOutput("numCovar.1"))
                             
                           ), # number of covariates in level 1
                        
                           fluidRow(
                             
                             column(12,
                             uiOutput("tbar"))
                             
                           ), # proportion of treatment assignment
                        
                           fluidRow(
                             
                             column(12,
                             uiOutput("alpha"))
                             
                           ), # alpha
                        
                           fluidRow(
                             
                             column(12,
                             uiOutput("r2.1"))
                             
                           ), # Adding R2.1

                           fluidRow(
                              
                              column(12,
                              uiOutput("icc.2"))
                              
                           ), # Adding icc.2
                          
                           fluidRow(
                             
                             column(12,
                             uiOutput("r2.2"))

                           ), # Adding R2.2
                           
                           fluidRow(
                             
                             column(12,
                             uiOutput("omega.2"))
                             
                           ), # Adding omega.2
                        
                           fluidRow(
                             
                             column(12,
                             uiOutput("r2.3")      
                                )
                           ), # Adding r2.3
                        
                           fluidRow(
                             
                             column(12,
                             uiOutput("icc.3"))
                             
                           ), # Adding icc.3
                        
                           fluidRow(
                             
                             column(12,
                             uiOutput("k"))
                             
                           ), # Adding k
                        
                           fluidRow(
                             
                             column(12,
                             uiOutput("omega.3"))
                             
                           ), # Adding omega.3
     
                           fluidRow(
                              
                              column(6,
                              actionButton("goButtonSs", "Go!")) 
                            
                           ) # Action Button to trigger other reactive values
              ), # Power calculation sidebarPanel
                                       
              mainPanel(
                          # Put some text here #
                          fluidRow(
                            
                            column(12, align = "center",
                                   offset = 2,
                                   #STh here for text
                                   )
                            
                          ),
                
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
          
    ), # Single Scenario Tab
    
    tabPanel(title = "Explorer", value = "explorer_tab",
  
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
                   
                   div(style = "display: inline-block, vertical-align:top;", 
                       column(12,
                              selectInput("estimationEx", "What would you like to estimate?",
                                          choices = list("Power" = "power",
                                                         "Minimum detectable effect size" = "mdes",
                                                         "Sample size" = "sample"),
                                          selected = "power")
                              
                       )) # User Mode of Exploration
                   
                 ), # picking the type of exploration you would like to run
                 
                 fluidRow(
                   
                   div(style = "display: inline-block, vertical-align:top;", 
                       column(12,
                              selectInput("designEx", "What research design is this for?", 
                                          choices = list("Design: 1 level, Randomization: level 1 - Constant effects" = "d1.1_m2cc",
                                                         "Design: 2 levels, Randomization: level 1 - Constant effects" = "d2.1_m2fc", 
                                                         "Design: 2 levels, Randomization: level 1 - Fixed effects" = "d2.1_m2ff", 
                                                         "Design: 2 levels, Randomization: level 1 - Random effects" = "d2.1_m2fr",
                                                         "Design: 3 levels, Randomization: level 1 - Random effects" = "d3.1_m3rr2rr",
                                                         "Design: 2 levels, Randomization: level 2 - Random effects" = "d2.2_m2rc",
                                                         "Design: 3 levels, Randomization: level 3 - Random effects" = "d3.3_m3rc2rc",
                                                         "Design: 3 levels, Randomization: level 2 - Fixed effects" = "d3.2_m3ff2rc",
                                                         "Design: 3 levels, Randomization: level 2 - Random effects" = "d3.2_m3rr2rc"
                                          ),
                                          selected = "d2.1_m2ff")     
                              
                       )) # selecting designs
                   
                 ), # picking the research design
          
                 fluidRow(
                   
                   div(style = "display: inline-block, vertical-align:top;",
                       column(12,
                              numericInput("numOutcomesEx", "Number of Outcomes", 
                                           min = 1, 
                                           max = 10, 
                                           value = 5, 
                                           step = 1)
                       ) # number of Outcomes    
                   ) # div
                   
                 ), # number of outcomes selection 
                 
                 # UI/UX input for variables to vary
                 fluidRow(
                   
                   column(12,
                          uiOutput("varVaryEx"))
                   
                 ), # drop down for variable to vary by
                 
                 fluidRow(
                   
                   column(6,
                          uiOutput("nbarEx")),
                   
                   column(6,
                          uiOutput("jEx"))
                   
                 ), # Units per block and number of blocks  
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("mtpEx"))
                   
                 ), # MTP shared by all designs
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("mdesEx"))
                   
                 ), # Minimum Detectable Effect Size
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("rhoEx"))
                   
                 ), # correlation between outcomes
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("numCovar.1Ex"))
                   
                 ), # number of covariates in level 1
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("tbarEx"))
                   
                 ), # proportion of treatment assignment
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("alphaEx"))
                   
                 ), # alpha
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("r2.1Ex"))
                   
                 ), # Adding R2.1
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("icc.2Ex"))
                   
                 ), # Adding icc.2
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("r2.2Ex"))
                   
                 ), # Adding R2.2
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("omega.2Ex"))
                   
                 ), # Adding omega.2
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("r2.3Ex")      
                   )
                 ), # Adding r2.3
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("icc.3Ex"))
                   
                 ), # Adding icc.3
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("kEx"))
                   
                 ), # Adding k
                 
                 fluidRow(
                   
                   column(12,
                          uiOutput("omega.3Ex"))
                   
                 ), # Adding omega.3
                 
                 fluidRow(
                   
                   column(6,
                          actionButton("goButtonEx", "Go!")) 
                   
                 ) # Action Button to trigger other reactive values
               ), # Power calculation sidebarPanel
               
               mainPanel(
                 # Put some text here #
                 fluidRow(
                   
                   column(12, align = "center",
                          offset = 2,
                          #STh here for text
                   )
                 ),
                 
                 br(),    
                 br(),
                 
                 fluidRow(
                   
                   column(8, align = "center",
                          offset = 2,
                          plotlyOutput("powercalcGraphP2LBIEX"))
                   
                 ), # end of Fluid Row
                 
                 br(), # To create spaces between Table and Plots
                 br(), # To create spaces between Table and Plots
                 br(), # To create spaces between Table and Plots
                 br(), # To create spaces between Table and Plots
                 br(), # To create spaces between Table and Plots
                 br(), # To create spaces between Table and Plots
                 
                 fluidRow(
                   
                   column(12, align = "center",
                          DT::dataTableOutput("powercalcTableP2LBIEX")) #The power calculation table output
                   
                 ) #fluidRow for first half of the page
                 
               ) # Power calculation Main Panel
             ) # Power Calculation sidebar Layout
             
    ) # Explorer tab
             
  ) # End of main tabset Panel
) # end of Fluid Page


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session = FALSE) {
  
  # observeEvent(input$numOutcomesSs,{
  #   if(is.na(as.numeric(numOutcomesSs))){
  #     showFeedbackDanger(
  #       inputId = 'numOutcomesEx',
  #       text = 'Please input a number'
  #     )
  #   }else{
  #     hideFeedback('numOutcomesEx')
  #   }
  # })
  # 
  ##########################################################
  # Get reactive expression for tab
  ##########################################################

  # As it is a tab, I have to use observeEvent to see whose has actually clicked the Tab.
  
  whichTab <- reactiveValues()
  
  observeEvent(input$tabset, {
    
    if(input$tabset == "explorer_tab"){
      
      print("We are outputting explorer tab")
      whichTab$scenario<- "explorer_tab"
    } # Grabbing the home tab
     
    if(input$tabset == "single_scenario_tab"){

      print("We are outputting single scenario tab")
      whichTab$scenario <- "single_scenario_tab"
    } # Grabbing the Single Explorer tab identity to be passed into function

})
  
  ##########################################################
  # Get reactive expression for single       #
  ##########################################################
  
  getNumOutcomes <- reactive({
    
    if(input$tabset == "single_scenario_tab"){
      input$numOutcomesSs 
    }  
    
    if(input$tabset == "explorer_tab"){
      input$numOutcomesEx
    }
    
  })
  
  
  ######################################################################
  # Get reactive expression for what type of estimation we are running #
  ######################################################################
  
  getEstimationSs <- reactive({
    
    if(input$estimationSs == "power"){
      
      print("power")
      return(c("power"))
    }
  
    if(input$estimationSs == "mdes"){
      
      print("mdes")
      return(c("mdes"))
      
    }  
    
    if(input$estimationSs == "sample"){
      
      print("sample")
      return(c("sample"))
      
    }
    
  })
  
  getEstimationEx <- reactive({
    
    if(input$estimationEx == "power"){
      
      print("power")
      return(c("power"))
    }
    
    if(input$estimationEx == "mdes"){
      
      print("mdes")
      return(c("mdes"))
      
    }  
    
    if(input$estimationEx == "sample"){
      
      print("sample")
      return(c("sample"))
      
    }
    
  })
  
  ##########################################################
  # Get reactive expression for experimental chosen design #
  ##########################################################
  
  getDesignSs <- reactive({
    
    if(input$designSs == "d1.1_m2cc"){
     
      print("d1.1_m2cc")
      return(c("d1.1_m2cc"))
    }
    
    if(input$designSs == "d2.1_m2fc"){
      
      print("d2.1_m2fc")
      return(c("d2.1m2cc"))
    }
    
    if(input$designSs == "d2.1_m2ff") {
      
      print("d2.1_m2ff")
      return(c("d2.1_m2ff"))}
      
    if(input$designSs == "d2.1_m2fr"){
      
      print("d2.1_m2fr")
      return(c("d2.1_m2fr"))}
    
    if(input$designSs == "d3.1_m3rr2rr"){
      
      print("d3.1_m3rr2rr")
      return(c("d3.1_m3rr2rr"))}
    
    if(input$designSs == "d2.2_m2rc"){
      
      print("d2.2_m2rc")
      return(c("d2.2_m2rc"))}
    
    if(input$designSs == "d3.3_m3rc2rc"){
      
      print("d3.3_m3rc2rc")
      return(c("d3.3_m3rc2rc"))}
    
    if(input$designSs == "d3.2_m3ff2rc"){
      
      print("d3.2_m3ff2rc")
      return(c("d3.2_m3ff2rc"))}
    
    if(input$designSs == "d3.2_m3rr2rc"){
      
      print("d3.2_m3rr2rc")
      return(c("d3.2_m3rr2rc"))}
    
  }) # getDesignSs

  getDesignEx <- reactive({
    
    if(input$designEx == "d1.1_m2cc"){
      
      print("d1.1_m2cc")
      return(c("d1.1_m2cc"))
    }
    
    if(input$designEx == "d2.1_m2fc"){
      
      print("d2.1_m2fc")
      return(c("d2.1_m2fc"))
    }
    
    if(input$designEx == "d2.1_m2ff") {
      
      print("d2.1_m2ff")
      return(c("d2.1_m2ff"))}
    
    if(input$designEx == "d2.1_m2fr"){
      
      print("d2.1_m2fr")
      return(c("d2.1_m2fr"))}
    
    if(input$designEx == "d3.1_m3rr2rr"){
      
      print("d3.1_m3rr2rr")
      return(c("d3.1_m3rr2rr"))}
    
    if(input$designEx == "d2.2_m2rc"){
      
      print("d2.2_m2rc")
      return(c("d2.2_m2rc"))}
    
    if(input$designEx == "d3.3_m3rc2rc"){
      
      print("d3.3_m3rc2rc")
      return(c("d3.3_m3rc2rc"))}
    
    if(input$designEx == "d3.2_m3ff2rc"){
      
      print("d3.2_m3ff2rc")
      return(c("d3.2_m3ff2rc"))}
    
    if(input$designEx == "d3.2_m3rr2rc"){
      
      print("d3.2_m3rr2rc")
      return(c("d3.2_m3rr2rc"))}
    
  }) # getDesignSs
  
  ###########################################################
  # Get which variables to vary
  ###########################################################
  
  getVarVaryEx <- reactive({
    
    theEstimation = as.character(getEstimationEx())
    theDesign = as.character(getDesignEx())
    theScenario = as.character(whichTab$scenario)
    id <- paste0(theEstimation, "_", "varVary", "_", theDesign, "_" , theScenario)
    
    if(!is.null(input[[id]])){

      if(input[[id]] == "mtp"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("mtp"))    
      }
      
      if(input[[id]] == "mdes"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("mdes"))    
      }
      
      if(input[[id]] == "rho"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("rho"))    
      }
      
      if(input[[id]] == "numCovar.1"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("numCovar.1"))    
      }
      
      if(input[[id]] == "tbar"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("tbar"))    
      }
      
      if(input[[id]] == "alpha"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("alpha"))    
      }
      
      if(input[[id]] == "r2.1"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("r2.1"))    
      }
      
      if(input[[id]] == "icc.2"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("icc.2"))    
      }
      
      if(input[[id]] == "r2.2"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("icc.2"))    
      }
      
      if(input[[id]] == "omega.2"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("omega.2"))    
      }
      
      if(input[[id]] == "r2.3"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("r2.3"))    
      }
      
      if(input[[id]] == "icc.3"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("icc.3"))    
      }
      
      if(input[[id]] == "k"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("k"))    
      }
      
      if(input[[id]] == "omega.3"){
        
        print(paste0("variable to vary is ", input[[id]]))  
        return(c("omega.3"))    
      }
    } else {
      
      print("Setting a default rho variable to vary.")
      return(c("rho"))
      
    }
    
  }) # which variable to vary
  
  ######################################################
  # Rendering Variable Objects to UI for chosen design #
  ######################################################
  
  output$varVaryEx <- renderUI({
    
    theEstimation = as.character(getEstimationEx())
    theDesign = as.character(getDesignEx())
    theScenario = as.character(whichTab$scenario)
    
    div(style = "display: inline-block, vertical-align:top;", 
        varVaryInputEx(estimation = theEstimation, design = theDesign , scenario = theScenario))    
    
  }) # variable to vary by
  
  output$nbar <- renderUI({
    
    theEstimation = as.character(getEstimationSs())
    theDesign = as.character(getDesignSs())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top;", 
        nbarInput(estimation = theEstimation, design = theDesign , scenario = theScenario))    
    
  }) # number of units per block

  output$nbarEx <- renderUI({
    
    theEstimation = as.character(getEstimationEx())
    theDesign = as.character(getDesignEx())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top;", 
        nbarInputEx(estimation = theEstimation, design = theDesign , scenario = theScenario))    
    
  }) # number of units per block
  
  
  output$j <- renderUI({
    
    theEstimation = as.character(getEstimationSs())
    theDesign = as.character(getDesignSs())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top;", 
        jInput(estimation = theEstimation, design = theDesign , scenario = theScenario))    
    
  }) # number of units per block
  
  output$jEx <- renderUI({
    
    theEstimation = as.character(getEstimationEx())
    theDesign = as.character(getDesignEx())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top;", 
        jInputEx(estimation = theEstimation, design = theDesign , scenario = theScenario))    
    
  }) # number of units per block
  
  output$mtp <- renderUI({
    
    theEstimation = as.character(getEstimationSs())
    theDesign = as.character(getDesignSs())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top;", 
        mtpInput(estimation = theEstimation, design = theDesign , scenario = theScenario))
    
  }) # MTP for chosen design
  
  output$mtpEx <- renderUI({
    
    theEstimation = as.character(getEstimationEx())
    theDesign = as.character(getDesignEx())
    theScenario = as.character(whichTab$scenario)
    theVarVary = as.character(getVarVaryEx())
    div(style = "display: inline-block, vertical-align:top;", 
        mtpInputEx(estimation = theEstimation, design = theDesign , scenario = theScenario, varVary = theVarVary))
    
  }) # MTP for chosen design
  
  output$m <- renderUI({
    
    theEstimation = as.character(getEstimationSs())
    theDesign = as.character(getDesignSs())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top;", 
        mInput(estimation = theEstimation, design = theDesign, scenario = theScenario))
  }) # Number of Outcomes 
  
  output$mEx <- renderUI({
    
    theEstimation = as.character(getEstimationEx())
    theDesign = as.character(getDesignEx())
    theScenario = as.character(whichTab$scenario)
    theVarVary = as.character(getVarVaryEx())
    div(style = "display: inline-block, vertical-align:top;", 
        mInputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, varVary = theVarVary))
  }) # Number of Outcomes 

  output$mdes <- renderUI({
    
    theEstimation = as.character(getEstimationSs())
    theDesign = as.character(getDesignSs())
    theScenario = as.character(whichTab$scenario)
    theNumOutcomes = as.numeric(getNumOutcomes())
    
    div(style = "display: inline-block, vertical-align:top:",
        mdesInput(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
  }) # Minimum detectable effect size
  
  output$mdesEx <- renderUI({
    
    theEstimation = as.character(getEstimationEx())
    theDesign = as.character(getDesignEx())
    theScenario = as.character(whichTab$scenario)
    theNumOutcomes = as.numeric(getNumOutcomes())
    theVarVary = as.character(getVarVaryEx())
    
    div(style = "display: inline-block, vertical-align:top:",
        mdesInputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes, varVary = theVarVary))
  }) # Minimum detectable effect size
  
  output$rho <- renderUI({
    
    theEstimation = as.character(getEstimationSs())
    theDesign = as.character(getDesignSs())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top:",
        rhoInput(estimation = theEstimation, design = theDesign, scenario = theScenario))
        
  }) # correlation between test statistics
  
  output$rhoEx <- renderUI({
    
    theEstimation = as.character(getEstimationEx())
    theDesign = as.character(getDesignEx())
    theScenario = as.character(whichTab$scenario)
    theVarVary = as.character(getVarVaryEx())
    div(style = "display: inline-block, vertical-align:top:",
        rhoInputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, varVary = theVarVary))
    
  }) # correlation between test statistics
  
  output$numCovar.1 <- renderUI({
  
    theEstimation = as.character(getEstimationSs())
    theDesign = as.character(getDesignSs())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top:",
        numCovar.1Input(estimation = theEstimation, design = theDesign, scenario = theScenario))
    
  }) # number of level 1 covariates
  
  output$numCovar.1Ex <- renderUI({
    
    theEstimation = as.character(getEstimationEx())
    theDesign = as.character(getDesignEx())
    theScenario = as.character(whichTab$scenario)
    theVarVary = as.character(getVarVaryEx())
    div(style = "display: inline-block, vertical-align:top:",
        numCovar.1InputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, varVary = theVarVary))
    
  }) # number of level 1 covariates
  
  output$tbar <- renderUI({
    
    theEstimation = as.character(getEstimationSs())
    theDesign = as.character(getDesignSs())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top:",
        tbarInput(estimation = theEstimation, design = theDesign, scenario = theScenario))
    
  }) # Proportion of treatment assignment
  
  output$tbarEx <- renderUI({
    
    theEstimation = as.character(getEstimationEx())
    theDesign = as.character(getDesignEx())
    theScenario = as.character(whichTab$scenario)
    theVarVary = as.character(getVarVaryEx())
    div(style = "display: inline-block, vertical-align:top:",
        tbarInputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, varVary = theVarVary))
    
  }) # Proportion of treatment assignment
  
  output$alpha <- renderUI({
    
    theEstimation = as.character(getEstimationSs())
    theDesign = as.character(getDesignSs())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top:",
        alphaInput(estimation = theEstimation, design = theDesign, scenario = theScenario))
    
  }) # Significance level (alpha)
  
  output$alphaEx <- renderUI({
    
    theEstimation = as.character(getEstimationEx())
    theDesign = as.character(getDesignEx())
    theScenario = as.character(whichTab$scenario)
    theVarVary = as.character(getVarVaryEx())
    div(style = "display: inline-block, vertical-align:top:",
        alphaInputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, varVary = theVarVary))
    
  }) # Significance level (alpha)

  output$r2.1 <- renderUI({
    
    theEstimation = as.character(getEstimationSs())
    theDesign = as.character(getDesignSs())
    theScenario = as.character(whichTab$scenario)
    theNumOutcomes = as.numeric(getNumOutcomes())
    
    
    div(style = "display: inline-block, vertical-align:top;", 
        r2.1Input(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
  
  }) # R2.1 element for chosen and required designs
  
  output$r2.1Ex <- renderUI({
    
    theEstimation = as.character(getEstimationEx())
    theDesign = as.character(getDesignEx())
    theScenario = as.character(whichTab$scenario)
    theNumOutcomes = as.numeric(getNumOutcomes())
    theVarVary = as.character(getVarVaryEx())
    
    #browser()
    div(style = "display: inline-block, vertical-align:top;", 
        r2.1InputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes, varVary = theVarVary))
    
  }) # R2.1 element for chosen and required designs
  
  output$r2.2 <- renderUI({
    
    req(input$designSs)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designSs == 'd2.2_m2rc' || input$designSs == 'd3.3_m3rc2rc'||
       input$designSs == 'd3.2_m3ff2rc' || input$designSs == 'd3.2_m3rr2rc'){
      print('Design r2-2 Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationSs())
      theDesign = as.character(getDesignSs())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      
      div(style = "display: inline-block, vertical-align:top;", 
          r2.2Input(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
    }else{
      # leave blank otherwise
    }
    
  }) # R2.2 element for chosen and required designs
  
  output$r2.2Ex <- renderUI({
    
    req(input$designEx)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designEx == 'd2.2_m2rc' || input$designEx == 'd3.3_m3rc2rc'||
       input$designEx == 'd3.2_m3ff2rc' || input$designEx == 'd3.2_m3rr2rc'){
      print('Design r2-2 Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationEx())
      theDesign = as.character(getDesignEx())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      theVarVary = as.character(getVarVaryEx())
      
      div(style = "display: inline-block, vertical-align:top;", 
          r2.2InputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes, varVary = theVarVary))
    }else{
      # leave blank otherwise
    }
    
  }) # R2.2 element for chosen and required designs
  
  
  output$icc.2 <- renderUI({
    
    req(input$designSs)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designSs == 'd2.1_m2fc' || input$designSs == 'd2.1_m2ff' || input$designSs == 'd2.1_m2fr' ||
       input$designSs == 'd3.1_m3rr2rr' || input$designSs == 'd2.2_m2rc' || input$designSs == 'd3.3_m3rc2rc' ||
       input$designSs == 'd3.2_m3ff2rc' || input$designSs == 'd3.2_m3rr2rc'){
      print('Design ICC2 Trigger')
      check = TRUE
    }
    

    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationSs())
      theDesign = as.character(getDesignSs())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      
      div(style = "display: inline-block, vertical-align:top;", 
          icc.2Input(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
    }else{
      # leave blank otherwise
    }

  }) # icc.2 element for chosen and required designs
  
  output$icc.2Ex <- renderUI({
    
    req(input$designEx)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designEx == 'd2.1_m2fc' || input$designEx == 'd2.1_m2ff' || input$designEx == 'd2.1_m2fr' ||
       input$designEx == 'd3.1_m3rr2rr' || input$designEx == 'd2.2_m2rc' || input$designEx == 'd3.3_m3rc2rc' ||
       input$designEx == 'd3.2_m3ff2rc' || input$designEx == 'd3.2_m3rr2rc'){
      print('Design ICC2 Trigger')
      check = TRUE
    }
    
    
    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationEx())
      theDesign = as.character(getDesignSs())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      theVarVary = as.character(getVarVaryEx())
      
      div(style = "display: inline-block, vertical-align:top;", 
          icc.2InputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes, varVary = theVarVary))
    }else{
      # leave blank otherwise
    }
    
  }) # icc.2 element for chosen and required designs
  
  output$omega.2 <- renderUI({
  
    req(input$designSs)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designSs == 'd2.1_m2fr' || input$designSs == 'd3.1_m3rr2rr'){
      print('Design omega 2 Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationSs())
      theDesign = as.character(getDesignSs())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      
      div(style = "display: inline-block, vertical-align:top;", 
          omega.2Input(design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
    }else{
      # leave blank otherwise
    }
    
  }) # omega.2 element for chosen and required designs
  
  output$omega.2Ex <- renderUI({
    
    req(input$designEx)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designEx == 'd2.1_m2fr' || input$designEx == 'd3.1_m3rr2rr'){
      print('Design omega 2 Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationEx())
      theDesign = as.character(getDesignEx())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      theVarVary = as.character(getVarVaryEx())
      
      div(style = "display: inline-block, vertical-align:top;", 
          omega.2InputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes, varVary = theVarVary))
    }else{
      # leave blank otherwise
    }
    
  }) # omega.2 element for chosen and required designs
  
  output$r2.3 <- renderUI({
    
    #   conditionalPanel(condition = c("input.design == 'd3.3_m3rc2rc'"),
    
    req(input$designSs)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designSs == 'd3.3_m3rc2rc'){
      print('Design r2-3 Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationSs())
      theDesign = as.character(getDesignSs())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      
      div(style = "display: inline-block, vertical-align:top;", 
          r2.3Input(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
    }else{
      # leave blank otherwise
    }

  }) # R2.3 element for chosen and required designs
  
  output$r2.3Ex <- renderUI({
    
    #   conditionalPanel(condition = c("input.design == 'd3.3_m3rc2rc'"),
    
    req(input$designEx)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designEx == 'd3.3_m3rc2rc'){
      print('Design r2-3 Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationEx())
      theDesign = as.character(getDesignEx())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      theVarVary = as.character(getVarVaryEx())
      
      div(style = "display: inline-block, vertical-align:top;", 
          r2.3InputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes, varVary = theVarVary))
    }else{
      # leave blank otherwise
    }
    
  }) # R2.3 element for chosen and required designs
  
  output$icc.3 <- renderUI({
  
    req(input$designSs)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designSs == 'd3.1_m3rr2rr' || input$designSs == 'd3.3_m3rc2rc'||
       input$designSs == 'd3.2_m3ff2rc' || input$designSs == 'd3.2_m3rr2rc'){
      print('Design icc.3 Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationSs())
      theDesign = as.character(getDesignSs())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      
      div(style = "display: inline-block, vertical-align:top;", 
          icc.3Input(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
    }else{
      # leave blank otherwise
    }    
    
  }) # icc.3 element for chosen and required designs

  output$icc.3Ex <- renderUI({
    
    req(input$designEx)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designEx == 'd3.1_m3rr2rr' || input$designEx == 'd3.3_m3rc2rc'||
       input$designEx == 'd3.2_m3ff2rc' || input$designEx == 'd3.2_m3rr2rc'){
      print('Design icc.3 Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationEx())
      theDesign = as.character(getDesignEx())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      theVarVary = as.character(getVarVaryEx())
      
      div(style = "display: inline-block, vertical-align:top;", 
          icc.3InputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes, varVary = theVarVary))
    }else{
      # leave blank otherwise
    }    
    
  }) # icc.3 element for chosen and required designs
  
  output$omega.3 <- renderUI({
    
    # conditionalPanel(condition = c("input.design == 'd3.1_m3rr2rr' || input.design == 'd3.2_m3rr2rc'"),
    req(input$designSs)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designSs == 'd3.1_m3rr2rr' || input$designSs == 'd3.2_m3rr2rc'){
      print('Design omega.3 Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationSs())
      theDesign = as.character(getDesignSs())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      
      div(style = "display: inline-block, vertical-align:top;", 
          omega.3Input(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
    }else{
      # leave blank otherwise
    }
    
  }) # omega.3 element for chosen and required designs
  
  output$omega.3Ex <- renderUI({
    
    # conditionalPanel(condition = c("input.design == 'd3.1_m3rr2rr' || input.design == 'd3.2_m3rr2rc'"),
    req(input$designEx)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designEx == 'd3.1_m3rr2rr' || input$designEx == 'd3.2_m3rr2rc'){
      print('Design omega.3 Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationEx())
      theDesign = as.character(getDesignEx())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      theVarVary = as.character(getVarVaryEx())
      
      div(style = "display: inline-block, vertical-align:top;", 
          omega.3InputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes, varVary = theVarVary))
    }else{
      # leave blank otherwise
    }
    
  }) # omega.3 element for chosen and required designs
  
  output$k <- renderUI({
    
    req(input$designSs)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designSs == 'd3.1_m3rr2rr' || input$designSs == 'd3.3_m3rc2rc'||
       input$designSs == 'd3.2_m3ff2rc' || input$designSs == 'd3.2_m3rr2rc'){
      print('Design k Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationSs())
      theDesign = as.character(getDesignSs())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      
      div(style = "display: inline-block, vertical-align:top;", 
          kInput(estimation = theEstimation, design = theDesign, scenario = theScenario))
    }else{
      # leave blank otherwise
    }    
  }) # number of level-3 groups
  
  output$kEx <- renderUI({
    
    req(input$designEx)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$designEx == 'd3.1_m3rr2rr' || input$designEx == 'd3.3_m3rc2rc'||
       input$designEx == 'd3.2_m3ff2rc' || input$designEx == 'd3.2_m3rr2rc'){
      print('Design k Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      
      theEstimation = as.character(getEstimationEx())
      theDesign = as.character(getDesignEx())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      theVarVary = as.character(getVarVaryEx())
      
      div(style = "display: inline-block, vertical-align:top;", 
          kInputEx(estimation = theEstimation, design = theDesign, scenario = theScenario, varVary = theVarVary))
    }else{
      # leave blank otherwise
    }    
  }) # number of level-3 groups

  observeEvent(input$goButtonSs,{
    
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
    
    ############################################################################
    # Generating Power Results for diffferent designs & mode of exploration    #
    ############################################################################
    
    # Getting the research design that we have to estimate the statistical results for
    
    theEstimation = as.character(getEstimationSs())
    theDesign = as.character(getDesignSs())
    theScenario = as.character(whichTab$scenario)
    
    # set up to receive all the input parameters
    
    # Get string for input subsetting
    design_subset <- "designSs"
    m_subset <- "numOutcomesSs"
    nbar_subset <- paste0(theEstimation, "_", "nbar", "_", theDesign)
    j_subset <- paste0(theEstimation, "_", "j", "_", theDesign)
    mtp_subset <- paste0(theEstimation, "_", "mtp", "_", theDesign,"_", theScenario)
    mdes_subset <- paste0(theEstimation, "_", "mdes", "_", theDesign, "_", theScenario)
    rho_subset <- paste0(theEstimation, "_", "rho", "_", theDesign, "_", theScenario)
    numCovar.1_subset <- paste0(theEstimation, "_", "numCovar.1","_", theDesign, "_", theScenario)
    tbar_subset <- paste0(theEstimation, "_", "tbar","_", theDesign, "_", theScenario)
    alpha_subset <- paste0(theEstimation, "_", "alpha", "_", theDesign, "_", theScenario)
    r2.1_subset <- paste0(theEstimation, "_", "r2.1", "_", theDesign, "_", theScenario)
    r2.2_subset <- paste0(theEstimation, "_", "r2.2", "_", theDesign, "_", theScenario)
    icc.2_subset <- paste0(theEstimation, "_", "icc.2", "_", theDesign, "_", theScenario)
    omega.2_subset <- paste0(theEstimation, "_", "omega.2", "_", theDesign, "_", theScenario)
    r2.3_subset <- paste0(theEstimation, "_", "r2.3", "_", theDesign, "_", theScenario)
    icc.3_subset <- paste0(theEstimation, "_", "icc.3", "_", theDesign, "_", theScenario)
    k_subset <- paste0(theEstimation, "_", "k","_", theDesign, "_", theScenario)
    omega.3_subset <- paste0(theEstimation, "_", "omega.3", "_", theDesign, "_", theScenario)
    
    # Pulling in values for all the designs
    design <- input[[design_subset]]
    nbar <- input[[nbar_subset]]
    j <- input[[j_subset]]
    mtp <- input[[mtp_subset]]
    m <- input[[m_subset]]
    mdes <- input[[mdes_subset]]
    rho <- input[[rho_subset]]
    numCovar.1 <- input[[numCovar.1_subset]]
    tbar <- input[[tbar_subset]]
    alpha <- input[[alpha_subset]]
    r2.1 <- input[[r2.1_subset]]
    icc.2 <- "0"
    r2.2 <- "0"
    omega.2 <- "0"
    r2.3 <- "0"
    icc.3 <- "0"
    k <- 1
    omega.3 <- "0"
    
    if(design %in% c("d2.2_m2rc", "d3.3_m3rc2rc", "d3.2_m3ff2rc", "d3.2m3rr2rc")){
      
      r2.2 <- input[[r2.2_subset]]
      
    } # r2.2
    
    if(design %in% c("d2.1_m2fc" , "d2.1_m2ff" , "d2.1_m2fr" , "d2.2_m2rc" , "d3.1_m3rr2rr" , "d3.3_m3rc2rc" , "d3.2_m3ff2rc" , "d3.2_m3rr2rc")){
    
      icc.2 <- input[[icc.2_subset]]
      
    } 
    
    if (design %in% c("d2.1_m2fr", 'd3.1_m3rr2rr')){
      
      omega.2 <- input[[omega.2_subset]]
      
    } 
    
    if (design %in% c("d3.3_m3rc2rc")){
      
      r2.3 <- input[[r2.3_subset]]
      
    }
    
    if (design %in% c("d3.1_m3rr2rr", "d3.3_m3rc2rc", "d3.2_m3ff2rc", "d3.2_m3rr2rc")) {
      
      icc.3 <- input[[icc.3_subset]]
      
    }
    
    if (design %in% c("d3.1_m3rr2rr", "d3.3_m3rc2rc", "d3.2_m3ff2rc", "d3.2_m3rr2rc")){
      
      k <- input[[k_subset]]
      
    }
    
    if (design %in% c("d3.1_m3rr2rr", "d3.2_m3rr2rc")) {
      
      omega.3 <- input[[omega.3_subset]]
      
    }
    
    dat <- as.data.frame(
        isolate(pum::pump_power(design = design,
                                nbar = nbar, # The number of units per block
                                J = j, # The number of schools
                                K = k, # 3 level grouping variable count
                                MTP = as.character(unlist(strsplit(mtp, ","))),
                                M = m, # The number of hypotheses/outcomes
                                MDES = as.numeric(unlist(strsplit(mdes, ","))),
                                rho = rho,
                                numCovar.1 = numCovar.1,
                                Tbar = tbar, # The proportion of samples that are assigned to the treatment
                                alpha = alpha,
                                R2.1 = as.numeric(unlist(strsplit(r2.1, ","))),
                                R2.2 = as.numeric(unlist(strsplit(r2.2, ","))),
                                R2.3 = as.numeric(unlist(strsplit(r2.3, ","))), 
                                ICC.2 = as.numeric(unlist(strsplit(icc.2, ","))),
                                ICC.3 = as.numeric(unlist(strsplit(icc.3, ","))),
                                omega.2 = as.numeric(unlist(strsplit(omega.2, ","))),
                                omega.3 = as.numeric(unlist(strsplit(omega.3, ","))),
                                tnum = 10000,
                                B = 100,
                                cl = NULL,
                                updateProgress = updateProgress)
                
       )) #Power generation table
      

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
                axis.text = element_text(size = 14, angle = 45),
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
  

  observeEvent(input$goButtonEx,{
    
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
    
    ############################################################################
    # Generating Power Results for diffferent designs & mode of exploration    #
    ############################################################################
    
    # Getting the research design that we have to estimate the statistical results for
    
    theEstimation = as.character(getEstimationSs())
    theDesign = as.character(getDesignSs())
    theScenario = as.character(whichTab$scenario)
    theVarVary = as.character(getVarVaryEx())
    
    # set up to receive all the input parameters
    
    # Get string for input subsetting
    design_subset <- "designEx"
    m_subset <- "numOutcomesEx"
    nbar_subset <- paste0(theEstimation, "_", "nbar", "_", theDesign, "_", theScenario)
    j_subset <- paste0(theEstimation, "_", "j", "_", theDesign, "_", theScenario)
    mtp_subset <- paste0(theEstimation, "_", "mtp", "_", theDesign,"_", theScenario, "_", theVarVary)
    mdes_subset <- paste0(theEstimation, "_", "mdes", "_", theDesign, "_", theScenario, "_", theVarVary)
    rho_subset <- paste0(theEstimation, "_", "rho", "_", theDesign, "_", theScenario, "_", theVarVary)
    numCovar.1_subset <- paste0(theEstimation, "_", "numCovar.1","_", theDesign, "_", theScenario, "_", theVarVary)
    tbar_subset <- paste0(theEstimation, "_", "tbar","_", theDesign, "_", theScenario, "_", theVarVary)
    alpha_subset <- paste0(theEstimation, "_", "alpha", "_", theDesign, "_", theScenario, "_", theVarVary)
    r2.1_subset <- paste0(theEstimation, "_", "r2.1", "_", theDesign, "_", theScenario, "_", theVarVary)
    r2.2_subset <- paste0(theEstimation, "_", "r2.2", "_", theDesign, "_", theScenario, "_", theVarVary)
    icc.2_subset <- paste0(theEstimation, "_", "icc.2", "_", theDesign, "_", theScenario, "_", theVarVary)
    omega.2_subset <- paste0(theEstimation, "_", "omega.2", "_", theDesign, "_", theScenario, "_", theVarVary)
    r2.3_subset <- paste0(theEstimation, "_", "r2.3", "_", theDesign, "_", theScenario, "_", theVarVary)
    icc.3_subset <- paste0(theEstimation, "_", "icc.3", "_", theDesign, "_", theScenario, "_", theVarVary)
    k_subset <- paste0(theEstimation, "_", "k","_", theDesign, "_", theScenario, "_", theVarVary)
    omega.3_subset <- paste0(theEstimation, "_", "omega.3", "_", theDesign, "_", theScenario, "_", theVarVary)
    
    # Pulling in values for all the designs
    design <- input[[design_subset]]
    nbar <- input[[nbar_subset]]
    j <- input[[j_subset]]
    mtp <- input[[mtp_subset]]
    m <- input[[m_subset]]
    mdes <- input[[mdes_subset]]
    rho <- input[[rho_subset]]
    numCovar.1 <- input[[numCovar.1_subset]]
    tbar <- input[[tbar_subset]]
    alpha <- input[[alpha_subset]]
    r2.1 <- input[[r2.1_subset]]
    icc.2 <- "0"
    r2.2 <- "0"
    omega.2 <- "0"
    r2.3 <- "0"
    icc.3 <- "0"
    k <- 1
    omega.3 <- "0"
    
    if(design %in% c("d2.2_m2rc", "d3.3_m3rc2rc", "d3.2_m3ff2rc", "d3.2m3rr2rc")){
      
      r2.2 <- input[[r2.2_subset]]
      
    } # r2.2
    
    if(design %in% c("d2.1_m2fc" , "d2.1_m2ff" , "d2.1_m2fr" , "d2.2_m2rc" , "d3.1_m3rr2rr" , "d3.3_m3rc2rc" , "d3.2_m3ff2rc" , "d3.2_m3rr2rc")){
      
      icc.2 <- input[[icc.2_subset]]
      
    } 
    
    if (design %in% c("d2.1_m2fr", 'd3.1_m3rr2rr')){
      
      omega.2 <- input[[omega.2_subset]]
      
    } 
    
    if (design %in% c("d3.3_m3rc2rc")){
      
      r2.3 <- input[[r2.3_subset]]
      
    }
    
    if (design %in% c("d3.1_m3rr2rr", "d3.3_m3rc2rc", "d3.2_m3ff2rc", "d3.2_m3rr2rc")) {
      
      icc.3 <- input[[icc.3_subset]]
      
    }
    
    if (design %in% c("d3.1_m3rr2rr", "d3.3_m3rc2rc", "d3.2_m3ff2rc", "d3.2_m3rr2rc")){
      
      k <- input[[k_subset]]
      
    }
    
    if (design %in% c("d3.1_m3rr2rr", "d3.2_m3rr2rc")) {
      
      omega.3 <- input[[omega.3_subset]]
      
    }
    
    dat <- as.data.frame(
      isolate(pum::pump_power_grid(design = design,
                              nbar = nbar, # The number of units per block
                              J = j, # The number of schools
                              K = k, # 3 level grouping variable count
                              MTP = as.character(unlist(strsplit(mtp, ","))),
                              M = m, # The number of hypotheses/outcomes
                              MDES = as.numeric(unlist(strsplit(mdes, ","))),
                              rho = rho,
                              numCovar.1 = numCovar.1,
                              Tbar = tbar, # The proportion of samples that are assigned to the treatment
                              alpha = alpha,
                              R2.1 = as.numeric(unlist(strsplit(r2.1, ","))),
                              R2.2 = as.numeric(unlist(strsplit(r2.2, ","))),
                              R2.3 = as.numeric(unlist(strsplit(r2.3, ","))), 
                              ICC.2 = as.numeric(unlist(strsplit(icc.2, ","))),
                              ICC.3 = as.numeric(unlist(strsplit(icc.3, ","))),
                              omega.2 = as.numeric(unlist(strsplit(omega.2, ","))),
                              omega.3 = as.numeric(unlist(strsplit(omega.3, ","))),
                              long.table = TRUE,
                              tnum = 10000,
                              B = 100,
                              cl = NULL,
                              updateProgress = updateProgress)
              
      )) #Power generation table
    
    
    # Save the reactive Power Table
    reactPowerTable(dat)
    {reactPowerTable()}
    
    #################################################
    # Rendering Single Scenario Power Table Results #
    #################################################
    
    # Rendering a reactive object table from the power function
    
    #browser()
    output$powercalcTableP2LBIEX <- renderDataTable({
      
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
    # dat <- reactPowerTable()
    # 
    # singleScenario2LevelBlockedDatLong <- 
    #   dat %>%
    #   dplyr::select_all() %>%
    #   dplyr::select(-indiv.mean) %>%
    #   tidyr::pivot_longer(!MTP, names_to = "powerType", values_to = "power")
    
    #browser()
    #####################################
    # Preparing the data frame for Plot #
    #####################################
    
    ## Setting up outcomes for Color gradient
    
    ## Setting up outcomes for Color gradient
    
    # Grab the number of outcomes
    M <- input[[m_subset]]
    
    # End of Minimum Power 
    minEnd <- M - 1
    minPower <- paste0(1:minEnd, "-minimum")
    
    # Create color gradient for minimum power
    mincolours <- scales::seq_gradient_pal(low = "gray80", high = "gray30", space = "Lab")(1:minEnd/minEnd)
    mincolours <- sort(mincolours)
    
    # Add complete, individual, minimum and raw power colors
    allcolors <- c("#90ee90", "#ADD8E6", mincolours, "mediumpurple")
    
    # Pulling the generated data table
    dat <- reactPowerTable()
    dat <- as.data.frame(dat)
    
    if(theVarVary == "mdes"){
    # Adjusting the data table for graphing
      withoutIndivPower <- 
        dat %>%
        dplyr::select_all() %>%
        dplyr::select(-design) %>%
        dplyr::arrange(desc(power)) %>%
        dplyr::rename(powerType = power) %>%
        tidyr::pivot_longer(!c(MDES, powerType), names_to = "MTP", values_to = "power") %>%
        dplyr::filter(!stringr::str_detect(powerType,"individual outcome")) %>%
        dplyr::mutate(powerType = ifelse(MTP == "None",
                                         "raw mean individual",
                                         powerType)) 
      
      # converting Power Type to a factor for coloring
      withoutIndivPower$powerType <- factor(withoutIndivPower$powerType,
                                            levels = c("complete", "mean individual", minPower, "raw mean individual"),
                                            ordered = TRUE)
      
      # converting data type for graphing purposes
      withoutIndivPower <- withoutIndivPower %>%
        dplyr::mutate(MDES = as.factor(MDES),
                      power = as.numeric(power),
                      MTP = as.factor(MTP))
      
      # pulling out Power Type Levels to match with all colors
      powerTypeLevels <- levels(withoutIndivPower$powerType)
      
      # create value for scale color manual by matching color and Power Type
      allcolorsvalues <- setNames(allcolors, powerTypeLevels)
      
      # name of MTP
      MTPname <- levels(withoutIndivPower$MTP)[1]
      
      ######################
      # Plotting the graph #
      ######################
      
      output$powercalcGraphP2LBIEX <- renderPlotly({
        
        # Wrapping the ggplot with plotly
        
        pg <- 
          plotly::ggplotly(ggplot2::ggplot(
            data = withoutIndivPower,
            aes(x = MDES,
                y = power,
                colour = powerType)) +
              geom_point(size = 2, 
                         position = position_jitter(width = 0.2)) +
              scale_y_continuous(limits = c(0,1)) +
              ggtitle(paste0(MTPname, " adjusted Power values across different \n Power Definitions & MDES values")) +
              scale_colour_manual(values = allcolorsvalues) +
              labs(x = "Different MDES Scenarios",
                   y = "Power Values",
                   colour = "") +
              theme_linedraw() +
              theme(plot.title = element_text(size = 16,
                                              face = "bold",
                                              vjust = 1,
                                              hjust = 0.5)) 
          )
        
        # plotly adjustments for margin, centering and axis titles
        
        pg <- layout(pg, 
                     #title = "<b>Adjusted Power values across different \n Power Definitions & MDES values </b>",
                     margin=list(t = 75),
                     legend = list(x = 100,
                                   orientation = "v",
                                   xanchor = "center",
                                   y = 0.5,
                                   title = list(text = '<b> Power Type </b>')))
        
        # plotly configurations to suit ourpuposes
        
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
        
      }) # ggplot for power graph
    
    } else {
      
      browser()  
      
      
    } # mdes condition handling
    
      
}) # server side call end
  
}) # end of server side

# Run the application 
shinyApp(ui = ui, server = server)
