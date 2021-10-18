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
                              selectInput("scenario", "Pick a mode of exploration",
                                          choices = list("Single scenario" = "ss",
                                                         "Explorer" = "ex"))
                              
                            )) # User Mode of Exploration

                          ), # picking the type of exploration you would like to run
                          
                          fluidRow(
                            
                            div(style = "display: inline-block, vertical-align:top;", 
                            column(12,
                             selectInput("design", "What research design is this for?", 
                                        choices = list("One Level RCT - Constant Effects" = "d1.1_m2cc",
                                                       "Two Levels Blocked RCT - Constant Effects" = "d2.1_m2fc", 
                                                       "Two Levels Blocked RCT - Fixed Effects" = "d2.1_m2ff", 
                                                       "Two Levels Blocked RCT - Random Effects" = "d2.1_m2fr",
                                                       "Three Levels Clustered RCT - Random Effects" = "d3.1_m3rr2rr",
                                                       "Design 2 levels, Randomization: level 2 - Random Effects" = "d2.2_m2rc",
                                                       "Design 3 levels, Randomization: level 3 - Random Effects" = "d3.3_m3rc2rc",
                                                       "Design 3 levels, Randomization: level 2 - Fixed Effects" = "d3.2_m3ff2rc",
                                                       "Design 3 levels, Randomization: level 2 - Random Effects" = "d3.2_m3rr2rc"
                                                       ),
                                        selected = "d2.1_m2ff")     

                            )) # selecting designs
                            
                          ), # picking the research design
                        
                          fluidRow(
                            
                            div(style = "display: inline-block, vertical-align:top;",
                            column(12,
                              numericInput("numOutcomes", "Number of Outcomes", 
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

                           ),
                        
                        # 
                        #   
                        #    conditionalPanel(condition = c("input.design == 'd2.1_m2fr' || input.design == 'd3.1_m3rr2rr'"),
                        #                  
                        #     fluidRow(
                        #                    
                        #      column(12,
                        #      uiOutput("omega.2"))
                        #                    
                        #     ) # Adding omega.2 which does not exist for 2 level blocked RCT constant and fixed effects
                        #                  
                        #   ), # conditional Panel for omega.2
                        # 
                        #   conditionalPanel(condition = c("input.design == 'd3.3_m3rc2rc'"),
                        #     
                        #     fluidRow(
                        #       
                        #       column(12,
                        #       uiOutput("r2.3"))
                        #       
                        #     ) # Adding r2.3
                        #                    
                        #   ), # conditional Panel for r2.3
                        # 
                        #   conditionalPanel(condition = c("input.design == 'd3.1_m3rr2rr' || input.design == 'd3.3_m3rc2rc' || 
                        #                                   input.design == 'd3.2_m3ff2rc' || input.design == 'd3.2_m3rr2rc'"),
                        #                  
                        # 
                        #     fluidRow(
                        #       
                        #       column(12,
                        #       uiOutput("icc.3"))
                        #       
                        #     ), # adding icc.3 which does not exist for 3 level designs 
                        #     
                        #     fluidRow(
                        #       
                        #       column(12,
                        #       uiOutput("k"))
                        #       
                        #     ) # adding K for number of level 3 groupings
                        #      
                        # ), # conditional Panel for level 3 designs non-random effects at the 3rd level
                        # 
                        # conditionalPanel(condition = c("input.design == 'd3.1_m3rr2rr' || input.design == 'd3.2_m3rr2rc'"),
                        #                  
                        #                  fluidRow(
                        #                    
                        #                    column(12,
                        #                           uiOutput("omega.3"))
                        #                    
                        #                  ) # Adding omega.3 for random effect variation at the 3rd level
                        #                  
                        # ), # omega.3 for random effect variation at the 3rd level
     
                            fluidRow(
                              
                              column(6,
                              actionButton("goButtonP2LBISS", "Go!")) 
                            
                            ) # Action Button to trigger other reactive values
              ), # Power calculation sidebarPanel
                                       
              mainPanel(
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
    
    tabPanel(title = "Explorer", value = "explorer_tab") # Explorer tab
             
  ) # End of main tabset Panel
) # end of Fluid Page


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session = FALSE) {
  
  ##########################################################
  # Get reactive expression for tab
  ##########################################################

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
  # Get reactive expression for single or explorer         #
  ##########################################################
  
  getNumOutcomes <- reactive({
    
    input$numOutcomes 
    
  })
  
  ##########################################################
  # Get reactive expression for experimental chosen design #
  ##########################################################
  
  getDesign <- reactive({
    
    if(input$design == "d2.1_m2ff") {
      
      print("d2.1_m2ff")
      return(c("d2.1_m2ff"))}
      
    if(input$design == "d2.1_m2fr"){
      
      print("d2.1_m2fr")
      return(c("d2.1_m2fr"))}
      
    if(input$design == "d2.1_m2fc") {
      
      print("d2.1_m2fc")
      return(c("d2.1_m2fc"))
      
    }
  
  }) # getDesign

  ######################################################
  # Rendering Variable Objects to UI for chosen design #
  ######################################################
  
  output$design <- renderUI({
    
    div(style = "display: inline-block, vertical-align:top;", 
        designInput(x = "design"))    
        
  }) # number of units per block  

  ######################################################
  # Rendering Variable Objects to UI for chosen design #
  ######################################################
  
  output$nbar <- renderUI({
    
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top;", 
        nbarInput(design = theDesign , scenario = theScenario))    
    
  }) # number of units per block

  output$j <- renderUI({
    
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top;", 
        jInput(design = theDesign , scenario = theScenario))    
    
  }) # number of units per block
  
  output$mtp <- renderUI({
    
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top;", 
        mtpInput(design = theDesign , scenario = theScenario))
    
  }) # MTP for chosen design
  
  output$m <- renderUI({
    
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top;", 
        mInput(design = theDesign, scenario = theScenario))
  }) # Number of Outcomes 

  output$mdes <- renderUI({
    
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    theNumOutcomes = as.numeric(getNumOutcomes())
    
    div(style = "display: inline-block, vertical-align:top:",
        mdesInput(design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
  }) # Minimum detectable effect size
  
  output$rho <- renderUI({
    
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top:",
        rhoInput(design = theDesign, scenario = theScenario))
        
  }) # correlation between test statistics
  
  output$numCovar.1 <- renderUI({
  
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top:",
        numCovar.1Input(design = theDesign, scenario = theScenario))
    
  }) # number of level 1 covariates
  
  output$tbar <- renderUI({
    
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top:",
        tbarInput(design = theDesign, scenario = theScenario))
    
  }) # Proportion of treatment assignment
  
  output$alpha <- renderUI({
    
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    div(style = "display: inline-block, vertical-align:top:",
        alphaInput(design = theDesign, scenario = theScenario))
    
  }) # Significance level (alpha)

  output$r2.1 <- renderUI({
    
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    theNumOutcomes = as.numeric(getNumOutcomes())
    
    
    div(style = "display: inline-block, vertical-align:top;", 
        r2.1Input(design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
  
  }) # R2.1 element for chosen and required designs
  
  output$r2.2 <- renderUI({
    
    #  conditionalPanel(condition = c("input.design == 'd2.2_m2rc' || input.design == 'd3.3_m3rc2rc' ||
    #                                 input.design == 'd3.2_m3ff2rc' || input.design == 'd3.2_m3rr2rc'" ),
    
    req(input$design)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$design == 'd2.2_m2rc' || input$design == 'd3.3_m3rc2rc'||
       input$design == 'd3.2_m3ff2rc' || input$design == 'd3.2_m3ff2rc'){
      print('Design r2-2 Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      theDesign = as.character(getDesign())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      
      div(style = "display: inline-block, vertical-align:top;", 
          r2.2Input(design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
    }else{
      # leave blank otherwise
    }
    
    
  }) # R2.2 element for chosen and required designs
  
  output$icc.2 <- renderUI({
    
    req(input$design)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$design == 'd2.1_m2fc' || input$design == 'd2.1_m2ff' || input$design == 'd2.1_m2fr' ||
       input$design == 'd3.1_m3rr2rr' || input$design == 'd2.2_m2rc' || input$design == 'd3.3_m3rc2rc' ||
       input$design == 'd3.2m3ff2rc' || input$design == 'd3.2m3rr2rc'){
      print('Design ICC2 Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      theDesign = as.character(getDesign())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      
      div(style = "display: inline-block, vertical-align:top;", 
          icc.2Input(design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
    }else{
      # leave blank otherwise
    }

  }) # icc.2 element for chosen and required designs
  
  output$omega.2 <- renderUI({
  
    req(input$design)    # requiring design input
    check = FALSE # checking default condition as fault
    
    # conditions when check becomes true
    if(input$design == 'd2.1_m2fr' || input$design == 'd3.1_m3rr2rr'){
      print('Design omega 2 Trigger')
      check = TRUE
    }
    
    # output ui when check is true
    if(check){
      theDesign = as.character(getDesign())
      theScenario = as.character(whichTab$scenario)
      theNumOutcomes = as.numeric(getNumOutcomes())
      
      div(style = "display: inline-block, vertical-align:top;", 
          omega.2Input(design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
    }else{
      # leave blank otherwise
    }
    
  }) # omega.2 element for chosen and required designs
  
  output$r2.3 <- renderUI({
    
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    theNumOutcomes = as.numeric(getNumOutcomes())
    
    
    div(style = "display: inline-block, vertical-align:top;", 
        r2.3Input(design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
    
  }) # R2.3 element for chosen and required designs
  
  output$icc.3 <- renderUI({
    
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    theNumOutcomes = as.numeric(getNumOutcomes())
    
    div(style = "display: inline-block, vertical-align:top;", 
        icc.3Input(design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
    
  }) # icc.3 element for chosen and required designs
  
  output$omega.3 <- renderUI({
    
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    theNumOutcomes = as.numeric(getNumOutcomes())
    
    div(style = "display: inline-block, vertical-align:top;", 
        omega.3Input(design = theDesign, scenario = theScenario, numOutcome = theNumOutcomes))
    
  }) # omega.3 element for chosen and required designs
  
  output$k <- renderUI({
    
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    theNumOutcomes = as.numeric(getNumOutcomes())
    
    div(style = "display: inline-block, vertical-align:top;", 
        kInput(design = theDesign , scenario = theScenario))    
    
  }) # number of level-3 groups
  

  
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
    
    ############################################################################
    # Generating Power Results for diffferent designs & mode of exploration    #
    ############################################################################
    
    # Getting the research design that we have to estimate the statistical results for
    theDesign = as.character(getDesign())
    theScenario = as.character(whichTab$scenario)
    
    # set up to receive all the input parameters
    
    # Get string for input subsetting
    design_subset <- "design"
    m_subset <- "numOutcomes"
    nbar_subset <- paste0("nbar", "_", theDesign, "_", theScenario)
    j_subset <- paste0("j", "_", theDesign, "_", theScenario)
    mtp_subset <- paste0("mtp", "_", theDesign,"_", theScenario)
    mdes_subset <- paste0("mdes", "_", theDesign, "_", theScenario)
    rho_subset <- paste0("rho", "_", theDesign, "_", theScenario)
    numCovar.1_subset <- paste0("numCovar.1","_", theDesign, "_", theScenario)
    tbar_subset <- paste0("tbar","_", theDesign, "_", theScenario)
    alpha_subset <- paste0("alpha", "_", theDesign, "_", theScenario)
    r2.1_subset <- paste0("r2.1", "_", theDesign, "_", theScenario)
    r2.2_subset <- paste0("r2.2", "_", theDesign, "_", theScenario)
    icc.2_subset <- paste0("icc.2", "_", theDesign, "_", theScenario)
    omega.2_subset <- paste0("omega.2", "_", theDesign, "_", theScenario)
    r2.3_subset <- paste0("r2.3", "_", theDesign, "_", theScenario)
    icc.3_subset <- paste0("icc.3", "_", theDesign, "_", theScenario)
    k_subset <- paste0("k","_", theDesign, "_", theScenario)
    omega.3_subset <- paste0("omega.3", "_", theDesign, "_", theScenario)
    
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
    k <- "0"
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
