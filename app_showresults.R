# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# fake data for testing (Deni's original code)
nblocks<-c(10,20,30,40,50,60)
nindiv<-c(10,20,30,40,50,60)
mtp<-c("None","BF","HO","BH","WY")
R2<-seq(0,1,by=0.1)
cor<-seq(0,1,by=0.1)
ntests<-c(3,6,9,12)
prfalse<-c("1/3","2/3","1")
defn<-c("individual","1-minimal","1/3-minimal","2/3-minimal","complete")
dat<-expand.grid(nblocks,nindiv,mtp,R2,cor,ntests,prfalse,defn)
colnames(dat)<-c("nblocks","nindiv","mtp","R2","cor","ntests","prfalse","defn")
dat$power<-rnorm(nrow(dat),0.7,0.15) #drawn from Normal Distribution
dat$power[dat$power>1]<-0.1

#' Data generating function for Causal Relations
#'
#' @param N Number of sample size
#' @param beta_0 Intercept/The baseline value 
#' @param beta_1 The group mean difference of X1 covariate 
#' @param seed To be set for consistency in the data generating process  
#' @param tau treatment effect
#'
#' @return a data set
#' @export
#'
#' @examples
dgp = function(N, beta_0, beta_1, seed, tau = 5){
  
    set.seed(seed = seed)
    X = rnorm(seq(N), mean = 65, sd = 3)
    Y_0 = beta_0 + beta_1 * X + 0 + rnorm(seq(N), mean = 0, sd = 1)
    Y_1 = beta_0 + beta_1 * X + tau + rnorm(seq(N), mean = 0, sd = 1)
    dat = data.frame(cbind(seq(N), X, Y_0,Y_1))
    colnames(dat) <- c("Index", "X", "Y0", "Y1")
    
  return(dat)
    
}#dgp function ends

# Libraries loaded here
library(shiny)
library(dropR)
library(shinyBS)
library(shinydashboard)
library(DT)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Statistical Power When Adjusting for Multiple Hypothesis Tests", style = "font-family: 'calibri'; font-size: 48px; color:blue")),
  
  
  navbarPage(title=NULL, id="Navbar",
             tabPanel("Landing page", 
                      mainPanel(
                      
                        h1("Background", style="color:red"),
                        tags$div(
                          tags$p("Researchers are often interested in testing the effectiveness of an intervention on multiple outcomes, 
                                 for multiple subgroups, at multiple points in time, or across multiple treatment groups. The resulting 
                                 multiplicity of statistical hypothesis tests can lead to spurious findings of effects. 
                                 Multiple testing procedures (MTPs) are statistical procedures that counteract this problem by adjusting p-values
                                 for effect estimates upward. While MTPs are increasingly used in impact evaluations in many subject areas,
                                 an important consequence of their use is a change in statistical power that can be substantial. Unfortunately, 
                                 researchers frequently ignore the power implications of MTPs when designing studies. Consequently, in some cases,
                                 sample sizes may be too small, and studies may be underpowered to detect effects as small as a desired size.
                                 In other cases, sample sizes may be larger than needed, or studies may be powered to detect smaller effects than anticipated.",
                                 style = "font-family: 'calibri'; font-size: 16px"), 
                          tags$p("Researchers typically worry that moving from one to multiple hypothesis tests and thus employing MTPs results in a loss of power.
                                 However, that need not always be the case. Power is indeed lost if one focuses on individual power: the probability of detecting
                                 an effect of a particular size or larger for each particular hypothesis test, given that the effect truly exists. However,
                                 in studies with multiplicity, alternative definitions of power exist that in some cases may be more appropriate. For example,
                                 when testing for effects on multiple outcomes, one might consider 1-minimal power: the probability of detecting effects of at
                                 least a particular size on at least one outcome. Similarly, one might consider ½-minimal power: the probability of detecting effects
                                 of at least a particular size on at least ½ of the outcomes. Also, one might consider complete power: the power to detect effects
                                 of at least a particular size on all out-comes. The choice of definition of power depends on the objectives of the study and on
                                 how the success of the intervention is defined. The choice of definition also affects the overall extent of power.",
                                 style = "font-family: 'calibri'; font-size: 16px"),
                          tags$p("This app presents computes statistical power, for multiple definitions of statistical power, when applying any of five common MTPs —
                                 Bonferroni, Holm, single-step and step-down versions of Westfall-Young, and Benjamini-Hochberg. It also presents empirical findings on how power
                                 is affected by the use of MTPs. Currently, the app focuses on multiplicity that results from estimating effects on multiple outcomes,
                                 and it focuses on one common research design and analysis plan that education studies typically use in practice: a multisite, randomized
                                 controlled trial (RCT) with the blocked randomization of individuals, in which effects are estimated using a model with block-specific 
                                 intercepts and with the assumption of constant effects across blocks.",
                                 style = "font-family: 'calibri'; font-size: 16px"),
                          tags$a(href="https://www.mdrc.org/sites/default/files/EC%20Methods%20Paper_2016.pdf", "The methods used to compute statistical power are documented here."),
                          br(),
                          br(),
                          br(),
                          actionButton("link_to_tabpanel_Plot", "Link to Plot panel")
                          
                        ))),
             tabPanel("Plot", 
                      
                      # Sidebar 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("plot.what", "What do you want on the X-axis?", 
                                      choices = list("Correlation between outcomes" = "cor", "Number of Blocks" = "nblocks", "Number of Individuals" = "nindiv","R2" = "R2")),
                          selectInput("MTP", "What MTP do you plan to use?", 
                                      choices = list("Bonferroni" = "BF", "Holm" = "HO", "Westfall-Young" = "WY","Benjamini-Hochberg" = "BH")),
                          bsPopover(id = "MTP", title = NULL,
                                    content = paste0("For more information on MTPs, refer to the ", 
                                                     a("statistical power paper here ", 
                                                       href = "https://www.mdrc.org/sites/default/files/EC%20Methods%20Paper_2016.pdf",
                                                       target="_blank")),
                                    placement = "right", 
                                    trigger = "click", 
                                    options = list(container = "body")),
                          
                          selectInput("ntests", "Number of tests", choices = list("3" = 3, "6" = 6, "9" = 9,"12" = 12)),
                          sliderInput("cor", "Correlation between outcomes",min = 0, max = 1, value = 0.1, step = 0.1, animate=animationOptions(interval=100, loop=TRUE)),     
                          sliderInput("nblocks", "Number of blocks", min = 10, max = 60, value = 10,step=10, animate=animationOptions(interval=100, loop=TRUE)),     
                          sliderInput("nindiv", "Number of individuals per block",min = 10, max = 60, value = 10,step=10,animate=animationOptions(interval=100, loop=TRUE)),     
                          sliderInput("R2", "R2", min = 0, max = 1, value = 0.1 ,step=0.1, animate=animationOptions(interval=100, loop=TRUE))
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          
                          tabsetPanel(
                            tabPanel("Individual Power", plotOutput("LineChart1"), br(), br(), br(), br(), br(), br(),
                                     bsCollapse(
                                       id = "description", open = NULL,
                                       bsCollapsePanel("How to read this plot","This figure presents estimates when....", style="default")
                                               ),
                                       bsCollapsePanel("Key Findings","The probability of detecting an effect on a particular outcome can be (but is not necessarily) much lower when adjusting p-values for multiple tests.", style="success")
                                               ),
                                    

                            tabPanel("1-Minimal Power", plotOutput("LineChart2"), br(), br(), br(), p("some text for the specific tab")),
                            tabPanel("1/3-Minimal Power", plotOutput("LineChart3")),
                            tabPanel("2/3-Minimal Power", plotOutput("LineChart4")), 
                            tabPanel("Complete Power", plotOutput("LineChart5"))
                          ),
                          br(),
                          br(),
                          br(),
                          p("This text goes on all tabs."),
                          br(),
                          p("This too.")
                        )
                      )),
             tabPanel("Calculation", 
                  
                  # A sidebar layout
                  sidebarLayout(
                    sidebarPanel(
                      #selector input for type of Simulation
                      selectInput("t.sim", "Type of Simulations", choices = list("Sample Based" = "s.base", "Randomization Based" = "r.base")),
                      #numeric input for number of sample sizes
                      #Inputting a fluid row for testing with bootstrap fluidRow and Column designs
                      fluidRow(
                        
                        column(6,
                          numericInput("n", "N", value = 100, min = 100, max = 100000)
                        ), #End of first column
                        
                        column(6,
                          numericInput("s", "S", value = 200, min = 200, max = 2000)
                        ) #End of second column
                        
                      ), # End of Fluid row 1
                      
                      #Inputting another fluid row for testing with action button and vertical alignment
                      fluidRow(
                      
                        column(11,
                          div(style ="display: inline-block, vertical-align:top;", sliderInput("beta_0", "Beta_0", min = 5, max = 15, value = 10, step = 1))#div ends
                        ), #End of first column
                      
                        column(1,
                          div(style ="display: inline-block, vertical-align:top;",actionButton("question",label = "", icon = icon("question"))) #div ends
                        ) # End of second column
                        
                      ), # End of Fluid row 2  
                      sliderInput("beta_1", "Beta_1", min = 1, max = 5, value = 3.5, step = 0.5),
                      numericInput("seed", "Seed", value = 1234, min = 1000, max = 2000),
                      sliderInput("assign", "Assignment Probabilites", min = 0.3, max = 0.8, value = 0.5, step = 0.1)
                    ),# sidebar panel 1
                    
               
                    mainPanel(
                      
                      fluidRow(
                        
                        box(
                          title = "Calculation Answers", width = 12, height = 300
                        ), # box for answers
                        
                        box( title = "Data Views", width = 12, height = 300,
                             tableOutput("view") #The view table output
                        ) # box for data views
                        
                      ) # fluid Row 

                    ) # main panel
                      
                      ) # sidebar layout 
                      
             ) # tabpanel              
             
  ) # navbar Panel
  
) # fluid Panel

) # shiny UI


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  # Function Section
  ## Subset plot function
  subsetplot<-function(dat,which.defn) {
    dat<-dat[dat$defn==which.defn,]
    
    #what is ww here?
    
    if (input$plot.what=="cor")     ww<-which(dat$nindiv==input$nindiv & dat$nblocks==input$nblocks & dat$ntests==input$ntests & dat$R2==input$R2)
    if (input$plot.what=="nblocks") ww<-which(dat$nindiv==input$nindiv                              & dat$ntests==input$ntests & dat$R2==input$R2 & dat$cor==cor)
    if (input$plot.what=="nindiv")  wW<-which(                           dat$nblocks==input$nblocks & dat$ntests==input$ntests & dat$R2==input$R2 & dat$cor==cor)
    if (input$plot.what=="R2")      ww<-which(dat$nindiv==input$nindiv & dat$nblocks==input$nblocks & dat$ntests==input$ntests                    & dat$cor==cor)
    dc<-dat[ww,]
    
    #So the X covariates are the columns from input$plot.what??
    
    cc<-which(colnames(dc)==input$plot.what)
    
    dc.3.3<-dc[dc$prfalse=="1",]
    dc.2.3<-dc[dc$prfalse=="2/3",]
    dc.1.3<-dc[dc$prfalse=="1/3",]
    
    par(oma = c(0,0,0,2))
    plot(dc.3.3[dc.3.3$mtp==input$MTP,cc],dc.3.3[dc.3.3$mtp==input$MTP,"power"],ylim=c(0,1),ylab="Power",xlab=input$plot.what,col=1,type="p",pch=18)
    points(dc.2.3[dc.2.3$mtp==input$MTP,cc],dc.2.3[dc.2.3$mtp==input$MTP,"power"],col=2,type="p",pch=18)
    points(dc.1.3[dc.1.3$mtp==input$MTP,cc],dc.1.3[dc.1.3$mtp==input$MTP,"power"],col=3,type="p",pch=18)
    legend("bottom", inset=c(0,-0.5), legend=c("All nulls are false","2/3 nulls are false","1/3 nulls are false"), col=c(1,2,3), xpd=TRUE)
    
  }
  
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
  

  
  
  
  
  
  output$LineChart1 <- renderPlot({subsetplot(dat,"individual") }, width = 700, height= 500) #this sets the plot size
  output$LineChart2 <- renderPlot({subsetplot(dat,"1-minimal") }, width = 700, height= 500) #this sets the plot size
  output$LineChart3 <- renderPlot({subsetplot(dat,"1/3-minimal") }, width = 700, height= 500) #this sets the plot size
  output$LineChart4 <- renderPlot({subsetplot(dat,"2/3-minimal") }, width = 700, height= 500) #this sets the plot size
  output$LineChart5 <- renderPlot({subsetplot(dat,"complete") }, width = 700, height= 500) #this sets the plot size
  
  
  
  addPopover(session, "LineChart1", "Data", content = paste0("Description of the plot
                                                             "), trigger = 'click')
  
  #Links to panel 'Plot'
  observeEvent(input$link_to_tabpanel_Plot, {
    newvalue <- "Plot"
    updateTabItems(session, "Navbar", newvalue)
  })
  
  
  
})

# Run the application 
options(shiny.trace=FALSE)
shinyApp(ui = ui, server = server)




