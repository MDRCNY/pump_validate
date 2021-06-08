shinyUI(fluidPage(
  tags$h2("Estimated power by adjustment procedure, with varying assumptions"),
  fluidRow(
    column(width=6,
           p("Modeled on code at",
             a(href="http://nvd3.org/examples/line.html", "Simple Line Chart"),
             "example for the",
             a(href="http://nvd3.org/", "NVD3.js"),
             "JavaScript charting library."
           ),
           p("Source code:",
             a(href="https://github.com/jcheng5/shiny-js-examples/tree/master/output", "@jcheng5/shiny-js-examples/output"))
    )
  ),
  fluidRow(
    column(width=8,
           lineChartOutput("mychart1")
           #lineChartOutput("mychart2"),
           #lineChartOutput("mychart3")
    ),
    column(width=4,
           selectInput("plot.what", "What do you want on the X-axis?", 
                       choices = list("Correlation between outcomes" = "cor", "Number of Blocks" = "nblocks", "Number of Individuals" = "nindiv",
                                      "R2 at Level 1" = "R2.1", "R2 at Level 2" = "R2.2",
                                      "ICC" = "ICC"), selected = 3),
           
           selectInput("ntests", "Number of tests", 
                       choices = list("2" = 2, "4" = 4,"6" = 6), selected = 1),
           sliderInput("cor", "Correlation between outcomes",min = 0, max = 1, value = 0.2,step=0.2,animate=animationOptions(interval=100, loop=TRUE)),     
           sliderInput("nblocks", "Number of blocks",min = 10, max = 60, value = 10,step=10,animate=animationOptions(interval=100, loop=TRUE)),     
           sliderInput("nindiv", "Number of individuals per block",min = 10, max = 60, value = 10,step=10,animate=animationOptions(interval=100, loop=TRUE)),     
           sliderInput("ICC", "ICC",min = 0, max = 1, value = 0.2,step=0.2,animate=animationOptions(interval=100, loop=TRUE)),     
           sliderInput("R2.1", "R2 for Level 1",min = 0, max = 1, value = 0.2,step=0.2,animate=animationOptions(interval=100, loop=TRUE)),     
           sliderInput("R2.2", "R2 for Level 2",min = 0, max = 1, value = 0.2,step=0.2,animate=animationOptions(interval=100, loop=TRUE))     
    )
  )
))
