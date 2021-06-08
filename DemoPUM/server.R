library(shiny)

# To be called from ui.R
lineChartOutput <- function(inputId, width="100%", height="600px") {
  style <- sprintf("width: %s; height: %s;",
                   validateCssUnit(width), validateCssUnit(height))
  
  tagList(
    # Include CSS/JS dependencies. Use "singleton" to make sure that even
    # if multiple lineChartOutputs are used in the same page, we'll still
    # only include these chunks once.
    singleton(tags$head(
      tags$script(src="d3/d3.v3.min.js"),
      tags$script(src="nvd3/nv.d3.min.js"),
      tags$link(rel="stylesheet", type="text/css", href="nvd3/nv.d3.min.css"),
      tags$script(src="linechart-binding.js")
    )),
    div(id=inputId, class="nvd3-linechart", style=style,
        tag("svg", list())
    )
  )
}

# To be called from server.R
renderLineChart <- function(expr, env=parent.frame(), quoted=FALSE) {
  # This piece of boilerplate converts the expression `expr` into a
  # function called `func`. It's needed for the RStudio IDE's built-in
  # debugger to work properly on the expression.
  installExprFunction(expr, "func", env, quoted)
  
  function() {
    dataframe <- func()
    
    mapply(function(col, name) {
      
      values <- mapply(function(val, i) {
        list(x = i, y = val)
        #      }, col, 1:nrow(dataframe), SIMPLIFY=FALSE, USE.NAMES=FALSE)
      }, col, rownames(dataframe), SIMPLIFY=FALSE, USE.NAMES=FALSE)
      
      list(key = name, values = values)
      
    }, dataframe, names(dataframe), SIMPLIFY=FALSE, USE.NAMES=FALSE)
  }
}


# Data frame or list looks like:
# 
# {
#   "Series A": [1,2,3,4,5],
#   "Series B": [6,7,8,9,10]
# }
# 
# D3 expects:
# 
# [
#   {
#     key: "Series A",
#     values: [{x:1,y:1}, {x:2,y:2}, {x:3,y:3}, {x:4,y:4}, {x:5,y:5}]
#   },
#   {
#     key: "Series B",
#     values: [{x:1,y:6}, {x:2,y:7}, {x:3,y:8}, {x:4,y:9}, {x:5,y:10}]
#   }
# ]
dataFrameToD3 <- function(df=cars) {
  
}
library(shiny)

# To be called from ui.R
lineChartOutput <- function(inputId, width="100%", height="600px") {
  style <- sprintf("width: %s; height: %s;",
                   validateCssUnit(width), validateCssUnit(height))
  
  tagList(
    # Include CSS/JS dependencies. Use "singleton" to make sure that even
    # if multiple lineChartOutputs are used in the same page, we'll still
    # only include these chunks once.
    singleton(tags$head(
      tags$script(src="d3/d3.v3.min.js"),
      tags$script(src="nvd3/nv.d3.min.js"),
      tags$link(rel="stylesheet", type="text/css", href="nvd3/nv.d3.min.css"),
      tags$script(src="linechart-binding.js")
    )),
    div(id=inputId, class="nvd3-linechart", style=style,
        tag("svg", list())
    )
  )
}

# To be called from server.R
renderLineChart <- function(expr, env=parent.frame(), quoted=FALSE) {
  # This piece of boilerplate converts the expression `expr` into a
  # function called `func`. It's needed for the RStudio IDE's built-in
  # debugger to work properly on the expression.
  installExprFunction(expr, "func", env, quoted)
  
  function() {
    dataframe <- func()
    
    mapply(function(col, name) {
      
      values <- mapply(function(val, i) {
        list(x = i, y = val)
        #      }, col, 1:nrow(dataframe), SIMPLIFY=FALSE, USE.NAMES=FALSE)
      }, col, rownames(dataframe), SIMPLIFY=FALSE, USE.NAMES=FALSE)
      
      list(key = name, values = values)
      
    }, dataframe, names(dataframe), SIMPLIFY=FALSE, USE.NAMES=FALSE)
  }
}


# Data frame or list looks like:
# 
# {
#   "Series A": [1,2,3,4,5],
#   "Series B": [6,7,8,9,10]
# }
# 
# D3 expects:
# 
# [
#   {
#     key: "Series A",
#     values: [{x:1,y:1}, {x:2,y:2}, {x:3,y:3}, {x:4,y:4}, {x:5,y:5}]
#   },
#   {
#     key: "Series B",
#     values: [{x:1,y:6}, {x:2,y:7}, {x:3,y:8}, {x:4,y:9}, {x:5,y:10}]
#   }
# ]
dataFrameToD3 <- function(df=cars) {
  
}

nblocks<-c(10,20,30,40,50,60)
nindiv<-c(10,20,30,40,50,60)
adjustment<-c("None","BF","BH","WY")
ICC<-seq(0,1,by=0.2)
R2.1<-R2.2<-ICC
dat<-expand.grid(nblocks,nindiv,adjustment,ICC,R2.1,R2.2)
cor<-seq(0,1,by=0.2)
power.type<-c("Individual") #,"1-Minimal","50%-Minimal")
ntests<-c(2,4,6)
dat<-expand.grid(nblocks,nindiv,power.type,adjustment,cor,ntests,ICC,R2.1,R2.2)
colnames(dat)<-c("nblocks","nindiv","power.type","adjustment","cor","ntests","ICC","R2.1","R2.2")
nn<-nrow(dat)
dat$power<-rnorm(nn,0.7,0.15)
dat$power[dat$power>1]<-0.1



#shinyServer(function(input, output, session) {
shinyServer(function(input, output) {
  
  output$text1 <- renderText({ 
    paste("You have selected", input$var)
  })
  
  output$mychart1 <- renderLineChart({
    if (input$plot.what=="cor")     ww1<-which(dat$power.type=="Individual" & dat$nindiv==input$nindiv & dat$ICC==input$ICC & dat$R2.1==input$R2.1 & dat$R2.2==input$R2.2 & dat$nblocks==input$nblocks & dat$ntests==input$ntests)
    if (input$plot.what=="nblocks") ww1<-which(dat$power.type=="Individual" & dat$nindiv==input$nindiv & dat$ICC==input$ICC & dat$R2.1==input$R2.1 & dat$R2.2==input$R2.2 & dat$cor==input$cor & dat$ntests==input$ntests)
    if (input$plot.what=="nindiv")  ww1<-which(dat$power.type=="Individual" & dat$nblocks==input$nblocks & dat$ICC==input$ICC & dat$R2.1==input$R2.1 & dat$R2.2==input$R2.2 & dat$cor==input$cor & dat$ntests==input$ntests)
    if (input$plot.what=="ICC")     ww1<-which(dat$power.type=="Individual" & dat$nindiv==input$nindiv & dat$nblocks==input$nblocks & dat$R2.1==input$R2.1 & dat$R2.2==input$R2.2 & dat$cor==input$cor & dat$ntests==input$ntests)
    if (input$plot.what=="R2.1")    ww1<-which(dat$power.type=="Individual" & dat$nindiv==input$nindiv & dat$ICC==input$ICC & dat$nblocks==input$nblocks & dat$R2.2==input$R2.2 & dat$cor==input$cor & dat$ntests==input$ntests)
    if (input$plot.what=="R2.2")    ww1<-which(dat$power.type=="Individual" & dat$nindiv==input$nindiv & dat$ICC==input$ICC & dat$R2.1==input$R2.1 & dat$nblocks==input$nblocks & dat$cor==input$cor & dat$ntests==input$ntests)
    newdat1<-dat[ww1,]
    BF1<-newdat1$power[newdat1$adjustment=="BF"]
    BH1<-newdat1$power[newdat1$adjustment=="BH"]
    WY1<-newdat1$power[newdat1$adjustment=="WY"]
    #print(BF1)
    if (input$plot.what%in%c("cor","ICC","R2.1","R2.2")) names(BF1)<-names(BH1)<-names(WY1)<-seq(0,1,by=0.2)
    if (input$plot.what%in%c("nblocks","nindiv")) names(BF1)<-names(BH1)<-names(WY1)<-seq(10,60,by=10)
    
    data.frame(
      #     None = dat$power[which(dat$nblocks==10 & dat.adjustment=="None" & dat$power.type=="Individual")],
      Bonferroni = BF1,
      BenjaminiHochberg = BH1,
      WestfallYoung = WY1
    )
  })
  # output$mychart2 <- renderLineChart({
  #   if (input$plot.what=="cor")     ww2<-which(dat$power.type=="1-Minimal" & dat$nindiv==input$nindiv & dat$ICC==input$ICC & dat$R2.1==input$R2.1 & dat$R2.2==input$R2.2 & dat$nblocks==input$nblocks & dat$ntests==input$ntests)
  #   if (input$plot.what=="nblocks") ww2<-which(dat$power.type=="1-Minimal" & dat$nindiv==input$nindiv & dat$ICC==input$ICC & dat$R2.1==input$R2.1 & dat$R2.2==input$R2.2 & dat$cor==input$cor & dat$ntests==input$ntests)
  #   if (input$plot.what=="nindiv")  ww2<-which(dat$power.type=="1-Minimal" & dat$nblocks==input$nblocks & dat$ICC==input$ICC & dat$R2.1==input$R2.1 & dat$R2.2==input$R2.2 & dat$cor==input$cor & dat$ntests==input$ntests)
  #   if (input$plot.what=="ICC")     ww2<-which(dat$power.type=="1-Minimal" & dat$nindiv==input$nindiv & dat$nblocks==input$nblocks & dat$R2.1==input$R2.1 & dat$R2.2==input$R2.2 & dat$cor==input$cor & dat$ntests==input$ntests)
  #   if (input$plot.what=="R2.1")    ww2<-which(dat$power.type=="1-Minimal" & dat$nindiv==input$nindiv & dat$ICC==input$ICC & dat$nblocks==input$nblocks & dat$R2.2==input$R2.2 & dat$cor==input$cor & dat$ntests==input$ntests)
  #   if (input$plot.what=="R2.2")    ww2<-which(dat$power.type=="1-Minimal" & dat$nindiv==input$nindiv & dat$ICC==input$ICC & dat$R2.1==input$R2.1 & dat$nblocks==input$nblocks & dat$cor==input$cor & dat$ntests==input$ntests)
  #   newdat2<-dat[ww2,]
  #   data.frame(
  #     #     None = dat$power[which(dat$nblocks==10 & dat.adjustment=="None" & dat$power.type=="Individual")],
  #     BF = newdat2$power[newdat2$adjustment=="BF"],
  #     BH = newdat2$power[newdat2$adjustment=="BH"],
  #     WY = newdat2$power[newdat2$adjustment=="WY"]
  #   )
  #   })
  # output$mychart3 <- renderLineChart({
  #   if (input$plot.what=="cor")     ww3<-which(dat$power.type=="50%-Minimal" & dat$nindiv==input$nindiv & dat$ICC==input$ICC & dat$R2.1==input$R2.1 & dat$R2.2==input$R2.2 & dat$nblocks==input$nblocks & dat$ntests==input$ntests)
  #   if (input$plot.what=="nblocks") ww3<-which(dat$power.type=="50%-Minimal" & dat$nindiv==input$nindiv & dat$ICC==input$ICC & dat$R2.1==input$R2.1 & dat$R2.2==input$R2.2 & dat$cor==input$cor & dat$ntests==input$ntests)
  #   if (input$plot.what=="nindiv")  ww3<-which(dat$power.type=="50%-Minimal" & dat$nblocks==input$nblocks & dat$ICC==input$ICC & dat$R2.1==input$R2.1 & dat$R2.2==input$R2.2 & dat$cor==input$cor & dat$ntests==input$ntests)
  #   if (input$plot.what=="ICC")     ww3<-which(dat$power.type=="50%-Minimal" & dat$nindiv==input$nindiv & dat$nblocks==input$nblocks & dat$R2.1==input$R2.1 & dat$R2.2==input$R2.2 & dat$cor==input$cor & dat$ntests==input$ntests)
  #   if (input$plot.what=="R2.1")    ww3<-which(dat$power.type=="50%-Minimal" & dat$nindiv==input$nindiv & dat$ICC==input$ICC & dat$nblocks==input$nblocks & dat$R2.2==input$R2.2 & dat$cor==input$cor & dat$ntests==input$ntests)
  #   if (input$plot.what=="R2.2")    ww3<-which(dat$power.type=="50%-Minimal" & dat$nindiv==input$nindiv & dat$ICC==input$ICC & dat$R2.1==input$R2.1 & dat$nblocks==input$nblocks & dat$cor==input$cor & dat$ntests==input$ntests)
  #   newdat3<-dat[ww3,]
  #   data.frame(
  #     #     None = dat$power[which(dat$nblocks==10 & dat.adjustment=="None" & dat$power.type=="Individual")],
  #     BF = newdat3$power[newdat3$adjustment=="BF"],
  #     BH = newdat3$power[newdat3$adjustment=="BH"],
  #     WY = newdat3$power[newdat3$adjustment=="WY"]
  #   )
  # })
  
})
