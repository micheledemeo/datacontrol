#runapp:
pastedir=function(...) paste(..., sep="/")
library(data.table)
library(ggplot2)
pri=fread(pastedir(getwd(),"input/pri.txt") )
camp=fread(pastedir(getwd(),"input/campionari.txt") )
setkey(camp, batcod)
setkey(pri, batcod)
camp=pri[,.(batcod, strato)][camp]
var=camp[,unique(cod_variable)]
strato=camp[,unique(strato)]
strato=strato[order(strato)]

shinyServer(function(input, output) {
  
  input_var=reactive({ input$var  })
  input_strato=reactive({ input$strato  })
  
  output$var=renderUI({  selectInput("var", label = "Choose a variable to analyze", choices =var, selected = var[1] ) })
  output$strato=renderUI({  selectInput("strato", label = "Choose a strata to analyze", choices =strato, selected = strato[1] ) })
  
  output$boxplot = renderPlot({
    
    camp[ cod_variable==input_var() & strato==input_strato(),
         ggplot(.SD, aes(x=cod_variable, y= values, fill=cod_variable)) + geom_boxplot() + guides(fill=FALSE), 
         .SDcols=c('cod_variable', 'values') ]
  })
})