#runapp ####
wd=getwd()
source( paste(wd, "source/ini_fun.R", sep="/") )
source( pastedir(wd , "source/ini_data.R") )

var=d[,unique(var)]
strato=d[giorni_mare>0,unique(id_strato)]

shinyServer(function(input, output) {
  
  input_var=reactive({ input$var  })
  input_strato=reactive({ input$strato  })
  input_check_gio=reactive({ input$check_gio  })
  
  #output$check_giorni=checkboxInput(inputId = "check_gio",label = "Controllo variabili solo se giorni_mare>0", value=T)
  
  output$var=renderUI({  selectInput("var", label = "Choose a variable to analyze", choices =var, selected = var[1] ) })
  output$strato=renderUI({  selectInput("strato", label = "Choose a strata to analyze", choices =strato, selected = strato[1] ) })
  
  output$boxplot = renderPlot({
    
    d[giorni_mare>(input_check_gio()-1) & id_strato==input_strato() & var==input_var() ,ggplot(.SD, aes(x= var,y=value)) +geom_boxplot(outlier.size=3 ,outlier.colour="red")]
    
    
  })
})