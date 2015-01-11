#runapp ####
source( paste(getwd(), "source/ini_fun.R", sep="/") )
source( pastedir(getwd() , "source/ini_data.R") )

var=d[,unique(var)]
strato=d[,unique(id_strato)]
regione=d[,unique(regione)]
codsis=d[,unique(codsis199)]
codlft=d[,unique(codlft199)]

shinyServer(function(input, output) {
  
  input_var=reactive({ input$var  })
  input_codsis=reactive({ input$codsis  })
  input_codlft=reactive({ input$codlft  })
  input_strato=reactive({ input$strato  })
  input_check_gio=reactive({ input$check_gio  })
  
  #output$var=renderUI({  checkboxGroupInput("var", label = "Choose a variable to analyze", choices =var , selected=var) })
  output$var=renderUI({  selectInput("var", label = "Scegli variabili di costo:", choices =var, selected = var, multiple = T ) })  
  output$codsis=renderUI({  selectInput("codsis", label = "Filtra sistema di pesca:", choices =codsis, selected = codsis, multiple = T ) })
  output$codlft=renderUI({  selectInput("codlft", label = "Filtra classe lft:", choices =codlft, selected = codlft, multiple = T ) })
  
  output$strato=renderUI({  selectInput("strato", label = "Filtra strato:", choices =strato, selected = NULL, multiple = T ) })
  
  d_panel=reactive({
    if (  !is.null( input_strato() )  ) {    
        d[giorni_mare>(input_check_gio()-1) & id_strato %in% input_strato() & var %in% input_var()]    
      } else {    
        d[giorni_mare>(input_check_gio()-1) & var %in% input_var()]    
    }
  })

  output$boxplot = renderPlot({   
    d_panel()[ ,ggplot(.SD, aes(x= var,y=value)) +geom_boxplot(outlier.size=3 ,outlier.colour="red", fill="grey",colour = "blue") + xlab("") ]
  })
  
  #output$table_data = renderTable({head(d_panel() )})
  output$table_data = renderDataTable({d_panel()})
  
})