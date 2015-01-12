#runapp ####
source( paste(getwd(), "source/ini_fun.R", sep="/") )
source( pastedir(getwd() , "source/ini_data.R") )

var=d[,unique(var)]
strato=d[,unique(id_strato)]
codsis=d[,unique(codsis199)]
codlft=d[,unique(codlft199)]

shinyServer(function(input, output, session) {
  
  input_var=reactive({ input$var  })
  input_codsis=reactive({ input$codsis  })
  input_codlft=reactive({ input$codlft  })
  input_strato=reactive({ input$strato  })
  input_check_gio=reactive({ input$check_gio  })
  
  #output$var=renderUI({  checkboxGroupInput("var", label = "Choose a variable to analyze", choices =var , selected=var) })
  output$var=renderUI({  selectInput("var", label = "Select the variables:", choices =var, selected = var, multiple = T ) })  
  output$codsis=renderUI({selectInput("codsis", label = "Apply a filter on gear type:", choices =codsis, selected = codsis, multiple = T ) })
  output$codlft=renderUI({ selectInput("codlft", label = "Apply a filter on LOA:", choices =codlft, selected = codlft, multiple = T ) })
  
  output$strato=renderUI({ selectInput("strato", label = "Select the strata:", choices =strato, selected = NULL, multiple = T ) })
  
  observe({ if (!is.null(input_codsis()) | !is.null(input_codlft())  ) updateSelectInput(session, "strato", choices =strato, selected = NULL) })
  observe({ 
    if (!is.null(input_strato()) ) {
    updateSelectInput(session, "codsis", choices =codsis, selected = NULL) 
    updateSelectInput(session, "codlft", choices =codlft, selected = NULL)
    }
  })
  
  observe({ 
    if (input$reset>0) {
      updateSelectInput(session, "codsis", choices =codsis, selected = codsis) 
      updateSelectInput(session, "codlft", choices =codlft, selected = codlft)
    }
  })
  
  d_panel=reactive({
    if (  !is.null( input_strato() )  ) {    
        d[giorni_mare>(input_check_gio()-1) & var %in% input_var() & id_strato %in% input_strato() ]    
      } else {    
        d[giorni_mare>(input_check_gio()-1) & var %in% input_var() & codsis199 %in% input_codsis() & codlft199 %in% input_codlft()]    
    }
  })

  output$boxplot = renderPlot({   
    d_panel()[ ,ggplot(.SD, aes(x= var,y=value)) +geom_boxplot(outlier.size=3 ,outlier.colour="red", fill="grey",colour = "blue") + xlab("") ]
  })
  
  #output$table_data = renderTable({head(d_panel() )})
  output$table_data = renderDataTable({d_panel()})
  
  output$text=renderText({if(is.null(input_strato())) 0 else input_strato() })
  
})