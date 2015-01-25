#runapp: ####
#library(shiny)
#runApp("C:/Users/mdemeo/Documents/000/datacontrol",port = 12345)

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
  
  # considering updateSelectInput, the last else in d_panel will never be true
  d_panel=reactive({ 
    
    d_panel=if (  !is.null( input_strato() ) ) d[giorni_mare>(input_check_gio()-1) & var %in% input_var() & id_strato %in% input_strato() ]
            else if ( !is.null(input_codsis()) &  is.null(input_codlft()) )  d[giorni_mare>(input_check_gio()-1) & var %in% input_var() & codsis199 %in% input_codsis()]
            else if ( is.null(input_codsis()) &  !is.null(input_codlft()) )  d[giorni_mare>(input_check_gio()-1) & var %in% input_var() & codlft199 %in% input_codlft()]
            else if ( !is.null(input_codsis()) &  !is.null(input_codlft()) ) d[giorni_mare>(input_check_gio()-1) & var %in% input_var() & codsis199 %in% input_codsis() & codlft199 %in% input_codlft()]  
            else d[0]
    
    d_panel[,parameter:=as.numeric(0) ]
    d_panel[var %in% c('spmanu','alcofi','amm','indeb','invest'),parameter:=round(as.numeric(value)) ]
    d_panel[var %in% c('alcova','carbur','ricavi','ricavi_est') &  giorni_mare>0, parameter:=round(value/giorni_mare) ]
    d_panel[var=='lavoro' &  giorni_mare>0, parameter:=round(value/equipaggio_medio) ]
    
    ricavi=d_panel[var=='ricavi' & value>0, .(id_battello,ricavi=value)]
    setkey(d_panel, id_battello)
    setkey(ricavi, id_battello)
    d_panel=ricavi[d_panel]
    
    d_panel[var=='spcom' & ricavi>0 ,  parameter:=round(value/ricavi,3)  ]
    d_panel[,ricavi:=NULL]
    rm(ricavi)
    
    d_panel
  }) 
  
  # considering updateSelectInput, the last else in facet will never be true
  facet_vars=reactive({ if (  !is.null( input_strato() ) & ( is.null(input_codsis()) | is.null(input_codlft())) ) "id_strato" 
                        else if ( !is.null(input_codsis()) &  is.null(input_codlft()) ) "codsis199"
                        else if ( is.null(input_codsis()) &  !is.null(input_codlft()) ) "codlft199"
                        else if ( !is.null(input_codsis()) &  !is.null(input_codlft()) ) c("codsis199","codlft199") 
                        else ( c("id_strato","codsis199","codlft199")  )
                    })  
    
  d_pie=reactive({
    
    d_pie=d_panel()[,list(value=as.numeric(sum2(value)) ),  keyby=c(facet_vars(),'var')]
    d_pie2=d_pie[,list(tot=sum2(value) ), keyby=c(facet_vars())]      
    d_pie[d_pie2, value:=round(ifelse(tot==0,0,value/tot),3)]      
    d_pie[,pie_label_position:=cumsum(value), by=c(facet_vars() )]
    d_pie[,pie_label_position:=pie_label_position-.5*value]
    d_pie
    
  })
  
  d_outliers_value=reactive({
    
    d_outliers=d_panel()
    by_vars=c('codsis199','codlft199','id_strato','var')[ c('codsis199','codlft199','id_strato','var') %in% names(d_panel())]
    d_outliers2=d_panel()[,list( out_up=quantile(value,.75)+1.5*IQR(value), out_down=quantile(value,.25)-1.5*IQR(value) ),  keyby=.(var)]
    setkey(d_outliers, var )
    setkey(d_outliers2, var )
    
    d_outliers=d_outliers2[d_outliers][value>out_up | value<out_down][,c('out_up','out_down'):=NULL]
    d_outliers
    
  })
  
  d_outliers_parameter=reactive({
    
    d_outliers=d_panel()
    by_vars=c('codsis199','codlft199','id_strato','var')[ c('codsis199','codlft199','id_strato','var') %in% names(d_panel())]
    d_outliers2=d_panel()[,list( out_up=quantile(parameter,.75)+1.5*IQR(parameter), out_down=quantile(parameter,.25)-1.5*IQR(parameter) ),  keyby=.(var)]
    setkey(d_outliers, var )
    setkey(d_outliers2, var )
    
    d_outliers=d_outliers2[d_outliers][parameter>out_up | parameter<out_down][,c('out_up','out_down'):=NULL]
    d_outliers
    
  })
  
  output$pie = renderPlot({   
    if (nrow(d_pie())>0)  d_pie()[,ggplot(.SD, aes(x="",y=value,fill=var)) + geom_bar(stat="identity") + coord_polar(theta="y") + facet_wrap(~eval(parse(text= paste0(facet_vars(), collapse=" + " ) ))) + geom_text(aes(label = paste0(round(100*value,0), "%"), y=pie_label_position) )]
  })
  
  output$boxplot_value = renderPlot({
    d_panel()[ ,ggplot(.SD, aes(x= var,y=value)) +geom_boxplot(outlier.size=3 ,outlier.colour="red", fill="grey",colour = "blue") + xlab("") ]
  })
  
  output$boxplot_parameter = renderPlot({
    d_panel()[ ,ggplot(.SD, aes(x= var,y=parameter)) +geom_boxplot(outlier.size=3 ,outlier.colour="red", fill="grey",colour = "blue") + xlab("") ]
  })
  
  #output$table_data = renderTable({head(d_panel() )})
  output$table_outliers_value = renderDataTable({d_outliers_value()[,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,giorni_mare,value,parameter)]})
  output$table_outliers_parameter = renderDataTable({d_outliers_parameter()[,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,giorni_mare,parameter,value)]})
  output$pie_data = renderDataTable({d_pie()[,1:(ncol(d_pie() ) -1), with=F ] })
  output$table_free_filters=renderDataTable({  d[giorni_mare>(input_check_gio()-1) & var %in% input_var()] })
  output$table_consegne=renderDataTable({bat})
  output$perc_consegne_annuali=renderText({ perc_consegne_annuali })
  output$perc_consegne_mensili=renderText({ perc_consegne_mensili })
  
  #output$table_data2 = renderDataTable({ d_pie() })
  
})