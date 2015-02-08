# runapp: ####
# library(shiny)
# runApp("C:/Users/mdemeo/Documents/000/datacontrol",port = 12345)
# options(shiny.trace=T)


shinyServer(function(input, output, session) {
    
  # defining function ####
  source( paste(getwd(), "source/ini_fun.R", sep="/"),loc=T )
  
  # data import from mysql with a fake progress bar ####
  source( paste(getwd(), "source/refresh_data.R", sep="/"),loc=T )  
  
  # refresh data with an action button
  observe({ if (input$refresh > 0) source( paste(getwd(), "source/refresh_data.R", sep="/"),loc=T )  })
  
  output$var=renderUI({  selectInput("var", label = "Select the variables:", choices =var, selected = var, multiple = T ) })  
  output$codsis=renderUI({selectInput("codsis", label = "Apply a filter on gear type:", choices =codsis, selected = codsis, multiple = T ) })
  output$codlft=renderUI({ selectInput("codlft", label = "Apply a filter on LOA:", choices =codlft, selected = codlft, multiple = T ) })
  
  output$strato=renderUI({ selectInput("strato", label = "Select the strata:", choices =strato, selected = NULL, multiple = T ) })
  
  output$nr_strato=renderUI({ selectInput("nr_strato", label = "Select the strata:", choices =strato, selected = NULL, multiple = T ) })
  
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
    
    d_panel=if (  !is.null( input_strato() ) ) all[giorni_mare>(input_check_gio()-1) & id_strato %in% input_strato() ]
              else if ( !is.null(input_codsis()) &  is.null(input_codlft()) )  all[giorni_mare>(input_check_gio()-1)  & codsis199 %in% input_codsis()]
              else if ( is.null(input_codsis()) &  !is.null(input_codlft()) )  all[giorni_mare>(input_check_gio()-1)  & codlft199 %in% input_codlft()]
              else if ( !is.null(input_codsis()) &  !is.null(input_codlft()) ) all[giorni_mare>(input_check_gio()-1)  & codsis199 %in% input_codsis() & codlft199 %in% input_codlft()]  
              else all[0]
    
    if (input$headtab <=2) d_panel[ var %in% input_var() ] else d_panel
    
  })
  
  ok_rows=reactive({nrow(d_panel())>0})
    
  # considering updateSelectInput, the last else in facet will never be true
  facet_vars=reactive({ if (  !is.null( input_strato() ) & ( is.null(input_codsis()) | is.null(input_codlft())) ) "id_strato" 
                        else if ( !is.null(input_codsis()) &  is.null(input_codlft()) ) "codsis199"
                        else if ( is.null(input_codsis()) &  !is.null(input_codlft()) ) "codlft199"
                        else if ( !is.null(input_codsis()) &  !is.null(input_codlft()) ) c("codsis199","codlft199") 
                        else ( c("id_strato","codsis199","codlft199")  )
                    })  
    
  d_pie=reactive({
    
    d_pie = 
      if(input_apply_weights()==TRUE) 
        d_panel()[var %in% c("alcova","carbur","lavoro","spcom","spmanu","alcofi" ) , list(id_strato,codsis199,codlft199,var,value=value/pr_i) ]
      else  
        d_panel()[var %in% c("alcova","carbur","lavoro","spcom","spmanu","alcofi" ) , list(id_strato,codsis199,codlft199,var,value) ]
    
    d_pie=d_pie[,list(value=as.numeric(sum2(value)) ),  keyby=c(facet_vars(),'var')]
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
  
  # definisci i waterfall data.table ####
  source( paste(getwd(), "source/waterfall.R", sep="/"),loc=T )
  
  output$pie = renderPlot({   
    if (nrow(d_pie())>0)  d_pie()[,ggplot(.SD, aes(x="",y=value,fill=var)) + geom_bar(stat="identity") + coord_polar(theta="y") + facet_wrap(~eval(parse(text= paste0(facet_vars(), collapse=" + " ) ))) + geom_text(aes(label = paste0(round(100*value,0), "%"), y=pie_label_position) )]
  })
  
  output$boxplot_value = renderPlot({
    d_panel()[ ,ggplot(.SD, aes(x= var,y=value)) +geom_boxplot(outlier.size=3 ,outlier.colour="red", fill="grey",colour = "blue") + xlab("") ]
  })
  
  output$boxplot_parameter = renderPlot({
    d_panel()[ ,ggplot(.SD, aes(x= var,y=parameter)) +geom_boxplot(outlier.size=3 ,outlier.colour="red", fill="grey",colour = "blue") + xlab("") ]
  })
  
  output$waterfall_plot_italy = renderPlot({
    d_waterfall_italy()[,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(value,big.mark = "."))) + theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") ]
  })
  
  output$waterfall_plot_strata = renderPlot({
    
    if(ok_rows()) {
      str_in_panel=d_waterfall_strata()[,unique(id_strato)]
      
      if (length(str_in_panel)<14 )  {
        
        d_waterfall_strata()[,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(value,big.mark = "."))) +facet_wrap(~id_strato, ncol=2,scales = "free") +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") ]
        
      } else {
        
        d_waterfall_strata()[ id_strato %in% str_in_panel[1:14] ,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(value,big.mark = "."))) +facet_wrap(~id_strato, ncol=2,scales = "free") +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") ]
        
      }  
      
    }
    
    })      
  
  output$waterfall_plot_gear_loa = renderPlot({
    
    if(ok_rows()) {
      
      sis_lft_in_panel=d_waterfall_gear_loa()[,.N,list(codsis199,codlft199)]
      
      if ( nrow(sis_lft_in_panel)<14 )  {
        d_waterfall_gear_loa()[,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(value,big.mark = "."))) +facet_wrap(~ codsis199+codlft199, scales = "free",ncol = 2) +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") ]
        
      } else {
        
        u_sis = sis_lft_in_panel[1:14, unique(codsis199)]
        u_lft = sis_lft_in_panel[1:14, unique(codlft199)]
        
        d_waterfall_gear_loa()[ codsis199 %in% u_sis & codlft199 %in% u_lft,
                                ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(value,big.mark = "."))) +facet_wrap(~ codsis199+codlft199, scales = "free",ncol = 2) +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") ]
        
      }     
      
    }
  
  })
  
  output$waterfall_plot_gear = renderPlot({
    d_waterfall_gear()[,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(value,big.mark = "."))) +facet_wrap(~codsis199,scales = "free",ncol = 2) +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") ]
  })
  output$waterfall_plot_loa = renderPlot({
    d_waterfall_loa()[,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(value,big.mark = "."))) +facet_wrap(~codlft199,scales = "free", ncol=2) +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") ]
  })
  
  #output$table_data = renderTable({head(d_panel() )})
  output$table_outliers_value = renderDataTable({d_outliers_value()[,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,giorni_mare,value,parameter)]})
  output$table_outliers_parameter = renderDataTable({d_outliers_parameter()[,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,giorni_mare,parameter,value)]})
  output$pie_data = renderDataTable({d_pie()[,1:(ncol(d_pie() ) -1), with=F ] })
  output$table_free_filters=renderDataTable({  all[giorni_mare>(input_check_gio()-1) & var %in% input_var()] })
  output$table_consegne=renderDataTable({bat_ril})
  output$perc_consegne_annuali=renderText({ perc_consegne_annuali })
  output$perc_consegne_mensili=renderText({ perc_consegne_mensili })
  
  #output$uti = renderDataTable({ d_waterfall_italy() })
  
})