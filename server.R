# runapp: ####
# library(shiny)
# runApp("C:/Users/mdemeo/Documents/000/datacontrol",port = 12345,quiet = T)
#options(shiny.trace=T)

# crea nicoda temp dir
temp_dir_nicoda=paste0(Sys.getenv("LOCALAPPDATA"),"\\Nicoda")
unlink(temp_dir_nicoda, recursive = T, force = T)
dir.create(temp_dir_nicoda)

shinyServer(function(input, output, session) {
  
  # data import from mysql with a fake progress bar ####
  source( paste(getwd(), "source/data_download.R", sep="/"),loc=T )  
    
  output$var=renderUI({  selectInput("var", label = "Select the variables:", choices =var, selected = var, multiple = T ) })
  output$codsis=renderUI({selectInput("codsis", label = "Gear:", choices =codsis, selected = codsis, multiple = T ) })
  output$codlft=renderUI({ selectInput("codlft", label = "LOA:", choices =codlft, selected = codlft, multiple = T ) })  
  output$regione=renderUI({ selectInput("regione", label = "Region:", choices =regione, selected = regione, multiple = T ) })
  output$gsa=renderUI({ selectInput("gsa", label = "GSA:", choices =gsa, selected = gsa, multiple = T ) })
  output$strato=renderUI({ selectInput("strato", label = "Filter by strata:", choices =strato, selected = NULL, multiple = T ) })
  output$ril=renderUI({ selectInput("ril", label = "Filter by ril:", choices =ril, selected = NULL, multiple = T ) })
  #UI for imputation:
  output$var_imp=renderUI({  selectInput("var_imp", label = "Select the variables:", choices =var, selected = var, multiple = T ) })
  output$codsis_imp=renderUI({selectInput("codsis_imp", label = "Apply a filter on gear type:", choices =codsis, selected = codsis, multiple = T ) })
  output$codlft_imp=renderUI({ selectInput("codlft_imp", label = "Apply a filter on LOA:", choices =codlft, selected = codlft, multiple = T ) })  
  output$strato_imp=renderUI({ selectInput("strato_imp", label = "Select the strata:", choices =strato, selected = NULL, multiple = T ) })
  # UI for imputation manual
  output$var_imp_m=renderUI({  selectInput("var_imp_m", label = "Variables:", choices =var_imp_m, selected = var, multiple = T ) })
  output$strato_imp_m=renderUI({ selectInput("strato_imp_m", label = "Strata:", choices =strato, selected = NULL, multiple = T ) })
  output$id_battello_imp_m=renderUI({ selectInput("id_battello_imp_m", label = "id_battello:", choices =id_battello_imp_m, selected = NULL, multiple = T ) })
  
  
  observe({ if (!is.null(input_codsis_imp()) | !is.null(input_codlft_imp())  ) updateSelectInput(session, "strato_imp", choices =strato, selected = NULL) })
  observe({ 
    if (!is.null(input_strato_imp()) ) {
      updateSelectInput(session, "codsis_imp", choices =codsis, selected = NULL) 
      updateSelectInput(session, "codlft_imp", choices =codlft, selected = NULL)
    }
  })
  # update of input strato,sis,loa,ril ####
  observe({ 
    if (!is.null(input_codsis()) | !is.null(input_codlft()) | !is.null(input_regione()) | !is.null(input_gsa())  ) {
      updateSelectInput(session, "strato", choices =strato, selected = NULL) 
      updateSelectInput(session, "ril", choices =ril, selected = NULL)   
    } 
  })
  observe({ 
    if (!is.null(input_strato()) | !is.null(input_ril()) ) {
      updateSelectInput(session, "codsis", choices =codsis, selected = NULL) 
      updateSelectInput(session, "codlft", choices =codlft, selected = NULL)
      updateSelectInput(session, "regione", choices =regione, selected = NULL)
      updateSelectInput(session, "gsa", choices =gsa, selected = NULL)
    }
  })
  observe({ if (!is.null(input_strato()))  updateSelectInput(session, "ril", choices =ril, selected = NULL)    })
  observe({ if (!is.null(input_ril()))  updateSelectInput(session, "strato", choices =strato, selected = NULL)     })
  
  observe({ 
    if (input$reset>0) {
      updateSelectInput(session, "codsis", choices =codsis, selected = codsis) 
      updateSelectInput(session, "codlft", choices =codlft, selected = codlft)
      updateSelectInput(session, "regione", choices =regione, selected = regione)
      updateSelectInput(session, "gsa", choices =gsa, selected = gsa)
    }
  })
  
  # mediante conditional panel, not_sent_to_0 scompare quando input_check_gio()==1 
  observe({ if( input_check_gio()==1 ) updateCheckboxInput( session, "not_sent_as_0", value = 0) })
  
  selected_strata=reactive({ 
    if (  !is.null( input_strato() ) ) as.numeric(input_strato())
    else if ( !is.null( input_ril() ) ) ril_strato[.(input_ril()), unique(id_strato) ]
    else str_sis_lft_reg_gsa[codsis199 %in%  input_codsis_all() & codlft199 %in%  input_codlft_all() & regione %in% input_regione_all() & gsa %in% input_gsa_all(),
                             unique(id_strato)]
  })
  
  selected_strata_in_imp=reactive({ 
    if (  !is.null( input_strato_imp() ) ) as.numeric(input_strato_imp())
    else str_sis_lft_reg_gsa[codsis199 %in%  input_codsis_imp() & codlft199 %in%  input_codlft_imp(), unique(id_strato)]
  })
  
  # d_panel ####
  # considering updateSelectInput, the last else in d_panel will never be true
  d_panel=reactive({ 
    
   input_freeze_data()
   input_slider_imp_m()
   
   if (input_show_output()=='orig_data') all[,(c('value','parameter')):=list(value_or,parameter_or)] else all[,(c('value','parameter')):=list(value_ok,parameter_ok)]
    
    d_panel=if (  !is.null( input_strato() ) ) all[giorni_mare>(input_check_gio()-1) & id_strato %in% input_strato() ]
    else if ( !is.null( input_ril() ) ) all[giorni_mare>(input_check_gio()-1) & id_rilevatore %in% input_ril() ]
    else all[giorni_mare>(input_check_gio()-1) & codsis199 %in% input_codsis_all() & codlft199 %in%  input_codlft_all() & regione %in% input_regione_all() & gsa %in% input_gsa_all()]
    
    setkey(d_panel,id_battello)
    d_panel=pr_i[d_panel]
    d_panel[is.na(pr_i), pr_i:=Inf]
    
    if( input_not_sent_as_0()==0 ) {
      d_panel=d_panel[sent==1]
      
    } else { # input_not_sent_as_0()==1 quindi refresh pr_i
      
      # genero il pr_i_temp per il refresh dei soli strati selezionai
      source( paste(getwd(), "source/refresh_pr_i.R", sep="/"),loc=T )      
      # join con d_panel e update di pr_i ####
      #  apparentemente non sarebbe necessario un setkey, essendo già eseguito prima, ma se lo tolgo la key mi cambia con la generazione delle waterfall (non chiaro il motivo)
      setkey(d_panel, id_battello)
      d_panel[pr_i_temp, pr_i:=i.pr_i, nomatch=0]
      rm(pr_i_temp) 
    }
   
    if (input$headtab <=2 | input$headtab>=6) d_panel[ var %in% input_var() ] else d_panel
  })

  # downloadHandler in the project ####
  source( paste(getwd(), "source/download_shiny_objects.R", sep="/"),loc=T )
  
  ok_rows=reactive({nrow(d_panel())>0})
  
  facet_vars=reactive({ 
    c("id_strato","id_rilevatore","codsis199","codlft199","regione","gsa")[
      which(
        c(!is.null(input_strato()), !is.null(input_ril()), !is.null(input_codsis()), !is.null(input_codlft()), !is.null(input_regione()), !is.null(input_gsa()))
      )
      ]
  })  
  
  d_pie=reactive({
    
    d_pie = 
      if(input_apply_weights()==TRUE) 
        d_panel()[var %in% c("alcova","carbur","lavoro","spcom","spmanu","alcofi" ) , list(id_strato,id_rilevatore,codsis199,codlft199,regione,gsa,var,value=value/pr_i) ]
    else  
      d_panel()[var %in% c("alcova","carbur","lavoro","spcom","spmanu","alcofi" ) , list(id_strato,id_rilevatore,codsis199,codlft199,regione,gsa,var,value) ]
    d_pie=d_pie[,list(value=as.numeric(sum2(value)) ),  keyby=c(facet_vars(),'var')]
    d_pie2=d_pie[,list(tot=sum2(value) ), keyby=c(facet_vars())]      
    d_pie[d_pie2, value:=round(ifelse(tot==0,0,value/tot),3)]      
    d_pie[,pie_label_position:=cumsum(value), by=c(facet_vars() )]
    d_pie[,pie_label_position:=pie_label_position-.5*value]
    setkeyv(d_pie, facet_vars() )
    d_pie 
  })
  
  source( paste(getwd(), "source/download_shiny_objects.R", sep="/"),loc=T )
  
  d_outliers_value=reactive({
    
    d_outliers=d_panel()[,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,giorni_mare,value,is_ok,value_ok,value_or)]
    d_outliers2=d_outliers[,list( out_up=quantile(value,.75)+1.5*IQR(value), out_down=quantile(value,.25)-1.5*IQR(value) ),  keyby=.(var)]
    setkey(d_outliers, var )
    setkey(d_outliers2, var )    
    d_outliers2[d_outliers][value>out_up | value<out_down][,c('out_up','out_down'):=NULL]
    
  })
  
  
  d_outliers_parameter=reactive({

    d_outliers=d_panel()[,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,giorni_mare,value,is_ok,value_ok,parameter,value_or)]
    d_outliers2=d_outliers[,list( out_up=quantile(parameter,.75)+1.5*IQR(parameter), out_down=quantile(parameter,.25)-1.5*IQR(parameter) ),  keyby=.(var)]
    setkey(d_outliers, var )
    setkey(d_outliers2, var )    
    d_outliers2[d_outliers][parameter>out_up | parameter<out_down][,c('out_up','out_down'):=NULL]
    
  })
  
  
  # definisci i waterfall data.table ####
  source( paste(getwd(), "source/waterfall.R", sep="/"),loc=T )
  
  output$pie = renderPlot({    
    if(nrow(d_pie())>0){
    
      d_pie_filter=d_pie()[,.N,keyby=setdiff(facet_vars(),"var")][,N:=NULL]
      if(nrow(d_pie_filter)>10) {
        d_pie_filter=d_pie_filter[1:10]
        setkeyv(d_pie_filter, setdiff(facet_vars(),"var"))
      }
      current_plot=d_pie()[d_pie_filter,ggplot(.SD, aes(x="",y=value,fill=var)) + geom_bar(stat="identity") + coord_polar(theta="y") + facet_wrap(~eval(parse(text= paste0(facet_vars(), collapse=" + " ) )),ncol=5) + geom_text(aes(label = paste0(round(100*value,0), "%"), y=pie_label_position) )]
      ggsave("pie_chart.png",current_plot)
      current_plot
    }
  })
 
  output$boxplot_value = renderPlot({
    current_plot=d_panel()[ ,ggplot(.SD, aes(x= var,y=value)) +geom_boxplot(outlier.size=3 ,outlier.colour="red", fill="grey",colour = "blue") + xlab("")]
    ggsave("boxplot_outliers_abs.png",current_plot)
    current_plot
  })
  
  output$boxplot_parameter = renderPlot({
    current_plot=d_panel()[ ,ggplot(.SD, aes(x= var,y=parameter)) +geom_boxplot(outlier.size=3 ,outlier.colour="red", fill="grey",colour = "blue") + xlab("") ]
    ggsave("boxplot_outliers_parameter.png",current_plot)
    current_plot
  })
  
  output$waterfall_plot_italy = renderPlot({
    current_plot=d_waterfall_italy()[,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(round(value/1000),big.mark = "."))) + theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") + guides(fill=guide_legend(title="1.000 € \n" )) + geom_text(aes(o, 0,label=yoy)) ]
    ggsave("waterfall_italy.png",current_plot)
    current_plot
  })
  
  output$waterfall_plot_strata = renderPlot({
    
    if(ok_rows()) {
      str_in_panel=d_waterfall_strata()[,unique(id_strato)]
      
      current_plot=
      if (length(str_in_panel)<14 )  {
        
          d_waterfall_strata()[,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(round(value/1000),big.mark = "."))) +facet_wrap(~id_strato, ncol=2,scales = "free") +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") + guides(fill=guide_legend(title="1.000 € \n" )) + geom_text(aes(o, 0,label=yoy, size=4))  ]
        
      } else {
        
        d_waterfall_strata()[ id_strato %in% str_in_panel[1:14] ,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(round(value/1000),big.mark = "."))) +facet_wrap(~id_strato, ncol=2,scales = "free") +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") + guides(fill=guide_legend(title="1.000 € \n" )) + geom_text(aes(o, 0,label=yoy),size=4)  ]
        
      }
      ggsave("waterfall_strata.png",current_plot)
      current_plot
      
    }
    
  })      
  
  output$waterfall_plot_gear_loa = renderPlot({
    
    if(ok_rows()) {
      
      sis_lft_in_panel=d_waterfall_gear_loa()[,.N,list(codsis199,codlft199)]
      
      current_plot=
      if ( nrow(sis_lft_in_panel)<14 )  {
        d_waterfall_gear_loa()[,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(round(value/1000),big.mark = "."))) +facet_wrap(~ codsis199+codlft199, scales = "free",ncol = 2) +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") + guides(fill=guide_legend(title="1.000 € \n" )) + geom_text(aes(o, 0,label=yoy), size=4) ]
        
      } else {
        
        u_sis_flt = sis_lft_in_panel[1:14, unique(paste0(codsis199,codlft199))]
        
        d_waterfall_gear_loa()[ paste0(codsis199,codlft199) %in% u_sis_flt,
                                ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(round(value/1000),big.mark = "."))) +facet_wrap(~ codsis199+codlft199, scales = "free",ncol = 2) +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") + guides(fill=guide_legend(title="1.000 € \n" )) + geom_text(aes(o, 0,label=yoy), size=4) ]
        
      }
      
      ggsave("waterfall_plot_gear_loa.png",current_plot)
      current_plot
      
    }
    
  })
  
  output$waterfall_plot_gear = renderPlot({
    current_plot=
    d_waterfall_gear()[,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(round(value/1000),big.mark = "."))) +facet_wrap(~codsis199,scales = "free",ncol = 2) +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") + guides(fill=guide_legend(title="1.000 € \n" )) + geom_text(aes(o, 0,label=yoy)) ]
    ggsave("waterfall_plot_gear.png",current_plot)
    current_plot
  })
  output$waterfall_plot_loa = renderPlot({
    current_plot=
    d_waterfall_loa()[,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(round(value/1000),big.mark = "."))) +facet_wrap(~codlft199,scales = "free", ncol=2) +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") + guides(fill=guide_legend(title="1.000 € \n" )) + geom_text(aes(o, 0,label=yoy))]
    ggsave("waterfall_plot_loa.png",current_plot)
    current_plot
  })
  output$waterfall_plot_regione = renderPlot({
    
    if(ok_rows()) {      
      regione_in_panel=d_waterfall_regione()[,unique(regione)]
      current_plot=
      if(length(regione_in_panel)>10) {
        
        d_waterfall_regione()[regione %in% regione_in_panel[1:10],
  ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(round(value/1000),big.mark = "."))) +facet_wrap(~regione,scales = "free", ncol=2) +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") + guides(fill=guide_legend(title="1.000 € \n" )) + geom_text(aes(o, 0,label=yoy))]
        
      } else {
        
        d_waterfall_regione()[,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(round(value/1000),big.mark = "."))) +facet_wrap(~regione,scales = "free", ncol=2) +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") + guides(fill=guide_legend(title="1.000 € \n" )) + geom_text(aes(o, 0,label=yoy))]
        
      }
      ggsave("waterfall_plot_regione.png",current_plot)
      current_plot
    }    
  })
  output$waterfall_plot_gsa = renderPlot({
    
    if(ok_rows()) {      
      gsa_in_panel=d_waterfall_gsa()[,unique(gsa)]
      current_plot=
      if(length(gsa_in_panel)>10) {
        
        d_waterfall_gsa()[gsa %in% gsa_in_panel[1:10],
                          ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(round(value/1000),big.mark = "."))) +facet_wrap(~gsa,scales = "free", ncol=2) +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") + guides(fill=guide_legend(title="1.000 € \n" )) + geom_text(aes(o, 0,label=yoy))]
        
      } else {
        
        d_waterfall_gsa()[,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(round(value/1000),big.mark = "."))) +facet_wrap(~gsa,scales = "free", ncol=2) +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") + xlab("") + guides(fill=guide_legend(title="1.000 € \n" )) + geom_text(aes(o, 0,label=yoy))]
        
      }
      ggsave("waterfall_plot_gsa.png",current_plot)
      current_plot
    }    
  })
  
  # genera dataset da esportare in "Checks on zero values" tabset ####
  zero_checks=reactive({ d_panel()[value==0 & sent==1 , setdiff(names(d_panel()),c('sent','parameter','pr_i')), with=F ] })
  not_sent=reactive({ all[sent==0 , .N ,list(id_battello,numero_ue,id_rilevatore,id_strato,regione,codsis199,codlft199,gsa,descrizione) ][,N:=NULL] })  
  output$zero_checks_dt = renderDataTable({ zero_checks() })
  output$not_sent_dt = renderDataTable({ not_sent() })
  
# imputation process #####
source( paste(getwd(), "source/imputation.R", sep="/"),loc=T )
  
observe({ if (input_freeze_data()=='yes') all[,(c('value','parameter')):=list(value_ok,parameter_ok)] else all[,(c('value','parameter')):=list(value_or,parameter_or)] })

observe({ if (input_start_imputation()==0) updateRadioButtons(session, 'freeze_data', selected = 'no') })

observe({  
  if (input_freeze_data()=='yes' & input_start_imputation()==1) {    
    if (input_abs_or_mean_in_fix()=="abs-outliers") updateTabsetPanel(session, "headtab" ,selected = '1') else updateTabsetPanel(session, "headtab" ,selected = '2')
    updateCheckboxGroupInput(session,'show_output',selected = 'imput_data')
    updateSelectInput(session, 'strato',selected = input_strato_imp() )
    updateSelectInput(session, 'var',selected = input_var_imp() )
    updateSelectInput(session, 'codsis',selected = input_codsis_imp() )
    updateSelectInput(session, 'codlft',selected = input_codlft_imp() )
  } 
})

# update of manual imputation tab if imputation or closing_session is selected  ####
observe({ if (input$headtab %in% c(8,10)) { 
  updateSelectInput(session, 'slider_imp_m',selected = 0 ) 
  updateSelectInput(session, 'abs_imp_m',selected = 0 )
  updateSelectInput(session, 'strato_imp_m',selected = NA )
  updateSelectInput(session, 'id_battello_imp_m',selected = NA )
  } 
  })
# update of imputation tab if manual imputation is selected ####
observe({  if (input$headtab %in% c(9,10) ) updateCheckboxInput( session, "start_imputation", value = 0) })


# imputation upload with button and nicoda restart ####
observe({ 
  if (nrow(all[grepl(session_info, notes,fixed = T)])>0 & input$upload_button==T) {
    withProgress(message = "Uploadig data to remote server:",{
      n=20
      
      up=all[is_ok==1 & grepl(session_info,notes,fixed = T),  .(var,id_strato,id_battello,value_ok,parameter_ok,notes)]
      hist2=hist[! (id_battello %in% up[,unique(id_battello)] & var %in% up[,unique(var)]) ][,id:=NULL]
      
      setkey(up,id_battello)
      up=pr_i[up]
      up[is.na(pr_i), pr_i:=0] # qui non metto inf perché poi salvo nel db
      
      # refresh di pr_i
      if( input_not_sent_as_0()==1 ) {
        # genero il pr_i_temp per il refresh dei soli strati selezionai in imputation o manual imputation
        if (input$headtab==8) {
          source( paste(getwd(), "source/refresh_pr_i_imp.R", sep="/"),loc=T )
        } else if(input$headtab==9) {
          source( paste(getwd(), "source/refresh_pr_i_imp_m.R", sep="/"),loc=T )
        }
        
        setkey(up, id_battello)
        up[pr_i_temp, pr_i:=i.pr_i, nomatch=0]
      } # if( input_not_sent_as_0()==1 )
      
      up=up[,list(id_battello,var,day=Sys.Date(),year=strftime(Sys.Date(),"%Y"),pr_i=round(pr_i,8),hist_value=value_ok,hist_parameter=parameter_ok,hist_notes=notes,closing_session="open")]
      
      if(nrow(hist2)>0) up=rbindlist(list( up,hist2 ))
      
      write.table(up, paste0(temp_dir_nicoda,"\\nicoda.csv"), sep=";", quote = FALSE, na = "", row.names = F,col.names = F)
      ftp(action="put")
      if (input$headtab==8)  updateTabsetPanel(session, "headtab" ,selected = 9) else updateTabsetPanel(session, "headtab" ,selected = 8)
      pid=data.table(system("tasklist /V",intern = T))[grepl("127.0.0.1:12345",V1),V1]
      pid=regmatches(pid, regexpr("\\d+(?=\\s*Console)",pid,perl=T))
      for (i in 1:n) {
        incProgress(1/n, detail =  sample(9000:70000,1) )
        Sys.sleep(.1)
       }
    }) # withProgress     
     
     system(paste("taskkill /pid",pid),wait = T)
     Sys.sleep(2)
     browseURL("http://127.0.0.1:12345/")
     #system(paste("taskkill /pid",pid,"start http://127.0.0.1:12345/"))
  }
  
})

# manual imputation process #####
source( paste(getwd(), "source/imputation_manual.R", sep="/"),loc=T )

observe ({ 
  input$headtab
  updateCheckboxInput(session, 'upload_button', value = F)
  if (input_show_output()=='orig_data' & input_start_imputation()==1) updateCheckboxGroupInput(session,'freeze_data',selected = 'no')
})

observe ({ if (input$headtab==4)  updateCheckboxInput(session, 'apply_weights', value = T)   })

# export data in closing sessions ####
source( paste(getwd(), "source/closing_sessions.R", sep="/"),loc=T )

# sample rate ####
source( paste(getwd(), "source/sample_rate.R", sep="/"),loc=T )

# objects to show in output ####
  output$table_outliers_value = renderDataTable({d_outliers_value()[,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,giorni_mare,original_value=value_or,is_ok,final_value=ifelse(is_ok==1,value_ok,NA))]})
  output$table_outliers_parameter = renderDataTable({ d_outliers_parameter()[,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,giorni_mare,parameter,original_value=value_or,is_ok,final_value=ifelse(is_ok==1,value_ok,NA))]})
  output$pie_data = renderDataTable({d_pie()[,1:(ncol(d_pie() ) -1), with=F ] })
  output$table_free_filters=renderDataTable({  all[giorni_mare>(input_check_gio()-1) ] })
  output$table_consegne_ril=renderDataTable({bat_ril})
  output$table_consegne_strato=renderDataTable({bat_str})
  output$perc_consegne_annuali=renderText({ perc_consegne_annuali })
  output$perc_consegne_mensili=renderText({ perc_consegne_mensili })
output$notes_on_fixing=renderText({ 
    out=
    if (  !is.null( input_strato() ) ) paste(input_strato(),collapse = ",")
    else if ( !is.null(input_codsis()) &  is.null(input_codlft()) ) paste0(input_codsis(),collapse = ",")
    else if ( is.null(input_codsis()) &  !is.null(input_codlft()) )  paste0(input_codlft(),collapse = ",") 
    else if ( !is.null(input_codsis()) &  !is.null(input_codlft()) ) paste( paste0(input_codsis(),collapse = ",") , paste0(input_codlft(),collapse = ",") ,sep = " and " )
    else "NOTHING"      
    out= paste(" Fixing outliers for STRATA: \n", out, "\n and VARIABLES: \n", paste(input_var(),collapse = ",") )
    out= paste(out, "\n applying the Tukey's non-parametric distribution of", input_abs_or_mean_in_fix() ,".\n")    
    out= paste(out, if( input_subset_units()=='all' ) "You are considering all units selected in the initial tab" else paste("You are subsetting the following units:", paste( input_outliers_id_battello_list_to_subset() , collapse="," )) ,  ".\n"  )  
    out=paste(out,
      if(input_keep_accept_refuse_outliers()=='keep') {
        "You are keeping the situation as it is ."     
      }  else if( input_keep_accept_refuse_outliers()=='accept' ) {
        "You are accepting outliers as ok-values."
      } else {
        paste("You are applying the foollowing model to correct outliers:\n", input_imputation_method() ,"for data gouped by", input_group_for_imputation_method(),"." )
        
      }
    )  
    out
  
  })
output$table_universe_data=renderDataTable({universe_data()})
output$table_close_strata=renderDataTable({universe_data()[id_strato %in% c(input_strato_close(),input_strato_open())] })
output$table_sample_rate=renderDataTable({sample_data_react()})

output$version_nr=renderText({ '2.2.220' }) # 
#output$uti=renderText({ input_strato_open()[1] })  
  
}) #shinyServer
