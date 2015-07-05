
output$imp_m_sample=renderDataTable({ 
  
  if(input_imputation_manual_method()=='abs_change') {
    
    # con un ifelse sulla condizione where NEL data.table, non funziona l'aggioramento! quindi lascio due aggiornamenti consecutivi
    all[giorni_mare>(input_check_gio()-1) & (id_strato %in% input_strato_imp_m() | id_battello %in% input_id_battello_imp_m()) & var %in% input_var_imp_m(), 
        (c('value_ok','parameter_ok','notes','is_ok')):=list(round(ifelse((input_abs_imp_m() + value_or)<0,0,input_abs_imp_m() + value_or),0),
                                                             round( parameter_or,0),
                                                             paste(session_info,paste0("__MANUAL IMPUTATION__",input_slider_imp_m(),""),sep = "|"),
                                                             1
        )]
    all[!(giorni_mare>(input_check_gio()-1) & (id_strato %in% input_strato_imp_m() | id_battello %in% input_id_battello_imp_m()) & var %in% input_var_imp_m()), 
        (c('value_ok','parameter_ok','notes','is_ok')):=list( ifelse(is.na(hist_value),value_or,hist_value),
                                                              ifelse(is.na(hist_value),parameter_or,hist_parameter),
                                                              ifelse(is.na(hist_value),"",hist_notes),
                                                              ifelse(is.na(hist_value),0,hist_is_ok)
        )]
  } else {
    # con un ifelse sulla condizione where NEL data.table, non funziona l'aggioramento! quindi lascio due aggiornamenti consecutivi
    all[giorni_mare>(input_check_gio()-1) & (id_strato %in% input_strato_imp_m() | id_battello %in% input_id_battello_imp_m())  & var %in% input_var_imp_m(), 
        (c('value_ok','parameter_ok','notes','is_ok')):=list(round(((1+input_slider_imp_m()/100) * value_or),0),
                                                             round(((1+input_slider_imp_m()/100) * parameter_or),0),
                                                             paste(session_info,paste0("__MANUAL IMPUTATION__ ",input_slider_imp_m(),"%"),sep = "|"),
                                                             1
        )]
    all[!(giorni_mare>(input_check_gio()-1) & (id_strato %in% input_strato_imp_m() | id_battello %in% input_id_battello_imp_m())  & var %in% input_var_imp_m()), 
        (c('value_ok','parameter_ok','notes','is_ok')):=list( ifelse(is.na(hist_value),value_or,hist_value),
                                                              ifelse(is.na(hist_value),parameter_or,hist_parameter),
                                                              ifelse(is.na(hist_value),"",hist_notes),
                                                              ifelse(is.na(hist_value),0,hist_is_ok)
        )]
    
  }
  
  
  imp_m_sample=all[giorni_mare>(input_check_gio()-1) & (id_strato %in% input_strato_imp_m() | id_battello%in% input_id_battello_imp_m())  & var %in% input_var_imp_m(), list(id_battello,id_strato,var,value_or,value_ok,giorni_mare,notes,is_ok)]
  
  imp_m_sample
  
})

output$imp_m_universe=renderDataTable({ 
  
  if(input_imputation_manual_method()=='abs_change') {
    
    # con un ifelse sulla condizione where NEL data.table, non funziona l'aggioramento! quindi lascio due aggiornamenti consecutivi
    all[giorni_mare>(input_check_gio()-1) & (id_strato %in% input_strato_imp_m() | id_battello %in% input_id_battello_imp_m()) & var %in% input_var_imp_m(), 
        (c('value_ok','parameter_ok','notes','is_ok')):=list(round(ifelse((input_abs_imp_m() + value_or)<0,0,input_abs_imp_m() + value_or),0),
                                                             round( parameter_or,0),
                                                             paste(session_info,paste0("__MANUAL IMPUTATION__",input_slider_imp_m(),""),sep = "|"),
                                                             1
        )]
    all[!(giorni_mare>(input_check_gio()-1) & (id_strato %in% input_strato_imp_m() | id_battello %in% input_id_battello_imp_m()) & var %in% input_var_imp_m()), 
        (c('value_ok','parameter_ok','notes','is_ok')):=list( ifelse(is.na(hist_value),value_or,hist_value),
                                                              ifelse(is.na(hist_value),parameter_or,hist_parameter),
                                                              ifelse(is.na(hist_value),"",hist_notes),
                                                              ifelse(is.na(hist_value),0,hist_is_ok)
        )]
  } else {
    # con un ifelse sulla condizione where NEL data.table, non funziona l'aggioramento! quindi lascio due aggiornamenti consecutivi
    all[giorni_mare>(input_check_gio()-1) & (id_strato %in% input_strato_imp_m() | id_battello %in% input_id_battello_imp_m())  & var %in% input_var_imp_m(), 
        (c('value_ok','parameter_ok','notes','is_ok')):=list(round(((1+input_slider_imp_m()/100) * value_or),0),
                                                             round(((1+input_slider_imp_m()/100) * parameter_or),0),
                                                             paste(session_info,paste0("__MANUAL IMPUTATION__ ",input_slider_imp_m(),"%"),sep = "|"),
                                                             1
        )]
    all[!(giorni_mare>(input_check_gio()-1) & (id_strato %in% input_strato_imp_m() | id_battello %in% input_id_battello_imp_m())  & var %in% input_var_imp_m()), 
        (c('value_ok','parameter_ok','notes','is_ok')):=list( ifelse(is.na(hist_value),value_or,hist_value),
                                                              ifelse(is.na(hist_value),parameter_or,hist_parameter),
                                                              ifelse(is.na(hist_value),"",hist_notes),
                                                              ifelse(is.na(hist_value),0,hist_is_ok)
        )]
    
  }
  
  str_in_selection=all[id_battello %in% input_id_battello_imp_m() | id_strato %in% input_strato_imp_m(), unique(id_strato)]
  imp_m_universe=all[id_strato %in% str_in_selection & var %in% c('carbur','alcova','spcom','alcofi','spmanu','lavoro','ricavi') , list(id_battello,id_strato,var,value_ok)]
  
  if(nrow(imp_m_universe)>0){
    setkey(imp_m_universe,id_battello)
    setkey(pr_i,id_battello)
    imp_m_universe=pr_i[imp_m_universe]
    
    # refresh di pr_i
    if( input_not_sent_as_0()==1 ) {
      # genero il pr_i_temp per il refresh dei soli strati selezionai in imputation
      source( paste(getwd(), "source/refresh_pr_i_imp_m.R", sep="/"),loc=T )
      setkey(imp_m_universe, id_battello)
      imp_m_universe[pr_i_temp, pr_i:=i.pr_i, nomatch=0]
    }
    
    imp_m_universe=imp_m_universe[,list(value_ok=round(sum(value_ok/pr_i),0)), keyby=list(id_strato,var) ]
    imp_m_universe=imp_m_universe[,dcast.data.table(.SD, id_strato ~ var,value.var = "value_ok") ]
    imp_m_universe=imp_m_universe[,proflor:=round(ricavi-(carbur+alcova+spcom+alcofi+spmanu+lavoro),0)]
    
    imp_m_universe[,c('id_strato','carbur','alcova','spcom','alcofi','spmanu','lavoro','proflor','ricavi'),with=F]
  }
  
})