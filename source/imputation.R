
outliers_in_imputation=reactive({
  outliers_in_imputation = if(input$abs_or_mean_in_fix=='abs-outliers')  d_outliers_value()[,.(id_battello,var)] else d_outliers_parameter()[,.(id_battello,var)]   
  setkey(outliers_in_imputation , id_battello,var)  
  outliers_in_imputation  
})
output$outliers_id_battello_list=renderUI({selectInput("outliers_id_battello_list", choices =outliers_in_imputation()[,order(unique(id_battello))], multiple = T ) })

output$outliers_in_imputation=renderDataTable({ 
  
  if (input_accept_refuse_outliers()=="keep") {
    setkey(all, id_battello,var)
    all[outliers_in_imputation(), (c('value_ok','notes','is_ok')):=
          list( ifelse(is.na(hist_value),value_ok,hist_value),
               ifelse(is.na(hist_value),"",hist_notes),
               ifelse(is.na(hist_value),0,hist_is_ok)
               )
        ]
    all[outliers_in_imputation() ,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,is_ok,imputation=hist_value,outlier=value,hist_notes)]
    
  } else if (input_accept_refuse_outliers()=="accept") {
    nts=paste(session_info ,input_abs_or_mean_in_fix(),  input_accept_refuse_outliers(), input_accept_all_or_list(),sep="|")  
    setkey(all, id_battello,var)
    all[outliers_in_imputation(), (c('value_ok','notes','is_ok')):=list(value,nts,1)]
    all[outliers_in_imputation() ,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,is_ok,imputation=value_ok,outlier=value,notes)]
    
  } else {
    setkey(all, id_battello,var)
    all[outliers_in_imputation(), (c('value_ok','notes','is_ok')):=list(value,"",0)]
    all[outliers_in_imputation() ,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,is_ok,outlier=value)]
  }
   
})

