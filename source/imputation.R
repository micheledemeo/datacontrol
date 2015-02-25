
outliers_in_boxplot=reactive({
  outliers_in_boxplot = if(input$abs_or_mean_in_fix=='abs-outliers')  d_outliers_value()[,.(id_battello,var)] else d_outliers_parameter()[,.(id_battello,var)]   
  setkey(outliers_in_boxplot , id_battello,var)  
  outliers_in_boxplot  
})
output$outliers_id_battello_list_to_subset=renderUI({
  unique_id=outliers_in_boxplot()[,unique(id_battello)]
  unique_id=unique_id[order(unique_id)]
  selectInput("outliers_id_battello_list_to_subset", choices =unique_id, selected=unique_id, multiple = T , label=NA) 
})

outliers_in_imputation=reactive({
  if ( input_subset_units()=='all' ) outliers_in_boxplot() else outliers_in_boxplot()[.(as.numeric(input_outliers_id_battello_list_to_subset()))]  
})

output$outliers_in_imputation_dt=renderDataTable({ 
  
  if (input_keep_accept_refuse_outliers()=="keep") {
    setkey(all, id_battello,var)
    all[outliers_in_imputation(), (c('value_ok','notes','is_ok')):=
          list( ifelse(is.na(hist_value),value_ok,hist_value),
               ifelse(is.na(hist_value),"",hist_notes),
               ifelse(is.na(hist_value),0,hist_is_ok)
               )
        ]
    all[outliers_in_imputation() ,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,is_ok,imputation=hist_value,outlier=value,hist_notes)]
    
  } else if (input_keep_accept_refuse_outliers()=="accept") {
    
    setkey(all, id_battello,var)
    all[outliers_in_imputation(), (c('value_ok','notes','is_ok')):=
          list( ifelse(is.na(hist_value),value_ok,hist_value),
                ifelse(is.na(hist_value),"",hist_notes),
                ifelse(is.na(hist_value),0,hist_is_ok)
          )
        ]
    # con la precedente ho annullato eventuali modifiche temporanee
    nts=paste(session_info ,input_abs_or_mean_in_fix(),  input_keep_accept_refuse_outliers(), input_subset_units() ,sep="|")  
    all[outliers_in_imputation(), (c('value_ok','notes','is_ok')):=list(value,nts,1)]
    all[outliers_in_imputation(), .(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,is_ok,imputation=value_ok,outlier=value,notes)]

  } else { # qui input_keep_accept_refuse_outliers()=="refuse"
    setkey(all, id_battello,var)
    all[outliers_in_imputation(), (c('value_ok','notes','is_ok')):=
          list( ifelse(is.na(hist_value),value_ok,hist_value),
                ifelse(is.na(hist_value),"",hist_notes),
                ifelse(is.na(hist_value),0,hist_is_ok)
          )
        ]
# con la precedente ho annullato eventuali modifiche temporanee
    all[.(0)]
# source id model

  }
   
})

