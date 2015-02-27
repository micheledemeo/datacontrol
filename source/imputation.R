
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

# genera dataset con le imputazioni da mettere in join con all ####
data_for_imputation=reactive({
  # define dataset in imputation ####
  setkey(all, id_battello,var)
  in_imputation=all[outliers_in_imputation(),.N,keyby=list(var,codsis199,codlft199,id_strato)][,N:=NULL]  
  
  # il keyby non funziona con un vettore di tipo reactive, ottenuto dall'input dell'utente! purtroppo è necessario ripetere tutto lo step per ogni if
  if (input_group_for_imputation_method()=='strata') {
    setkey(all, var, id_strato)
    in_imputation_temp=in_imputation[,.N,keyby=list(var,id_strato)]
    imputation_output=all[in_imputation_temp, nomatch=0,][sent==1, list(id_battello,id_strato,var,value)]
    imputation_output[,c('out_up','out_down'):=list( quantile(value,.75)+1.5*IQR(value), quantile(value,.25)-1.5*IQR(value) ),  keyby=c('var','id_strato')]
    imputation_output[value>=out_down & value<=out_up, list(id_battello,var,id_strato,value)]
    
  } else if (input_group_for_imputation_method()=='gear') {
    setkey(all, var, codsis199)
    in_imputation_temp=in_imputation[,.N,keyby=list(var,codsis199)]
    imputation_output=all[in_imputation_temp, nomatch=0,][sent==1, list(id_battello,codsis199,var,value)]
    imputation_output[,c('out_up','out_down'):=list( quantile(value,.75)+1.5*IQR(value), quantile(value,.25)-1.5*IQR(value) ),  keyby=c('var','codsis199')]
    imputation_output[value>=out_down & value<=out_up, list(id_battello,var,codsis199,value)]
    
  } else if (input_group_for_imputation_method()=='loa') {
    setkey(all, var, codlft199)
    in_imputation_temp=in_imputation[,.N,keyby=list(var,codlft199)]
    imputation_output=all[in_imputation_temp, nomatch=0,][sent==1, list(id_battello,codlft199,var,value)]
    imputation_output[,c('out_up','out_down'):=list( quantile(value,.75)+1.5*IQR(value), quantile(value,.25)-1.5*IQR(value) ),  keyby=c('var','codlft199')]
    imputation_output[value>=out_down & value<=out_up, list(id_battello,var,codlft199,value)]
  
  } else if (input_group_for_imputation_method()=='gear_loa') {    
    setkey(all, var, codlft199,codsis199)
    in_imputation_temp=in_imputation[,.N,keyby=list(var,codlft199,codsis199)]
    imputation_output=all[in_imputation_temp, nomatch=0,][sent==1, list(id_battello,codlft199,codsis199,var,value)]
    imputation_output[,c('out_up','out_down'):=list( quantile(value,.75)+1.5*IQR(value), quantile(value,.25)-1.5*IQR(value) ),  keyby=c('var','codlft199','codsis199')]
    imputation_output[value>=out_down & value<=out_up, list(id_battello,var,codlft199,codsis199,value)]
    
  }  else {
    setkey(all, var)
    in_imputation_temp=in_imputation[,.N,keyby=list(var)]
    imputation_output=all[in_imputation_temp, nomatch=0,][sent==1, list(id_battello,var,value)]
    imputation_output[,c('out_up','out_down'):=list( quantile(value,.75)+1.5*IQR(value), quantile(value,.25)-1.5*IQR(value) ),  keyby=c('var')]
    imputation_output[value>=out_down & value<=out_up, list(id_battello,var,value)]
    
  }
})
  
  
# fit the model ####
imputation_output=reactive({
  
  key_vec=setdiff( names(data_for_imputation()) , c("id_battello", "value") )
  
  if (input_imputation_method()=='mean') {    
    data_for_imputation()[,list(imputation_value=round(mean(value),0) ), keyby=key_vec]
    
  } else if (input_imputation_method()=='regression') {
    reg_model=data_for_imputation()[,.N,keyby=list(id_battello,var)]
    setkey(all, id_battello,var)
    reg_model=all[reg_model, nomatch=0,c('id_battello',key_vec,'giorni_mare','value'), with=F]
    reg_model=reg_model[,as.list(lm(value~giorni_mare)$coeff), keyby=key_vec]
    setnames(reg_model,c('(Intercept)','giorni_mare'), c('a','b') )
    id_outliers=all[outliers_in_imputation(),c ('id_battello',key_vec,'giorni_mare'), with=F]
    setkeyv(id_outliers,key_vec)
    outliers_with_imputation=id_outliers[reg_model][,list(id_battello,var,imputation_value=round(a+b*giorni_mare,0) )]
    setkey(outliers_with_imputation, id_battello,var)
    outliers_with_imputation
    # : id_battello, var, imputation_value
    
  } else if (input_imputation_method()=='hot-deck') {
    
  }  else{
    
  }
  
})

# render data table to show ####
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
    nts=paste(session_info, input_abs_or_mean_in_fix(), input_subset_units(), input_keep_accept_refuse_outliers(), sep="|")  
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
    # eseguo l'imputazione in base gli input dell'utente:   
    nts=paste(session_info, input_abs_or_mean_in_fix(), input_subset_units(), input_keep_accept_refuse_outliers(), input_group_for_imputation_method(),input_imputation_method(), sep="|")    
    #imputation_output()
    setkeyv(all, key(imputation_output()) )
    if(nrow(imputation_output())>0){    
      all[imputation_output(), (c('value_ok','is_ok','notes')):=list(imputation_value,1L,nts )]
      setkey(all, id_battello,var)
      all[outliers_in_imputation(), .(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,is_ok,imputation=value_ok,outlier=value,notes)]
          
    } else { #in questo caso non ci sono dati a sufficienza per stimare, quindi ripristino la soluzione scaricata
      setkey(all, id_battello,var)
      nts=paste("not enough data in the actual session:", nts)
      all[outliers_in_imputation(), (c('value_ok','notes','is_ok')):=
            list( ifelse(is.na(hist_value),value,hist_value),
                  nts,
                  ifelse(is.na(hist_value),0,hist_is_ok)
            )
          ]
      setkey(all, id_battello,var)
      all[outliers_in_imputation(), .(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,is_ok,imputation=hist_value,outlier=value,notes)]
    }
    
  }
   
})

