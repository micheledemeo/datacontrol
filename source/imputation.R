
outliers_in_input=reactive({
  
  d_outliers=if (  !is.null( input_strato_imp() ) ) all[giorni_mare>(input_check_gio()-1) & id_strato %in% input_strato_imp() , .(id_battello,var,value_or,parameter_or,sent,is_ok,session_info,notes) ]
  else if ( !is.null(input_codsis_imp()) &  is.null(input_codlft_imp()) )  all[giorni_mare>(input_check_gio()-1)  & codsis199 %in% input_codsis_imp(), .(id_battello,var,value_or,parameter_or,sent,is_ok,session_info,notes) ]
  else if ( is.null(input_codsis_imp()) &  !is.null(input_codlft_imp()) )  all[giorni_mare>(input_check_gio()-1)  & codlft199 %in% input_codlft_imp(), .(id_battello,var,value_or,parameter_or,sent,is_ok,session_info,notes) ]
  else if ( !is.null(input_codsis_imp()) &  !is.null(input_codlft_imp()) ) all[giorni_mare>(input_check_gio()-1)  & codsis199 %in% input_codsis_imp() & codlft199 %in% input_codlft_imp(), .(id_battello,var,value_or,parameter_or,sent,is_ok,session_info,notes) ]  
  else all[0]
  d_outliers=d_outliers[ var %in% input_var_imp() ]
  
  #if(input_keep_imputations()==1) d_outliers=d_outliers[ !(is_ok==1 & !grepl(session_info, notes,fixed = T)), .(id_battello,var,value_or,parameter_or,sent) ]    
  if( input_not_sent_as_0()==0 ) d_outliers=d_outliers[sent==1]  
  d_outliers= if(input$abs_or_mean_in_fix=='abs-outliers') d_outliers[,.(id_battello,var,var_for_outliers=value_or)] else d_outliers[,.(id_battello,var,var_for_outliers=parameter_or)]
  
  d_outliers2=d_outliers[,list( out_up=quantile(var_for_outliers,.75)+1.5*IQR(var_for_outliers), out_down=quantile(var_for_outliers,.25)-1.5*IQR(var_for_outliers) ),  keyby=.(var)]
  setkey(d_outliers, var )
  setkey(d_outliers2, var )    
  d_outliers=d_outliers2[d_outliers][var_for_outliers>out_up | var_for_outliers<out_down][,list(id_battello,var)]
  setkey(d_outliers , id_battello,var)
  d_outliers 
  
})

output$outliers_id_battello_list_to_subset=renderUI({ 
    unique_id=outliers_in_input()[,unique(id_battello)]
    unique_id=unique_id[order(unique_id)]
    selectInput("outliers_id_battello_list_to_subset", choices =unique_id, selected=unique_id, multiple = T , label=NA)
  
})

outliers_in_imputation=reactive({
  if ( input_subset_units()=='all' ) outliers_in_input() else outliers_in_input()[.(as.numeric(input_outliers_id_battello_list_to_subset()))]
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
    imputation_output=all[in_imputation_temp, nomatch=0,][sent==1, list(id_battello,id_strato,var,value_or,value_ok,is_ok)]
    setkey(imputation_output, id_battello,var)
    imputation_output=imputation_output[!outliers_in_imputation()]
    if(input_remove_imputations_to_fit()==1) imputation_output=imputation_output[ !(is_ok==1 & round(value_ok-value_or,0)!=0) ]
    imputation_output=imputation_output[,list(id_battello,id_strato,var,value_or)]
    imputation_output[,c('out_up','out_down'):=list( quantile(value_or,.75)+1.5*IQR(value_or), quantile(value_or,.25)-1.5*IQR(value_or) ),  keyby=c('var','id_strato')]
    imputation_output[value_or>=out_down & value_or<=out_up, list(id_battello,var,id_strato,value_or)]
    
  } else if (input_group_for_imputation_method()=='gear') {
    setkey(all, var, codsis199)
    in_imputation_temp=in_imputation[,.N,keyby=list(var,codsis199)]
    imputation_output=all[in_imputation_temp, nomatch=0,][sent==1, list(id_battello,codsis199,var,value_or,value_ok,is_ok)]
    setkey(imputation_output, id_battello,var)
    imputation_output=imputation_output[!outliers_in_imputation()]
    if(input_remove_imputations_to_fit()==1) imputation_output=imputation_output[ !(is_ok==1 & round(value_ok-value_or,0)!=0) ]
    imputation_output=imputation_output[,list(id_battello,codsis199,var,value_or)]
    imputation_output[,c('out_up','out_down'):=list( quantile(value_or,.75)+1.5*IQR(value_or), quantile(value_or,.25)-1.5*IQR(value_or) ),  keyby=c('var','codsis199')]
    imputation_output[value_or>=out_down & value_or<=out_up, list(id_battello,var,codsis199,value_or)]
    
  } else if (input_group_for_imputation_method()=='loa') {
    setkey(all, var, codlft199)
    in_imputation_temp=in_imputation[,.N,keyby=list(var,codlft199)]
    imputation_output=all[in_imputation_temp, nomatch=0,][sent==1, list(id_battello,codlft199,var,value_or,value_ok,is_ok)]
    setkey(imputation_output, id_battello,var)
    imputation_output=imputation_output[!outliers_in_imputation()]
    if(input_remove_imputations_to_fit()==1) imputation_output=imputation_output[ !(is_ok==1 & round(value_ok-value_or,0)!=0) ]
    imputation_output=imputation_output[,list(id_battello,codlft199,var,value_or)]
    imputation_output[,c('out_up','out_down'):=list( quantile(value_or,.75)+1.5*IQR(value_or), quantile(value_or,.25)-1.5*IQR(value_or) ),  keyby=c('var','codlft199')]
    imputation_output[value_or>=out_down & value_or<=out_up, list(id_battello,var,codlft199,value_or)]
    
  } else if (input_group_for_imputation_method()=='gear_loa') {
    setkey(all, var, codlft199,codsis199)
    in_imputation_temp=in_imputation[,.N,keyby=list(var,codlft199,codsis199)]
    imputation_output=all[in_imputation_temp, nomatch=0,][sent==1, list(id_battello,codlft199,codsis199,var,value_or,value_ok,is_ok)]
    setkey(imputation_output, id_battello,var)
    imputation_output=imputation_output[!outliers_in_imputation()]
    if(input_remove_imputations_to_fit()==1) imputation_output=imputation_output[ !(is_ok==1 & round(value_ok-value_or,0)!=0) ]
    imputation_output=imputation_output[,list(id_battello,codlft199,codsis199,var,value_or)]
    imputation_output[,c('out_up','out_down'):=list( quantile(value_or,.75)+1.5*IQR(value_or), quantile(value_or,.25)-1.5*IQR(value_or) ),  keyby=c('var','codlft199','codsis199')]
    imputation_output[value_or>=out_down & value_or<=out_up, list(id_battello,var,codlft199,codsis199,value_or)]
    
  } else {
    setkey(all, var)
    in_imputation_temp=in_imputation[,.N,keyby=list(var)]
    imputation_output=all[in_imputation_temp, nomatch=0,][sent==1, list(id_battello,var,value_or,value_ok,is_ok)]
    setkey(imputation_output, id_battello,var)
    imputation_output=imputation_output[!outliers_in_imputation()]
    if(input_remove_imputations_to_fit()==1) imputation_output=imputation_output[ !(is_ok==1 & round(value_ok-value_or,0)!=0) ]
    imputation_output=imputation_output[,list(id_battello,var,value_or)]
    imputation_output[,c('out_up','out_down'):=list( quantile(value_or,.75)+1.5*IQR(value_or), quantile(value_or,.25)-1.5*IQR(value_or) ),  keyby=c('var')]
    imputation_output[value_or>=out_down & value_or<=out_up, list(id_battello,var,value_or)]
    
  }
}) #  data_for_imputation


# fit the model ####
imputation_output=reactive({ 
  
  input_remove_imputations_to_fit()
   
  if( nrow(data_for_imputation()) >0 ){
    key_vec=setdiff( names(data_for_imputation()) , c("id_battello", "value_or") )
    setkey(all, id_battello,var)
    in_imputation=all[outliers_in_imputation(), c('id_battello',key_vec,'giorni_mare','equipaggio_medio'), with=F]
    ricavi=all[.(outliers_in_imputation()[,unique(id_battello)])][var=='ricavi', .(id_battello,ricavi=value_or)]
    setkey(ricavi, id_battello)
    in_imputation=ricavi[in_imputation]
    # : id_battello,key_vec,giorni_mare,equipaggio_medio,ricavi
    
    setkeyv(in_imputation, key_vec)
    
    if (input_imputation_method()=='mean') {
      mean_model=data_for_imputation()[,list(imputation_value=round(mean(value_or),0),imputation_notes='ok' ), keyby=key_vec]
      mean_model=mean_model[in_imputation]
     
      mean_model[!is.na(imputation_notes),imputation_parameter:=as.numeric(0) ]
      mean_model[!is.na(imputation_notes), 
                 imputation_parameter:=ifelse(var %in% c('spmanu','alcofi','amm','indeb','invest'),
                                              round(as.numeric(imputation_value)),
                                              ifelse(var %in% c('alcova','carbur','ricavi','ricavi_est') &  giorni_mare>0,
                                                     round(imputation_value/giorni_mare),
                                                     ifelse(var=='lavoro' &  equipaggio_medio>0,
                                                            round(imputation_value/equipaggio_medio),
                                                            ifelse( var=='spcom' & ricavi>0,
                                                                    round(imputation_value/ricavi,3),
                                                                    0)
                                                            ) 
                                                      )
                                              )
                ]
      mean_model=mean_model[, list(id_battello,var,imputation_value,imputation_parameter,imputation_notes)]
      mean_model[is.na(imputation_notes),imputation_notes:="notok"]
      setkey(mean_model, id_battello,var)
      mean_model
      
    } else if (input_imputation_method()=='regression') {
      reg_model=data_for_imputation()[,.N,keyby=list(id_battello,var)]
      setkey(all, id_battello,var)
      reg_model=all[reg_model, nomatch=0,c('id_battello',key_vec,'giorni_mare','value_or'), with=F]
      reg_model=reg_model[,as.list(lm(value_or~giorni_mare)$coeff), keyby=key_vec]
      setnames(reg_model,c('(Intercept)','giorni_mare'), c('a','b') )
      reg_model[,imputation_notes:='ok']
      reg_model=reg_model[in_imputation]
      reg_model[!is.na(imputation_notes),imputation_value:=round(a+b*giorni_mare,0)]
      #reg_model=reg_model[,list(id_battello,var,imputation_value=round(a+b*giorni_mare,0),giorni_mare,equipaggio_medio,ricavi,imputation_notes )]
      reg_model[!is.na(imputation_notes),imputation_parameter:=as.numeric(0) ]
      reg_model[!is.na(imputation_notes), imputation_parameter:=ifelse(var %in% c('spmanu','alcofi','amm','indeb','invest'),
                                               round(as.numeric(imputation_value)),
                                               ifelse(var %in% c('alcova','carbur','ricavi','ricavi_est') &  giorni_mare>0,
                                                      round(imputation_value/giorni_mare),
                                                      ifelse(var=='lavoro' &  equipaggio_medio>0,
                                                             round(imputation_value/equipaggio_medio),
                                                             ifelse( var=='spcom' & ricavi>0,
                                                                     round(imputation_value/ricavi,3),
                                                                     0)
                                                      ) 
                                               )
      )
      ]
      reg_model=reg_model[, list(id_battello,var,imputation_value,imputation_parameter,imputation_notes)]
      reg_model[is.na(imputation_notes),imputation_notes:="notok"]
      setkey(reg_model, id_battello,var)
      reg_model
      # : id_battello, var, imputation_value,imputation_parameter,imputation_notes
      
    } else if (input_imputation_method()=='hot-deck') {
      hotdeck=copy(data_for_imputation())
      setkeyv(hotdeck, c(key_vec,'value_or') )
      hotdeck[,i:=.SD[,.I],by=key_vec]
      quant_to_filter=hotdeck[,list( i=ceiling( (input_slider_hotdeck()/100)*max(i) ) ),keyby=key_vec]
      setkeyv(quant_to_filter, c(key_vec,"i") )
      setkeyv(hotdeck, c(key_vec,"i") )
      hotdeck=hotdeck[quant_to_filter, nomatch=0]
      setnames(hotdeck, "value_or", "imputation_value")
      # rimuovo la i e id_battello per avere la chiave solo su key_vec
      hotdeck[,c('id_battello','i','imputation_notes'):=list(NULL,NULL,"ok")]
      setkeyv(hotdeck, key_vec)
      hotdeck=hotdeck[in_imputation]
      hotdeck[!is.na(imputation_notes),imputation_parameter:=as.numeric(0) ]
      hotdeck[!is.na(imputation_notes), imputation_parameter:=ifelse(var %in% c('spmanu','alcofi','amm','indeb','invest'),
                                             round(as.numeric(imputation_value)),
                                             ifelse(var %in% c('alcova','carbur','ricavi','ricavi_est') &  giorni_mare>0,
                                                    round(imputation_value/giorni_mare),
                                                    ifelse(var=='lavoro' &  equipaggio_medio>0,
                                                           round(imputation_value/equipaggio_medio),
                                                           ifelse( var=='spcom' & ricavi>0,
                                                                   round(imputation_value/ricavi,3),
                                                                   0)
                                                    ) 
                                             )
      )
      ] 
      hotdeck=hotdeck[,list(id_battello,var,imputation_value,imputation_parameter,imputation_notes)]
      hotdeck[is.na(imputation_notes),imputation_notes:="notok"]
      setkey(hotdeck, id_battello,var)
      hotdeck
    }  
  } else {
    data.table(id_battello=0L,var="",imputation_value=0L,imputation_parameter=0L,imputation_notes="")[0]
  }
  
})
# imputation_output()

# render data table to show ####
output$outliers_in_imputation_dt=renderDataTable({
  
  input_remove_imputations_to_fit()
#     write.table.ok(imputation_output(),"imputation_output.csv")
#     write.table.ok(data_for_imputation(),"data_for_imputation.csv")
  
  if (input_start_imputation()==0 ) {
    setkey(all, id_battello,var)
    all[outliers_in_imputation(), (c('value_ok','parameter_ok','notes','is_ok')):=
          list( ifelse(is.na(hist_value),value_or,hist_value),
                ifelse(is.na(hist_value),parameter_or,hist_parameter),
                ifelse(is.na(hist_value),"",hist_notes),
                ifelse(is.na(hist_value),0,hist_is_ok)
          )
        ]
    all[outliers_in_imputation() ,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,is_ok,imputation=hist_value,outlier=value_or,notes=hist_notes)]
    
  } else {
    
    if (input_keep_accept_refuse_outliers()=="keep" ) {
      setkey(all, id_battello,var)
      all[outliers_in_imputation(), (c('value_ok','parameter_ok','notes','is_ok')):=
            list( ifelse(is.na(hist_value),value_or,hist_value),
                  ifelse(is.na(hist_value),parameter_or,hist_parameter),
                  ifelse(is.na(hist_value),"",hist_notes),
                  ifelse(is.na(hist_value),0,hist_is_ok)
            )
          ]
      all[outliers_in_imputation() ,.(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,is_ok,imputation=hist_value,outlier=value_or,notes=hist_notes)]
      
      
    } else if (input_keep_accept_refuse_outliers()=="accept") { 
      
      setkey(all, id_battello,var)
      nts=paste(session_info, input_abs_or_mean_in_fix(), input_subset_units(), input_keep_accept_refuse_outliers(), sep="|")
      all[outliers_in_imputation(), (c('value_ok','parameter_ok','notes','is_ok')):=list(value_or,parameter_or,nts,1)]
      all[outliers_in_imputation(), .(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,is_ok,imputation=value_ok,imputation_parameter=parameter_ok,outlier=value_or,parameter_outlier=parameter_or,notes)]
      
    } else { # qui input_keep_accept_refuse_outliers()=="refuse" (start imputation)
      
      nts=paste(session_info, input_abs_or_mean_in_fix(), input_subset_units(), input_keep_accept_refuse_outliers(), input_remove_imputations_to_fit() ,input_group_for_imputation_method(),input_imputation_method(), sep="|")
     
      setkey(all, id_battello,var )
      all[imputation_output(), (c('value_ok','parameter_ok','is_ok','notes')):=list(ifelse(imputation_notes=='ok',imputation_value, ifelse(is.na(hist_value),value_or,hist_value) ),
                                                                                    ifelse(imputation_notes=='ok',imputation_parameter, ifelse(is.na(hist_parameter),parameter_or,hist_parameter)),
                                                                                    ifelse(imputation_notes=='ok',1L,  ifelse(is.na(hist_is_ok),0,hist_is_ok) ),
                                                                                    ifelse(imputation_notes=='ok', nts, paste('Keeping the data from the server. Not enough data to fit the model with the current filters.',ifelse(is.na(hist_notes),"",hist_notes) ))
                                                                                    )
          ]
      setkey(all, id_battello,var)
      all[outliers_in_imputation()][, .(id_rilevatore,var,id_strato,id_battello,regione,codsis199,codlft199,gsa,descrizione,is_ok,imputation=value_ok,imputation_parameter=parameter_ok,outlier=value_or,parameter_outlier=parameter_or, notes )]
      
    }
    
  } # if (input_start_imputation()==0 )
    
})
