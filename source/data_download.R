source( paste(getwd(), "source/ini_fun.R", sep="/"),loc=T )

# data import from remote server with a fake progress bar ####
withProgress(message = "Download from remote server:",
{
  n=20
  if( !system(paste("ping -n 1 www.google.com")) &  !file.exists(paste(getwd(),"source/stopdownload.uti",sep="/")) ) {
    
    ftp(action = "get",filename = "niseaYYYY.sql",year_local=year_local )
    writeLines( 
      paste0("C:\\wamp\\bin\\mysql\\mysql5.6.17\\bin\\mysql.exe -u nisea --password=n1s34 nisea < ",temp_dir_nicoda,"\\nisea", year_local, ".sql"),
      con=paste0(temp_dir_nicoda,"\\update_nisea.bat")  
    )
    system2(paste0(temp_dir_nicoda,"\\update_nisea.bat"))
    
  }  

  for (i in 1:n) {
    incProgress(1/n, detail =  sample(9000:70000,1) )
    Sys.sleep(.1)
  }
})

# data import from mysql with a fake progress bar ####
withProgress(message = "Loading data from local db:",
{
  n=20
  source( paste(getwd() , "source/ini_data.R", sep="/"), loc=T )

  for (i in 1:n) {
    incProgress(1/n, detail =  all[sample(1:nrow(all),1),id_battello] )
    Sys.sleep(.1)
  }
})


var=all[,unique(var)]
var=var[order(var)]
strato_dt=all[,.N,keyby=.(descrizione,id_strato)]
strato=strato_dt[,id_strato]
strato=as.list(strato)
names(strato)=strato_dt[,paste(descrizione,id_strato,sep="|@")]
rm(strato_dt)

id_battello_imp_m_dt=all[,.N,keyby=.(id_strato,descrizione,id_battello)][,descrizione:=paste(id_strato,"|",descrizione)][,id_strato:=NULL]
id_battello_imp_m_dt=id_battello_imp_m_dt[,.N,keyby=.(descrizione,id_battello)]
id_battello_imp_m=id_battello_imp_m_dt[,id_battello]
id_battello_imp_m=as.list(id_battello_imp_m)
names(id_battello_imp_m)=id_battello_imp_m_dt[,paste(descrizione,id_battello,sep="|@")]
rm(id_battello_imp_m_dt)

codsis=all[,unique(codsis199)]
codsis=codsis[order(codsis)]
codlft=all[,unique(codlft199)]
codlft=codlft[order(codlft)]
regione=all[,unique(regione)]
regione=regione[order(regione)]
gsa=all[,unique(gsa)]
gsa=gsa[order(gsa)]
str_sis_lft_reg_gsa=all[,.N,by=list(id_strato,codsis199,codlft199,regione,gsa)][,N:=NULL]
ril_strato=all[,.N,keyby=list(id_rilevatore,id_strato)][,N:=NULL]
ril=as.list(ril_dt[,id_rilevatore])
names(ril)=ril_dt[,paste(cognome,id_rilevatore,sep="  @")]
rm(ril_dt)
var_imp_m=var[var %in% c('carbur','alcova','spcom','alcofi','spmanu','lavoro','ricavi')]

input_show_output=reactive({ input$show_output  })
input_var=reactive({ input$var  })
input_codsis=reactive({ input$codsis  })
input_codlft=reactive({ input$codlft  })
input_strato=reactive({ input$strato  })
input_ril=reactive({ input$ril  })
input_regione=reactive({ input$regione  })
input_gsa=reactive({ input$gsa  })

input_codsis_all=reactive({ if(is.null(input_codsis())) codsis else input_codsis()  })
input_codlft_all=reactive({ if(is.null(input_codlft())) codlft else input_codlft()  })
input_regione_all=reactive({ if(is.null(input_regione())) regione else input_regione()  })
input_gsa_all=reactive({ if(is.null(input_gsa())) gsa else input_gsa()  })

input_check_gio=reactive({ input$check_gio  })
input_apply_weights=reactive({ input$apply_weights  })
input_not_sent_as_0=reactive({ input$not_sent_as_0  })
input_keep_imputations=reactive({ input$keep_imputations })

# oggetti di imputation tab:
input_var_imp=reactive({ input$var_imp  })
input_codsis_imp=reactive({ input$codsis_imp  })
input_codlft_imp=reactive({ input$codlft_imp  })
input_strato_imp=reactive({ input$strato_imp  })
input_start_imputation=reactive({ input$start_imputation })
input_remove_imputations_to_fit=reactive({ input$remove_imputations_to_fit })
input_abs_or_mean_in_fix=reactive({ input$abs_or_mean_in_fix })
input_subset_units=reactive({ input$subset_units })
input_outliers_id_battello_list_to_subset=reactive({input$outliers_id_battello_list_to_subset})
input_keep_accept_refuse_outliers=reactive({ input$keep_accept_refuse_outliers  })
input_group_for_imputation_method=reactive({ input$group_for_imputation_method  })
input_imputation_method=reactive({ input$imputation_method  })
input_slider_hotdeck=reactive({input$slider_hotdeck})
input_freeze_data=reactive({input$freeze_data})
dep_imputation_tab=reactive({
  input_var_imp()
  input_codsis_imp()
  input_codlft_imp()
  input_strato_imp()
  input_start_imputation()
  input_remove_imputations_to_fit()
  input_abs_or_mean_in_fix()
  input_subset_units()
  input_outliers_id_battello_list_to_subset()
  input_keep_accept_refuse_outliers()
  input_group_for_imputation_method()
  input_imputation_method()
  input_slider_hotdeck()
  input_freeze_data()
})

# oggetti in inputation manual
input_var_imp_m=reactive({ input$var_imp_m })
input_strato_imp_m=reactive({ input$strato_imp_m })
input_slider_imp_m=reactive({input$slider_imp_m})
input_imputation_manual_method=reactive({input$imputation_manual_method})
input_abs_imp_m=reactive({if(is.na(input$abs_imp_m)) 0 else input$abs_imp_m })
input_id_battello_imp_m=reactive({ input$id_battello_imp_m })
dep_imputation_manual_tab=reactive({
  input_var_imp_m()
  input_strato_imp_m()
  input_slider_imp_m()
  input_imputation_manual_method()
  input_abs_imp_m()
  input_id_battello_imp_m()
  
})
  
#oggetti in closing session
input_exp_data=reactive({ input$exp_data })
input_keyby_sample_rate=reactive({ input$keyby_sample_rate })
input_strato_close=reactive({ input$strato_close })
input_strato_open=reactive({ input$strato_open})




