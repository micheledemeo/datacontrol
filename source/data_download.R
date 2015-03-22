source( paste(getwd(), "source/ini_fun.R", sep="/"),loc=T )

# data import from remote server with a fake progress bar ####
withProgress(message = "Download from remote server:",
{
  n=20
  if( !system(paste("ping -n 1 www.google.com")) &  !file.exists(paste(getwd(),"source/stopdownload.uti",sep="/")) )  system2("C:/nisea/batch/AggiornamentoDB.bat")

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
codsis=all[,unique(codsis199)]
codsis=codsis[order(codsis)]
codlft=all[,unique(codlft199)]
codlft=codlft[order(codlft)]
str_sis_lft=all[,.N,by=list(id_strato,codsis199,codlft199)][,N:=NULL]

input_show_output=reactive({ input$show_output  })
input_var=reactive({ input$var  })
input_codsis=reactive({ input$codsis  })
input_codlft=reactive({ input$codlft  })
input_strato=reactive({ input$strato  })

input_check_gio=reactive({ input$check_gio  })
input_apply_weights=reactive({ input$apply_weights  })
input_not_sent_as_0=reactive({ input$not_sent_as_0  })

# oggetti di imputation tab:
input_var_imp=reactive({ input$var_imp  })
input_codsis_imp=reactive({ input$codsis_imp  })
input_codlft_imp=reactive({ input$codlft_imp  })
input_strato_imp=reactive({ input$strato_imp  })
input_start_imputation=reactive({ input$start_imputation })
input_discard_imputations=reactive({ input$discard_imputations })
input_abs_or_mean_in_fix=reactive({ input$abs_or_mean_in_fix })
input_subset_units=reactive({ input$subset_units })
input_outliers_id_battello_list_to_subset=reactive({input$outliers_id_battello_list_to_subset})
input_keep_accept_refuse_outliers=reactive({ input$keep_accept_refuse_outliers  })
input_group_for_imputation_method=reactive({ input$group_for_imputation_method  })
input_imputation_method=reactive({ input$imputation_method  })
input_slider_hotdeck=reactive({input$slider_hotdeck})
input_freeze_data=reactive({input$freeze_data})