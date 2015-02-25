# data import from remote server with a fake progress bar ####
withProgress(message = "Download from remote server:",
{ 
  n=20
  #system2("C:/nisea/batch/AggiornamentoDB.bat")
  
  for (i in 1:n) {
    incProgress(1/n, detail =  sample(9000:70000,1) ) 
    Sys.sleep(.1)
  }
})

# data import from mysql with a fake progress bar ####
withProgress(message = "Loading data from local MySql:",
{ 
  n=20
  source( pastedir(getwd() , "source/ini_data.R"), loc=T )
  
  for (i in 1:n) {
    incProgress(1/n, detail =  all[sample(1:nrow(all),1),id_battello] ) 
    Sys.sleep(.1)
  }
})


var=all[,unique(var)]
strato=all[,unique(id_strato)]
codsis=all[,unique(codsis199)]
codlft=all[,unique(codlft199)]
str_sis_lft=all[,.N,by=list(id_strato,codsis199,codlft199)][,N:=NULL]

input_var=reactive({ input$var  })
input_codsis=reactive({ input$codsis  })
input_codlft=reactive({ input$codlft  })
input_strato=reactive({ input$strato  })
input_check_gio=reactive({ input$check_gio  })
input_apply_weights=reactive({ input$apply_weights  })
input_not_sent_as_0=reactive({ input$not_sent_as_0  })
# oggetti di imputation tab:
input_abs_or_mean_in_fix=reactive({ input$abs_or_mean_in_fix })
input_subset_units=reactive({ input$subset_units })
input_outliers_id_battello_list_to_subset=reactive({input$outliers_id_battello_list_to_subset})
input_keep_accept_refuse_outliers=reactive({ input$keep_accept_refuse_outliers  })
#input_accept_all_or_list=reactive({ input$accept_all_or_list  })
#input_outliers_id_battello_list_to_accept=reactive({input$outliers_id_battello_list_to_accept})
input_group_for_imputation_method=reactive({ input$group_for_imputation_method  })
input_imputation_method=reactive({ input$imputation_method  })
