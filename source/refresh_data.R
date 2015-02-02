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
    incProgress(1/n, detail =  d[sample(1:nrow(d),1),id_battello] ) 
    Sys.sleep(.1)
  }
})

var=d[,unique(var)]
strato=d[,unique(id_strato)]
codsis=d[,unique(codsis199)]
codlft=d[,unique(codlft199)]

input_var=reactive({ input$var  })
input_codsis=reactive({ input$codsis  })
input_codlft=reactive({ input$codlft  })
input_strato=reactive({ input$strato  })
input_check_gio=reactive({ input$check_gio  })