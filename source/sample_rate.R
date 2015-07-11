output$keyby_sample_data=renderUI({  selectInput("keyby_sample_rate", label = NULL, choices =c('id_strato','regione','codsis199','codlft199','gsa','var'), selected = c('id_strato','regione','codsis199','codlft199','gsa','var'), multiple = T ) })

sample_data_react=reactive({
  
  sample_rate[,list(achieved_sample_rate=round(100*(sum(sample_achieved_units)/sum(universe_units)),2),
                  starting_sample_rate=round(100*sum(sample_units)/sum(universe_units),2),
                  response_rate=round(100*sum(sample_achieved_units)/sum(sample_units),2)
                  ),
           by=eval(input_keyby_sample_rate())]
  
})

