# csv ####

output$download_outliers_value = downloadHandler(   
  filename = "outliers_abs.csv",
  content = function(file) write.table( d_outliers_value(), file, sep=";",quote=F, na="",row.names = F)
)

output$download_outliers_parameter = downloadHandler(   
  filename = "outliers_mean.csv",
  content = function(file) write.table( d_outliers_parameter(), file, sep=";",quote=F, na="",row.names = F)
)

output$download_pie_chart_value = downloadHandler(   
  filename = "pie_chart.csv",
  content = function(file) write.table( d_pie()[,1:(ncol(d_pie() ) -1), with=F ], file, sep=";",quote=F, na="",row.names = F)
)

output$download_zero_checks = downloadHandler(   
  filename = "zero_checks.csv",
  content = function(file) write.table( zero_checks(), file, sep=";",quote=F, na="",row.names = F)
)

output$download_not_sent = downloadHandler(    
  filename = "not_sent.csv",
  content = function(file) write.table( not_sent(), file, sep=";",quote=F, na="",row.names = F)
)

output$download_universe_data = downloadHandler(   
  filename = "universe_data.csv",
  content = function(file) write.table( universe_data(), file, sep=",",quote=F, na="",row.names = F)
)

output$download_sample_data = downloadHandler(   
  filename = "sample_data.csv",
  content = function(file) write.table( sample_data(), file, sep=",",quote=F, na="",row.names = F)
)

output$download_sample_rate = downloadHandler(   
  filename = "sample_rate.csv",
  content = function(file) write.table( sample_data_react(), file, sep=",",quote=F, na="",row.names = F)
)
output$download_row_sample_rate = downloadHandler(   
  filename = "row_data_sample_rate.csv",
  content = function(file) write.table( sample_rate, file, sep=",",quote=F, na="",row.names = F)
)



# graph ####

output$download_boxplot_value = downloadHandler( 
    filename = "boxplot_outliers_abs.png",
    content = function(file) file.copy("boxplot_outliers_abs.png", file, overwrite=TRUE)
)

output$download_boxplot_parameter = downloadHandler( 
  filename = "boxplot_outliers_parameter.png",
  content = function(file) file.copy("boxplot_outliers_parameter.png", file, overwrite=TRUE)
)

output$download_pie_chart = downloadHandler( 
  filename = "pie_chart.png",
  content = function(file) file.copy("pie_chart.png", file, overwrite=TRUE)
)

output$download_waterfall = downloadHandler( 

  filename="waterfall.png",

  content = function(file="test") file.copy(switch (as.character(input$waterfall_tab), 
                                             "41"="waterfall_italy.png", "42"="waterfall_strata.png", "43"="waterfall_plot_gear.png",
                                             "44"="waterfall_plot_loa.png","45"="waterfall_plot_regione.png","46"="waterfall_plot_gsa.png","47"="waterfall_plot_gear_loa.png"),
                                     file, overwrite=TRUE)
)








