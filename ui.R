require(shinythemes)
wd=getwd()
shinyUI(fluidPage( theme = shinytheme("flatly"),
                   
                   #titlePanel("Nicoda Project"),
                   br(),
                   br(),
                   img(src="nicoda_logo.jpg"),
                   br(),
                   br(),
                   br(),
                   
                   sidebarLayout(
                     sidebarPanel(
                       conditionalPanel(condition="input.headtab==9",checkboxInput("upload_button", "UPLOAD OF IMPUTATION", value=F)),
                       conditionalPanel(condition="input.headtab >= 1 && input.headtab <= 4",
                                        verbatimTextOutput("version_nr"),
                                        radioButtons("show_output", label = 'Choose if:',
                                                     choices = list("Show output with original data" = 'orig_data', 'Show output with imputations' = 'imput_data'), selected = 'orig_data')
                        ),
                       conditionalPanel(condition="input.headtab == 1 || input.headtab == 2 || (input.headtab == 6 && input.zero_or_not_sent == 61)", uiOutput("var"), br() ),
                       conditionalPanel(condition="(input.headtab >= 1 && input.headtab <= 4) || (input.headtab == 6 && input.zero_or_not_sent == 61)",
                                        strong("Filter by gear-loa-region-gsa:"),
                                        uiOutput("codsis"),
                                        uiOutput("codlft"),
                                        uiOutput("regione"),
                                        uiOutput("gsa"),
                                        actionButton("reset", "Reset filters"),
                                        br(),
                                        uiOutput("strato"),
                                        uiOutput("ril"),
                                        br(),
                                        br()
                                        
                       ),
                       conditionalPanel(condition="input.headtab== 8 & input.start_imputation==0",
                                        uiOutput("var_imp"),
                                        uiOutput("strato_imp"),
                                        uiOutput("codsis_imp"),
                                        uiOutput("codlft_imp")
                       ),
                       conditionalPanel(condition="(input.headtab <=6 | (input.headtab ==8 & input.start_imputation==0) ) & !(input.headtab == 6 && input.zero_or_not_sent == 62)", 
                                        checkboxInput(inputId = "check_gio",label = "Days at sea > 0", value=F)
                       ),
                       conditionalPanel(condition="input.headtab == 3 || input.headtab == 4", 
                                        checkboxInput(inputId = "apply_weights",label = "Apply weights", value=T)
                       ),
                       conditionalPanel(condition="( (input.headtab >= 1 & input.headtab <= 4) | (input.headtab ==8 & input.start_imputation==0) ) & input.check_gio==0", 
                                        checkboxInput(inputId = "not_sent_as_0",label = "Not-sent as zero-values", value=F)
                       ),                     
                       conditionalPanel(condition="input.headtab== 8 & input.start_imputation==0",
                                        radioButtons("abs_or_mean_in_fix", label = "Choose if refer to abs-outliers or mean-outliers",
                                                     choices = list("abs-outliers" = 'abs-outliers', "mean-outliers" = 'mean-outliers'), selected = 'abs-outliers'),
                                        radioButtons("subset_units", label = "Choose if subset specific units",
                                                     choices = list("all units" = 'all', "subset units" = 'subset'), selected = 'all'),
                                        conditionalPanel(condition="input.subset_units == 'subset'",
                                                         uiOutput("outliers_id_battello_list_to_subset")
                                        )
                                        #,checkboxInput("keep_imputations", "Keep existing imputations (of previous sessions)", value=F)
                                        ),
                       conditionalPanel(condition="input.headtab== 8",
                                        checkboxInput(inputId = "start_imputation",label = "START IMPUTATION ON APPLIED FILTERS", value=F)
                       ),
                       conditionalPanel(condition="input.headtab == 8 & input.start_imputation==1  & input.freeze_data=='no'",
                                        radioButtons("keep_accept_refuse_outliers", label = "Choose if accept:",
                                                     choices = list("keep the data from the server"='keep',"accept outliers as ok-values" = "accept", "start imputation" = "refuse"), selected = "keep"),
                                        conditionalPanel(condition="input.keep_accept_refuse_outliers == 'refuse'",
                                                         checkboxInput("remove_imputations_to_fit", "Remove imputations (of previous sessions) to fit the model", value=T),
                                                         radioButtons("group_for_imputation_method", label = "Choose how to group units to build the model",
                                                                      choices = list("same strata"="strata", "same gear"='gear', "same loa"='loa', "same gear and loa"='gear_loa', "all data"='all_data' ), selected = 'strata'),
                                                         radioButtons("imputation_method", label = "Choose the imputation model to fix outliers",
                                                                      choices = list("mean of the group"='mean', "regression"='regression', "hot-deck"='hot-deck'), selected = 'mean'),
                                                         conditionalPanel(condition="input.imputation_method == 'hot-deck'",
                                                                          sliderInput("slider_hotdeck",label = NULL,min = 1,max=100,value=50)
                                                         )
                                        )                                        
                       ),
                       conditionalPanel(condition="input.headtab == 8 & input.start_imputation==1 & input.keep_accept_refuse_outliers == 'refuse'",
                                        radioButtons("freeze_data", label = "Freeze data with imputations",
                                                     choices = list("Take a tour with imputations" = 'yes', "Keep outliers in your charts" = 'no'), selected = 'no')
                       ),
                       conditionalPanel(condition="input.headtab == 1",
                                        downloadButton('download_outliers_value', 'Download')),
                       conditionalPanel(condition="input.headtab == 2",
                                        downloadButton('download_outliers_parameter', 'Download')),
                       conditionalPanel(condition="input.headtab == 6 && input.zero_or_not_sent == 61",
                                        downloadButton('download_zero_checks', 'Download')),
                       conditionalPanel(condition="input.headtab == 6 && input.zero_or_not_sent == 62",
                                        downloadButton('download_not_sent', 'Download')),
                       br(),br(),
                       h5("Notes:"),
                       #verbatimTextOutput("perc_consegne_mensili"),
                       #verbatimTextOutput("perc_consegne_annuali"),
                       conditionalPanel(condition="input.headtab == 8",
                                        verbatimTextOutput("notes_on_fixing")) #textOutput("notes_on_fixing"))
                       ,textOutput("uti")
                     ),
                     
                     
                     mainPanel(
                       tabsetPanel(id = "headtab",
                                   tabPanel("Outliers detection: abs values",value = 1,
                                            plotOutput("boxplot_value") ,
                                            h4("Outliers:"),
                                            dataTableOutput("table_outliers_value")), 
                                   
                                   tabPanel("Outliers detection: mean values", value = 2,
                                            plotOutput("boxplot_parameter"),
                                            h4("Outliers:"),
                                            dataTableOutput("table_outliers_parameter")),
                                   
                                   tabPanel("Pie chart - % costs",value = 3,plotOutput("pie"),dataTableOutput("pie_data") ),
                                   
                                   tabPanel("Waterfall: costs and profit", value = 4,
                                            tabsetPanel(
                                              tabPanel("All", plotOutput("waterfall_plot_italy")),
                                              tabPanel("Grouped by strata", plotOutput("waterfall_plot_strata", height = 1000)),
                                              tabPanel("Grouped by gear", plotOutput("waterfall_plot_gear",height = 800)),
                                              tabPanel("Grouped by LOA", plotOutput("waterfall_plot_loa",height = 800)),
                                              tabPanel("Grouped by regione", plotOutput("waterfall_plot_regione",height = 800)),
                                              tabPanel("Grouped by gear * LOA", plotOutput("waterfall_plot_gear_loa",height = 1000))  
                                            )
                                   ),
                                   
                                   
                                   tabPanel("Free filters on the data",value = 5,dataTableOutput("table_free_filters")),
                                   
                                   tabPanel("Checks on zero values", value = 6,
                                            tabsetPanel(id="zero_or_not_sent",
                                                        tabPanel("Zero checks for sent data",value=61,dataTableOutput("zero_checks_dt")),
                                                        tabPanel("Not sent",value=62, dataTableOutput("not_sent_dt"))
                                            )
                                   ),
                                   
                                   tabPanel("Delivery status", value = 7, 
                                            tabsetPanel(
                                              tabPanel("Data collector level",dataTableOutput("table_consegne_ril") ),
                                              tabPanel("Strata level",dataTableOutput("table_consegne_strato"))
                                            )
                                   ),
                                   tabPanel("Imputation process", value = 8,                                           
                                              tabPanel("A",dataTableOutput("outliers_in_imputation_dt"),value = 'A' )    
                                   ),
                                   tabPanel("Upload", value = 9,
                                            dataTableOutput("upload_dt")
                                   )
                       )
                       
                       
                     )
                   )
)
)