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
      
      uiOutput("var")
      ,br()
      ,br()
      ,br()
      ,uiOutput("codsis")
      ,uiOutput("codlft")
      ,actionButton("reset", "Reset")
      ,br()
      ,br()
      ,br()
      ,uiOutput("strato")
      ,br()
      ,br()
      ,br()
      ,checkboxInput(inputId = "check_gio",label = "Fishing days>0", value=T)
      ,br()
      ,br()
      ,br()
      ,h5("Notes:")
      ,textOutput("perc_consegne_mensili")
      ,textOutput("perc_consegne_annuali")
      
    ),

    
  mainPanel(
    tabsetPanel(
                tabPanel("Outliers detection: abs values",
                         plotOutput("boxplot_value") ,
                         h4("Outliers:"),
                         dataTableOutput("table_outliers_value")), 
                
                tabPanel("Outliers detection: mean values",
                         plotOutput("boxplot_parameter"),
                         h4("Outliers:"),
                         dataTableOutput("table_outliers_parameter")),
    
                tabPanel("Control of % costs",plotOutput("pie"),dataTableOutput("pie_data") ),
                
                tabPanel("Waterfall: costs and profit",
                           tabsetPanel(
                             tabPanel("Italy", plotOutput("waterfall_plot_italy")),
                             tabPanel("Grouped by strata", plotOutput("waterfall_plot_strata",height = 800)),
                             tabPanel("Grouped by gear * LOA", plotOutput("waterfall_plot_gear_loa",height = 800)),
                             tabPanel("Grouped by gear", plotOutput("waterfall_plot_gear",height = 800)),
                             tabPanel("Grouped by LOA", plotOutput("waterfall_plot_loa",height = 800))
                           )
                         ),
                         
                
                tabPanel("Free filters on the data",dataTableOutput("table_free_filters")),
                
                tabPanel("From sample to population"),
                
                tabPanel("Delivery status", dataTableOutput("table_consegne"))
    )
  )
  
  
)))

