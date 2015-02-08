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
      
      actionButton("refresh", "Refresh data from remote server") ,br(), br(), uiOutput("str_in_wat") ,
      conditionalPanel(condition="input.headtab == 1 || input.headtab == 2", uiOutput("var"), br(), br() ),
      conditionalPanel(condition="input.headtab >= 1 && input.headtab <= 4 || input.headtab == 6",
                      uiOutput("codsis"),
                      uiOutput("codlft"),
                      actionButton("reset", "Reset filters"),
                      br(),
                      br(),
                      br(),
                      uiOutput("strato"),
                      br(),
                      br()
                      
      ),
      conditionalPanel(condition="input.headtab != 7", 
                       checkboxInput(inputId = "check_gio",label = "Fishing days>0", value=T)
      ),
      conditionalPanel(condition="input.headtab == 3 || input.headtab == 4", 
                       checkboxInput(inputId = "apply_weights",label = "Apply weights", value=T)
      ),
      br(),br(),
      h5("Notes:"),
      textOutput("perc_consegne_mensili"),
      textOutput("perc_consegne_annuali")
      #,textOutput("uti")
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
                             tabPanel("Grouped by gear * LOA", plotOutput("waterfall_plot_gear_loa",height = 1000)),
                             tabPanel("Grouped by gear", plotOutput("waterfall_plot_gear",height = 800)),
                             tabPanel("Grouped by LOA", plotOutput("waterfall_plot_loa",height = 800))
                           )
                         ),
                         
                
                tabPanel("Free filters on the data",value = 5,dataTableOutput("table_free_filters")),
                
                tabPanel("From sample to population",value = 6),
                
                tabPanel("Delivery status", value = 7, dataTableOutput("table_consegne"))
    )
  )
  
  
)))

