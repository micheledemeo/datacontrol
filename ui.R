wd=getwd()
shinyUI(fluidPage(
  
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
                         plotOutput("boxplot") ,
                         h4("Outliers:"),
                         dataTableOutput("table_data")), 
                
                tabPanel("Outliers detection: mean values"),
    
                tabPanel("% control of costs",plotOutput("pie"),dataTableOutput("pie_data") ),
                
                tabPanel("Free filters on the data",dataTableOutput("table_free_filters")),
                
                tabPanel("From sample to population"),
                
                tabPanel("Delivery status", dataTableOutput("table_consegne"))
    )
  )
  
  
)))

