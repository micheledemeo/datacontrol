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

      
      
    ),

    
  mainPanel(
    tabsetPanel(
                tabPanel("Outliers detection: abs values",
                         plotOutput("boxplot") , 
                         dataTableOutput("table_data")), 
                
                tabPanel("Outliers detection: mean values"),
    
                tabPanel("% control of costs",plotOutput("pie"),dataTableOutput("pie_data") )
    )
  )
  
  
)))

