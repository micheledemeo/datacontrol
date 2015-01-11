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
      ,uiOutput("codsis")
      ,uiOutput("codlft")
      ,uiOutput("strato")
      ,checkboxInput(inputId = "check_gio",label = "Fishing days>0", value=T)
      
      
    ),

    
  mainPanel(  
    tabsetPanel(
                tabPanel("Outliers detection: abs values", 
                         plotOutput("boxplot") , 
                         dataTableOutput("table_data")), 
                tabPanel("Outliers detection: par values"), 
                tabPanel("Profit and loss account - %"))
    )
  )
  
  
))

