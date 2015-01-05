wd=getwd()
shinyUI(fluidPage(
  
  titlePanel("Outliers control with boxplot"),
  
    
  sidebarLayout(
    
      sidebarPanel(
        
        fluidRow(
          column(12,
                 h5("Test page:"),
                 actionButton("action", label = "Action"),
                 br())
          
        ),
        
        br(),
      
      uiOutput("var")
      ,uiOutput("codsis")
      ,uiOutput("codlft")
      ,uiOutput("strato")
      ,checkboxInput(inputId = "check_gio",label = "Filtra con giorni_mare>0", value=T)
      
      
    ),

    
    mainPanel( plotOutput("boxplot") ),
    
    
  ),
  
  fluidRow(
  
    column(1,
           h3("Test page:"),
           actionButton("action", label = "Action"),
           br(),
           br(), 
           actionButton("action2", label = "Action2"))
 
  )
))

