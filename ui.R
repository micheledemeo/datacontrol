
shinyUI(fluidPage(
  
  titlePanel("Outliers control with boxplot"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      uiOutput("var"), 
      uiOutput("strato")
      
    ),
    
    mainPanel( plotOutput("boxplot") )
  )
))
