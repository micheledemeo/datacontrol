
shinyUI(fluidPage(
  
  titlePanel("Outliers control with boxplot"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #selectInput("var", label = "Choose a variable to analyze", choices =var_cho, selected = 'carbur')
      uiOutput("var"), 
      uiOutput("strato")
      
    ),
    
    mainPanel( plotOutput("boxplot") )
  )
))
