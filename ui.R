
shinyUI(fluidPage(
  
  titlePanel("Outliers control with boxplot"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      uiOutput("var")
      ,uiOutput("strato")
      ,checkboxInput(inputId = "check_gio",label = "Filtra con giorni_mare>0", value=T)
      
    ),
    
    mainPanel( plotOutput("boxplot") )
  )
))
