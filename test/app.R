library(shiny)

server=function(input, output) {
  
  output$plot1 <- renderPlot({plot(runif(input$rb))})
  output$plot2 <- renderPlot({plot(runif(input$rb))})
    
}



ui = shinyUI(navbarPage("Test multi page",
                        
  tabPanel("tab1",                      
          sidebarLayout(
            sidebarPanel(
              radioButtons("rb","Nr of obs:",choices = c("50 obs"=50,"300 obs"=300))
            ),
            mainPanel(plotOutput("plot1"))
          )
  
          ),
  
  tabPanel("tab2",                      
    sidebarLayout(
      sidebarPanel(
        radioButtons("rb","Nr of obs:",choices = c("50 obs"=50,"300 obs"=300))
       ),
      mainPanel(plotOutput("plot2"))
    )
    
  )
  
))


shinyApp(ui = ui, server = server)

runApp("app")