source( pastedir(wd , "source/sidebarPanel_master.R") )

shinyUI(
  
  #sidebarLayout(master_panel),
  
  
  navbarPage("Nicoda: Survey Data Processing", theme = shinytheme("flatly"),id = "nav",
  
                   #sidebarLayout()
  tabPanel("Outliers detection: abs values",value = 1,
           
                           
             mainPanel( 
               plotOutput("boxplot_value") ,
               h4("Outliers:"),
               dataTableOutput("table_outliers_value") 
             )
             
           
  ),
  
  
  tabPanel("Outliers detection: mean values",
               
             
             mainPanel(
               plotOutput("boxplot_parameter"),
               h4("Outliers:"),
               dataTableOutput("table_outliers_parameter")
             )
             
           )
  )
#   
#   tabPanel("Costs and profit",
#            sidebarLayout(
#              
#              sidebarPanel( source( paste(getwd(), "source/sidebarPanel_master.R", sep="/") ) ),
#              
#              mainPanel( 
#                
#              )
#              
#            )
#   ),
#   
#   tabPanel("% control of costs",
#            sidebarLayout(
#              
#              sidebarPanel( source( paste(getwd(), "source/sidebarPanel_master.R", sep="/") ) ),
#              
#              mainPanel( 
#                plotOutput("pie"),
#                dataTableOutput("pie_data")
#               )
#              
#            )
#   ),
#   
#   tabPanel("Free filters on the data",
#            sidebarLayout(
#              
#              sidebarPanel( source( paste(getwd(), "source/sidebarPanel_master.R", sep="/") ) ),
#              
#              mainPanel( 
#                dataTableOutput("table_free_filters")
#              )
#              
#            )
#   ),
#   
#   tabPanel("From sample to population",
#            sidebarLayout(
#              
#              sidebarPanel( source( paste(getwd(), "source/sidebarPanel_master.R", sep="/") ) ),
#              
#              mainPanel( 
#              )
#              
#            )
#   ),
#   
#   tabPanel("Delivery status",
#            sidebarLayout(
#              
#              sidebarPanel( source( paste(getwd(), "source/sidebarPanel_master.R", sep="/") ) ),
#              
#              mainPanel( 
#                dataTableOutput("table_consegne")
#              )
#              
#            )
#   )
  
  
)

