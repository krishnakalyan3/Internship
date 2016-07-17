library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  img(src='eric.png', align = "right",width=100, height=75),
  titlePanel("Energy Simulator"),
  
  
  sidebarLayout(
    sidebarPanel( 
      fluidRow(
        column(8,
               radioButtons("radio", label = h5("Simulation Type"),
                            choices = list("Wind" = 1, "Solar" = 2, "Nuclear" = 3, "Hydro" = 4, "Thermal" = 5), 
                            selected = 1),
               dateRangeInput("dates", label = h5("Date range"), start='2005-01-01',end='2005-01-02'),
               textInput("var", label = h5("Jitter"), value = 0),
               
               column(12,
                      "",
                      fluidRow(
                        column(5,
                               submitButton("Submit")),
                        column(width = 5,
                               downloadButton('downloadData', 'Download'))
                      ))
        )
        
      )
      
      ),
    mainPanel(
      
      tabsetPanel(
      tabPanel("Plot",br(), plotOutput("plot1")),
      tabPanel("Table", 
               br(),
               dataTableOutput("table"))
      #tabPanel("Forecast",br(), plotOutput("plot2"))
      
      #plotOutput("plot1"),
      #textOutput("chk1")
    )
    )
  )
  
  
))