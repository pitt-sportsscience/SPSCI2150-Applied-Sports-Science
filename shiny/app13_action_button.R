#install.packages("shiny")
library(shiny)

server= function(input, output) { }

#Example 7
# Add plot button
ui <- fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id="tabset",
                  tabPanel("uniform",
                           numericInput("unif_count","Number of values",1000),
                           sliderInput("range",
                                       "Range of values:",
                                       min = -100,
                                       max = 100,
                                       value = c(-10,10))
                  ),
                  tabPanel("normal",
                           numericInput("normal_count","Number of values",1000),
                           numericInput("mean","Mean",0),
                           numericInput("sd","Standard deviation",1)
                  )
      ),
      actionButton("run", "Plot")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

#
server <- function(input, output){
  data=eventReactive(input$run,{
    if(input$tabset=="uniform"){
      runif(input$unif_count,input$range[1],input$range[2])
    } else {
      rnorm(input$normal_count,input$mean,input$sd)
    }
  })
  output$distPlot <- renderPlot({
    hist(data())
  })
}
shinyApp(ui, server)

