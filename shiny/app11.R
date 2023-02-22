#install.packages("shiny")
library(shiny)

server= function(input, output) { }

#Example 5
# Add numeric input box and slider bar
ui <- fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      numericInput("count","Number of values",1000),
      sliderInput("range",
                  "Range of values:",
                  min = -100,
                  max = 100,
                  value = c(-10,10))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

#
server <- function(input, output){
  output$distPlot <- renderPlot({
    hist(runif(input$count,input$range[1],input$range[2]),breaks=input$bins)
  })
}
shinyApp(ui, server)

