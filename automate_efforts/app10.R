#install.packages("shiny")
library(shiny)

server= function(input, output) { }

#Example 4
# add server function, create a shiny app that output histogram
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
                  value = 30)
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
    hist(runif(1000),breaks=input$bins)
  })
}
shinyApp(ui, server)

