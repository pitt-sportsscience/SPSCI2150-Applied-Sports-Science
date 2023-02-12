#install.packages("shiny")
library(shiny)

server= function(input, output) { }

#Example 6
# create uniform and normal panels
ui <- fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id="tabset",
                  tabPanel("uniform",
                           sliderInput("unif_bins",
                                       "Number of bins:",
                                       min = 1,
                                       max = 50,
                                       value = 30),
                           numericInput("unif_count","Number of values",1000),
                           sliderInput("range",
                                       "Range of values:",
                                       min = -100,
                                       max = 100,
                                       value = c(-10,10))
                  ),
                  tabPanel("normal",
                           sliderInput("normal_bins",
                                       "Number of bins:",
                                       min = 1,
                                       max = 50,
                                       value = 30),
                           numericInput("normal_count","Number of values",1000),
                           numericInput("mean","Mean",0),
                           numericInput("sd","Standard deviation",1)
                  )
      )
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
    if(input$tabset=="uniform"){
      hist(runif(input$unif_count,input$range[1],input$range[2]),
           breaks=input$unif_bins,
           main="Histogram of uniform distribution",
           xlab="x")
    }
    if(input$tabset=="normal"){
      hist(rnorm(input$normal_count,input$mean,input$sd),
           breaks=input$normal_bins,
           main="Histogram of normal distribution",
           xlab="x")
    }
  })
}
shinyApp(ui, server)

