#install.packages("shiny")
library(shiny)

#demo
ui <- fluidPage(
  titlePanel("Hello world!"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label = "Choices", choices = list("Choice 1" = 1, "Choice 2" = 2)),
      sliderInput("slider1", label = "Slider", min = 0, max = 100, value = 50)
    ),
    mainPanel("Testing")
  )
)
server= function(input, output) { }
shinyApp(ui, server)

