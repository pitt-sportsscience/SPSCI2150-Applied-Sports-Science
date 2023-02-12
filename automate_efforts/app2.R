#install.packages("shiny")
library(shiny)

#demo
ui <- fluidPage(
  titlePanel("Hello world!")
)
server= function(input, output) { }
shinyApp(ui, server)