#install.packages("shiny")
library(shiny)

#demo
ui <- fluidPage(
  titlePanel("Hello world!"),
  titlePanel("Hello world!")
)
server= function(input, output) { }
shinyApp(ui, server)
