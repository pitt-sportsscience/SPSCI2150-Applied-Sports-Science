#install.packages("shiny")
library(shiny)

#example
runExample("01_hello")

#empty one ui.R
ui <- fluidPage()
server= function(input, output) { }
shinyApp(ui, server)