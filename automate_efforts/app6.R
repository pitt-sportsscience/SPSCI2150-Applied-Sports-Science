#install.packages("shiny")
library(shiny)

server= function(input, output) { }

#Example 2
ui <- fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(
    sidebarPanel( "sidebar panel"),
    mainPanel("main panel")
  )
)
shinyApp(ui, server)

ui <- fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(position = "right",
                sidebarPanel( "sidebar panel"),
                mainPanel("main panel")
  )
)
shinyApp(ui, server)

