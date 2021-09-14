library(shiny)
library(ggplot2)
# Plot Output ####
ui <- fluidPage(
  textInput('name', 'Enter Name', 'David'),
  # CODE BELOW: Display the plot output named 'trend'
  plotOutput("trend")
)
server <- function(input, output, session) {
  # CODE BELOW: Render an empty plot and assign to output named 'trend'
  output$trend <- renderPlot({
    ggplot()
  })  
}
shinyApp(ui = ui, server = server)

# sidebarLayout ####
ui <- fluidPage(
  titlePanel("Baby Name Explorer"),
  # CODE BELOW: Add a sidebarLayout, sidebarPanel, and mainPanel
  sidebarLayout(
    sidebarPanel(textInput('name', 'Enter Name', 'David')),
    mainPanel(plotOutput('trend'))
    
  ))

server <- function(input, output, session) {
  output$trend <- renderPlot({
    ggplot()
  })
}
shinyApp(ui = ui, server = server)