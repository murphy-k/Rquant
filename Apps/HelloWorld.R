library(shiny)

ui <- fluidPage(
  'Hello World',
  textInput('name', 'What is your name?'),
  textOutput('greeting')
)

server <- function(input, output, session) {
  output$greeting <- renderText({
    paste('Hello,',input$name)
  })
}

shinyApp(ui, server)