library(shiny)
ui <- fluidPage(
    titlePanel("Regression Model (Dataset: Swiss)"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "outcome",
                label = h3("Outcome"),
                choices = list(
                    "Fertility" = "Fertility",
                    "Agriculture" = "Agriculture",
                    "Examination" = "Examination",
                    "Education" = "Education",
                    "Catholic" = "Catholic",
                    "Infant.Mortality" = "Infant.Mortality"
                ),
                selected = 1
            ),
            
            selectInput(
                "indepvar",
                label = h3("Explanatory variable"),
                choices = list(
                    "Fertility" = "Fertility",
                    "Agriculture" = "Agriculture",
                    "Examination" = "Examination",
                    "Education" = "Education",
                    "Catholic" = "Catholic",
                    "Infant.Mortality" = "Infant.Mortality"
                ),
                selected = 1
            )
            
        ),
        
        mainPanel(
            tabsetPanel(
                type = "tabs",
                
                tabPanel("Scatterplot", plotOutput("scatterplot")),
                # Plot
                tabPanel("Distribution", # Plots of distributions
                         fluidRow(
                             column(6, plotOutput("distribution1")),
                             column(6, plotOutput("distribution2"))
                         )),
                tabPanel("Model Summary", verbatimTextOutput("summary")),
                # Regression output
                tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                
            )
        )
    )
)

# SERVER
server <- function(input, output) {
    # Regression output
    output$summary <- renderPrint({
        fit <- lm(swiss[, input$outcome] ~ swiss[, input$indepvar])
        names(fit$coefficients) <- c("Intercept", input$var2)
        summary(fit)
    })
    
    # Data output
    output$tbl = DT::renderDataTable({
        DT::datatable(swiss, options = list(lengthChange = FALSE))
    })
    
    
    # Scatterplot output
    output$scatterplot <- renderPlot({
        plot(
            swiss[, input$indepvar],
            swiss[, input$outcome],
            main = "Scatterplot",
            xlab = input$indepvar,
            ylab = input$outcome,
            pch = 19
        )
        abline(lm(swiss[, input$outcome] ~ swiss[, input$indepvar]), col =
                   "red")
        lines(lowess(swiss[, input$indepvar], swiss[, input$outcome]), col =
                  "blue")
    }, height = 400)
    
    
    # Histogram output var 1
    output$distribution1 <- renderPlot({
        hist(swiss[, input$outcome], main = "", xlab = input$outcome)
    }, height = 300, width = 300)
    
    # Histogram output var 2
    output$distribution2 <- renderPlot({
        hist(swiss[, input$indepvar], main = "", xlab = input$indepvar)
    }, height = 300, width = 300)
}

shinyApp(ui = ui, server = server)