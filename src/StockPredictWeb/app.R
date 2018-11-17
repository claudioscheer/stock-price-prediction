library(shiny)
library(shinythemes)

load("../linear-regression-model.Rdata")

ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel(""),
                sidebarLayout(
                  sidebarPanel(
                    dateInput("txtDateValue", "Date value (yyyy-MM-dd):", format = "yyyy-mm-dd"),
                    numericInput("txtOpenValue", "Open value:", 1),
                    numericInput("txtHighValue", "High value:", 1),
                    numericInput("txtLowValue", "Low value:", 1),
                    actionButton("Predict", "Predict", class = "btn btn-primary btn-lg btn-block")
                  ),
                  mainPanel(
                    h4("Predict the opening value in the stock market"),
                    hr(),
                    h5("Enter the values and press 'Predict'."),
                    h6(htmlOutput("Result"))
                  )
                ))

server <- function(input, output) {
  predictCloseValue <- function() {
    date <- as.numeric(as.Date(input$txtDateValue, "%Y-%m-%d"))
    df <- data.frame(
      Date = date,
      Open = input$txtOpenValue,
      High = input$txtHighValue,
      Low = input$txtLowValue
    )
    y <-
      predict(regression_model, df, interval = "prediction")
    print(y)
    return(y[1])
  }
  
  observeEvent(input$Predict, {
    result <-
      paste(
        sep = "",
        "<pre>The opening value at <b>",
        input$txtDateValue,
        "</b> is predict to be: <h1>",
        trimws(round(predictCloseValue(), digits = 2)),
        "</h1></pre>"
      )
    output$Result <- renderText({
      result
    })
  })
}

shinyApp(ui = ui, server = server)
