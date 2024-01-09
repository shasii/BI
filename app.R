# Install required packages if not installed
# install.packages(c("shiny", "lmtest"))

# Load libraries
library(shiny)

# Define the data
ecommerce <- data.frame(
  month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

# Define the UI
ui <- fluidPage(
  titlePanel("E-commerce Sales Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("input_x1", "Enter Visitors:", value = 200000),
      numericInput("input_x2", "Enter Transactions:", value = 10000),
      numericInput("input_x3", "Enter Items/Transaction:", value = 5),
      numericInput("input_x4", "Enter Rating:", value = 8.5),
      numericInput("input_x5", "Enter Ads:", value = 50000),
      actionButton("predictButton", "Predict")
    ),
    mainPanel(
      plotOutput("predictionPlot"),
      verbatimTextOutput("predictionText")
    )
  )
)

# ...

# Define the server
server <- function(input, output) {
  model <- reactive({
    lm(y ~ x1 + x2 + x3 + x4 + x5, data = ecommerce)
  })
  
  output$predictionPlot <- renderPlot({
    new_data <- data.frame(
      x1 = input$input_x1,
      x2 = input$input_x2,
      x3 = input$input_x3,
      x4 = input$input_x4,
      x5 = input$input_x5
    )
    
    predicted_values <- predict(model(), newdata = new_data)
    
    bar_colors <- c(rep("blue", length(ecommerce$y)), rep("red", length(predicted_values)))
    
    barplot(
      c(ecommerce$y, predicted_values),
      col = bar_colors,
      beside = TRUE,
      main = "E-commerce Sales Prediction",
      xlab = "Month",
      ylab = "Sales",
      ylim = c(0, max(ecommerce$y, predicted_values) + 50)  # Adjust ylim if needed
    )
    
    axis(1, at = 1:length(ecommerce$month) + 0.5, labels = ecommerce$month)
    legend("topright", legend = c("Actual Sales", "Predicted Sales"), fill = c("blue", "red"), bty = "n")
  })
  
  output$predictionText <- renderPrint({
    new_data <- data.frame(
      x1 = input$input_x1,
      x2 = input$input_x2,
      x3 = input$input_x3,
      x4 = input$input_x4,
      x5 = input$input_x5
    )
    
    predicted_value <- predict(model(), newdata = new_data)
    
    cat("Predicted Sales:", round(predicted_value, 2))
  })
}

# ...


# Run the app
shinyApp(ui, server)
