# Install required packages if not installed
if (!require(shiny)) install.packages("shiny")

# Load required libraries
library(shiny)
library(DT)
library(tidyr)
library(ggplot2)
library(reshape2)

# Initial data
adclick <- data.frame(
  adplacement = c("day1", "day2", "day3", "day4", "day5", "day6", "day7", "day8", "day9", "day10"),
  leftsidebar = c(2.5, 2.7, 2.8, 2.6, 3, 2.4, 2.9, 2.5, 2.6, 2.7),
  centerpage = c(3.8, 3.5, 4, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  rightsidebar = c(3.1, 2.9, 3, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# Reshape the data for ANOVA
adclick_long <- reshape2::melt(adclick, id.vars = "adplacement", variable.name = "adposition", value.name = "clicks")

# Perform ANOVA for all ad positions
anova_result <- aov(clicks ~ adposition, data = adclick_long)

# Define UI
ui <- navbarPage(
  title = "CTR Data Entry & Analysis",
  tabPanel(
    "Data Entry",
    sidebarLayout(
      sidebarPanel(
        h4("Enter Data Manually"),
        textInput("adplacement", "Ad Placement:"),
        numericInput("leftsidebar", "Left Sidebar:", value = 0),
        numericInput("centerpage", "Center Page:", value = 0),
        numericInput("rightsidebar", "Right Sidebar:", value = 0),
        actionButton("addRowBtn", "Add Row"),
        br(),
        fileInput("file", "Upload Data (CSV only):"),
        br(),
        actionButton("loadDataBtn", "Load Data"),
        br(),
        actionButton("deleteInitialDataBtn", "Delete Initial Data")
      ),
      mainPanel(
        DTOutput("table"),
        downloadButton("downloadBtn", "Download Data")
      )
    )
  ),
  tabPanel(
    "Visualization",
    sidebarLayout(
      sidebarPanel(
        selectInput("selected_day", "Select Day:", choices = NULL),
        actionButton("visualizeBtn", "Visualize")
      ),
      mainPanel(
        plotOutput("overallCtrPlot"),
        plotOutput("dailyCtrPlot")
      )
    )
  ),
  tabPanel(
    "T-Test Analysis",
    sidebarLayout(
      sidebarPanel(
        h4("T-Test Analysis"),
        selectInput("variable1_ttest", "Variable 1:", choices = setdiff(colnames(adclick), "adplacement")),
        selectInput("variable2_ttest", "Variable 2:", choices = setdiff(colnames(adclick), "adplacement")),
        actionButton("run_ttest", "Run T-Test")
      ),
      mainPanel(
        verbatimTextOutput("summaryOutput")
      )
    )
  ),
  tabPanel(
    "ANOVA Analysis",
    sidebarLayout(
      sidebarPanel(
        h4("ANOVA Analysis"),
        verbatimTextOutput("anovaSummary"),
        plotOutput("boxplot")
      ),
      mainPanel(
        verbatimTextOutput("anovaResult")
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(data = adclick, available_days = unique(adclick$adplacement), anova_result = NULL)
  
  # Function to perform ANOVA
  perform_anova <- function() {
    adclick_long <- reshape2::melt(rv$data, id.vars = "adplacement", variable.name = "adposition", value.name = "clicks")
    rv$anova_result <- aov(clicks ~ adposition, data = adclick_long)
  }
  
  # Add row to the data
  observeEvent(input$addRowBtn, {
    new_row <- data.frame(
      adplacement = input$adplacement,
      leftsidebar = input$leftsidebar,
      centerpage = input$centerpage,
      rightsidebar = input$rightsidebar
    )
    rv$data <- rbind(rv$data, new_row)
    
    # Update available days
    rv$available_days <- unique(rv$data$adplacement)
    
    # Perform ANOVA on updated data
    perform_anova()
  })
  
  # Load data from file
  observeEvent(input$loadDataBtn, {
    if (!is.null(input$file)) {
      inFile <- input$file
      if (grepl("^.*\\.(csv)$", inFile$name)) {
        # Read the CSV file with sep = ";" and header = FALSE
        new_data <- read.csv(inFile$datapath, sep = ";", header = FALSE)
        
        # Set the column names
        col_names <- c("adplacement", "leftsidebar", "centerpage", "rightsidebar")
        names(new_data) <- col_names
        
        # Bind the new data to the existing data
        rv$data <- rbind(rv$data, new_data)
        
        # Update available days
        rv$available_days <- unique(rv$data$adplacement)
        
        # Perform ANOVA on updated data
        perform_anova()
      } else {
        showNotification("Invalid file format. Please upload a CSV file.", type = "warning")
      }
    }
  })
  
  # Update selected_day choices
  observe({
    updateSelectInput(session, "selected_day", choices = rv$available_days)
  })
  
  # Render table
  output$table <- renderDT({
    datatable(rv$data, editable = TRUE, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  })
  
  # Download button
  output$downloadBtn <- downloadHandler(
    filename = function() {paste("ctr_data_", Sys.Date(), ".csv", sep = "")},
    content = function(file) {
      write.csv(rv$data, file)
    }
  )
  
  # CTR Visualization
  output$overallCtrPlot <- renderPlot({
    # Update factor levels
    rv$data$adplacement <- factor(rv$data$adplacement, levels = rv$available_days)
    
    # Bar plot of overall CTR values
    ggplot(tidyr::gather(rv$data, key = "variable", value = "value", -adplacement), 
           aes(x = adplacement, fill = variable, y = value)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Overall CTR Performance",
           x = "Ad Placement",
           y = "CTR") +
      theme_minimal()
  })
  
  # Bar plot of daily CTR values
  output$dailyCtrPlot <- renderPlot({
    req(input$visualizeBtn)
    
    # Filter data for the selected day
    selected_day_data <- subset(rv$data, adplacement == input$selected_day)
    
    # Reshape data for easier plotting
    selected_day_data_long <- tidyr::gather(selected_day_data, key = "position", value = "CTR", -adplacement)
    
    # Bar plot of daily CTR values
    ggplot(selected_day_data_long, aes(x = adplacement, y = CTR, fill = position)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("CTR Performance for", input$selected_day),
           x = "Ad Placement",
           y = "CTR") +
      theme_minimal()
  })
  
  # T-test for illustration
  output$summaryOutput <- renderPrint({
    if (input$run_ttest) {
      req(!is.null(input$variable1_ttest), !is.null(input$variable2_ttest))
      
      t_test_result <- t.test(rv$data[, input$variable1_ttest], rv$data[, input$variable2_ttest])
      
      # Check significance level
      if (t_test_result$p.value < 0.05) {
        cat("T-Test Results are statistically significant at the 0.05 level.\n")
      } else {
        cat("T-Test Results are not statistically significant at the 0.05 level.\n")
      }
      
      # Print summary
      return(t_test_result)
    }
  })
  
  # ANOVA for illustration
  output$anovaResult <- renderPrint({
    req(!is.null(rv$anova_result))
    summary(rv$anova_result)
  })
  
  # Delete initial data
  observeEvent(input$deleteInitialDataBtn, {
    rv$data <- rv$data[0, ]  # Remove all rows
    rv$available_days <- character(0)  # Remove available days
    
    # Perform ANOVA on updated data
    perform_anova()
  })
}


# Run the app
shinyApp(ui, server)
