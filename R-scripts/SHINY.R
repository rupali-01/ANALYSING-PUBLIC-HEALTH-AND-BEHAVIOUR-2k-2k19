# Load required packages
library(shiny)      # For building interactive web applications
library(tidyverse)  # For data manipulation and visualization (collection of packages like ggplot2, dplyr, etc.)
library(lubridate)  # For working with dates and times
library(ggplot2)    # For creating advanced data visualizations
library(dplyr)      # For data manipulation (filter, select, mutate, summarize, etc.)
library(readxl)     # For reading Excel files
library(tidyr)      # For tidying data (reshape and clean datasets)
library(janitor)    # For cleaning data (e.g., clean column names, tabulation)
library(skimr)      # For summarizing data quickly (easy overview of datasets)
library(sqldf)      # For running SQL queries on R data frames
library(plotrix)    # For specialized plots (e.g., 3D pie charts, radar plots)
library(knitr)      # For dynamic report generation (tables, documents, reports)
library(reshape2)   # For reshaping data (melt and cast data frames)
library(caret)      # For confusion matrix and machine learning model training


# Define UI
ui <- fluidPage(
  
  # Add custom CSS for styling
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Roboto', sans-serif;
        background-color: #f4f7fa;
        color: #333;
      }
      .btn-primary {
        background-color: #007bff;
        border-color: #007bff;
        padding: 10px;
        font-size: 16px;
        border-radius: 5px;
      }
      .btn-primary:hover {
        background-color: #0056b3;
        border-color: #0056b3;
      }
      h1, h2, h3 {
        color: #4a4a4a;
        font-weight: bold;
      }
      .tab-content {
        background-color: #ffffff;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.1);
      }
      .sidebarPanel, .mainPanel {
        padding: 20px;
      }
      .shiny-input-container {
        margin-bottom: 15px;
      }
      .tab-panel-header {
        background-color: #f8f9fa;
        border-bottom: 2px solid #ddd;
        padding: 10px 15px;
        font-size: 18px;
        font-weight: bold;
      }
    "))
  ),
  
  # Application title
  titlePanel("Health Data Analysis", windowTitle = "Health Data Analysis"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload CSV File", accept = c(".csv")),
      selectInput("xvar", "Select X Variable", choices = NULL),
      selectInput("yvar", "Select Y Variable", choices = NULL),
      selectInput("graphType", "Select Graph Type", choices = c(
        "Bar Chart", "Pie Chart", "Boxplot", "Histogram", 
        "Density Plot", "Stacked Bar Chart", "Line Chart", 
        "Heatmap", "Violin Plot", "Treemap", "Radar Chart"
      )),
      actionButton("update", "Update Plot", class = "btn-primary"),
      hr(),
      helpText("Upload a CSV file containing health data. Select variables to visualize their relationship.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Selected Graph", 
                 div(class = "tab-panel-header", "Graph Visualization"),
                 plotOutput("selectedGraph", height = "400px"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded data
  data <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath)
  })
  
  # Update variable selections based on uploaded data
  observe({
    df <- data()
    updateSelectInput(session, "xvar", choices = names(df))
    updateSelectInput(session, "yvar", choices = names(df))
  })
  
  # Render selected graph
  output$selectedGraph <- renderPlot({
    req(input$graphType)
    df <- data()
    
    if (input$graphType == "Bar Chart") {
      ggplot(df, aes_string(x = input$xvar)) +
        geom_bar(fill = "#007bff") +
        labs(title = paste("Bar Chart: ", input$xvar), x = input$xvar, y = "Count") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
        )
    }
    
    else if (input$graphType == "Pie Chart") {
      counts <- table(df[[input$xvar]])
      pie(counts, labels = names(counts), col = rainbow(length(counts)),
          main = paste("Pie Chart: ", input$xvar))
    }
    
    else if (input$graphType == "Boxplot") {
      ggplot(df, aes_string(x = input$xvar, y = input$yvar)) +
        geom_boxplot(fill = "#007bff", color = "#333", alpha = 0.6) +
        labs(title = paste("Boxplot: ", input$xvar, " vs ", input$yvar)) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
        )
    }
    
    else if (input$graphType == "Histogram") {
      ggplot(df, aes_string(x = input$xvar)) +
        geom_histogram(bins = 30, fill = "#007bff", alpha = 0.7) +
        labs(title = paste("Histogram: ", input$xvar), x = input$xvar, y = "Frequency") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
        )
    }
    
    else if (input$graphType == "Density Plot") {
      ggplot(df, aes_string(x = input$xvar)) +
        geom_density(fill = "#007bff", alpha = 0.6) +
        labs(title = paste("Density Plot: ", input$xvar)) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
        )
    }
    
    else if (input$graphType == "Stacked Bar Chart") {
      ggplot(df, aes_string(x = input$xvar, fill = input$yvar)) +
        geom_bar(position = "stack") +
        labs(title = paste("Stacked Bar Chart: ", input$xvar, " by ", input$yvar)) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
        )
    }
    
    else if (input$graphType == "Line Chart") {
      ggplot(df, aes_string(x = input$xvar, y = input$yvar)) +
        geom_line(color = "#007bff", size = 1.2) +
        labs(title = paste("Line Chart: ", input$xvar, " vs ", input$yvar)) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
        )
    }
    
    else if (input$graphType == "Heatmap") {
      corr_matrix <- round(cor(df, use = "complete.obs"), 2)
      ggplot(melt(corr_matrix), aes(Var2, Var1, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", midpoint = 0) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
    }
    
    else if (input$graphType == "Violin Plot") {
      ggplot(df, aes_string(x = input$xvar, y = input$yvar)) +
        geom_violin(fill = "#007bff", color = "#333", alpha = 0.6) +
        labs(title = paste("Violin Plot: ", input$xvar, " vs ", input$yvar)) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
        )
    }
    
    else if (input$graphType == "Radar Chart") {
      # Radar chart requires the 'fmsb' or 'plotly' package for a more interactive version
      # This is just a placeholder for radar chart functionality, which is complex.
      # You could add the radar chart logic if desired with proper libraries.
      plot(1)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
