library(shiny)
library(ggplot2)
library(dplyr)

data <- read.csv("Tyler_Dataset.csv")

# Creating subset
dataset <- data[, c("track_name", "album_name", "popularity", "danceability", "energy", "tempo", "year")]

# Converting years from numeric to categorical
dataset$year <- as.character(dataset$year)
ui <- fluidPage(
  
  titlePanel("Explore Tyler's Dataset"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variables", "Select Variables:", 
                  choices = c("popularity", "danceability", "energy", "tempo"), multiple = TRUE),
      
      sliderInput("filter", "Filter by Popularity:", min = 0, max = 100, value = c(25, 75)),
      
      radioButtons("color", "Choose Graph Color:", 
                   choices = c("Blue", "Red", "Green")),
      
      checkboxInput("showStats", "Show Descriptive Statistics", TRUE)
    ),
    
    mainPanel(
      h3("Instructions:"),
      p("Select one or two variables from the dropdown menu. 
        The app will create a univariate or multivariate graph 
        depending on the number of selected variables."),
      
      plotOutput("mainPlot", height = "400px", width = "100%"),
      
      conditionalPanel(
        condition = "input.showStats == true",
        verbatimTextOutput("statsOutput")
      ),
      
      textOutput("noDataMessage")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive function to filter data by popularity
  filteredData <- reactive({
    dataset %>%
      filter(popularity >= input$filter[1] & popularity <= input$filter[2]) %>%
      filter(album_name %in% unique(dataset$album_name))
  })
  
  # Rendering the plot with improved titles and axis labels
  output$mainPlot <- renderPlot({
    data <- filteredData()
    req(data)
    req(input$variables)
    
    if (length(input$variables) == 1) {
      selected_var <- input$variables[1]
      binwidth <- if (selected_var %in% c("danceability", "energy")) 0.25 else 10
      
      ggplot(data, aes_string(x = selected_var)) +
        geom_histogram(binwidth = binwidth, fill = input$color, color = "black") +
        labs(
          title = paste("Distribution of", selected_var, "for All Albums"),
          x = selected_var,
          y = "Frequency"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
      
    } else if (length(input$variables) == 2) {
      ggplot(data, aes_string(x = input$variables[1], y = input$variables[2])) +
        geom_point(color = input$color) +
        labs(
          title = paste(input$variables[1], "vs", input$variables[2], "for All Albums"),
          x = input$variables[1],
          y = input$variables[2]
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  # Rendering the descriptive statistics and correlation if multivariate
  output$statsOutput <- renderPrint({
    data <- filteredData()
    req(data)
    
    if (length(input$variables) == 1) {
      cat("Summary Statistics for", input$variables[1], ":\n")
      print(summary(data[[input$variables[1]]]))
      
    } else if (length(input$variables) == 2) {
      var1 <- input$variables[1]
      var2 <- input$variables[2]
      
      cat("Summary Statistics:\n")
      print(summary(data[c(var1, var2)]))
      
      correlation <- cor(data[[var1]], data[[var2]], use = "complete.obs")
      cat("\nCorrelation between", var1, "and", var2, ":", round(correlation, 2), "\n")
      
    } else {
      cat("Please select one or two variables for statistics.\n")
    }
  })
  
  # Display message if no data is available
  output$noDataMessage <- renderText({
    if (nrow(filteredData()) == 0) {
      "No data available. Please select different filter criteria."
    } else {
      NULL
    }
  })
}

shinyApp(ui = ui, server = server)
