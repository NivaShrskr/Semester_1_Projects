library(shiny)

ui <- fluidPage(
  
titlePanel('Dice Roll App'),
  
  sidebarLayout(
    sidebarPanel(
    
      selectInput("num_dice", label = h3("Number of Dice:"),
                  choices = list("Two Dice" = 2, "Three Dice" = 3),
                  selected = 2),
      hr(),
      
      selectInput("num_rolls", label = h3("Number of Rolls:"),
                  choices = list("10 Rolls" = 10, "50 Rolls" = 50, 
                                 "100 Rolls" = 100, "500 Rolls" = 500),
                  selected = 10),
      hr()
    ),

    mainPanel(
      p('Number of Dice Rolled:'),
      fluidRow(verbatimTextOutput("numDice")),
      p('Number of Rolls:'),
      fluidRow(verbatimTextOutput("numRolls")),
      p('Histogram of Sums:'),
      fluidRow(plotOutput("histogram"))
    )
  ), 
tags$iframe(
  src = "https://www.youtube.com/embed/93lrosBEW-Q", # Replace VIDEO_ID with the YouTube video's ID
  width = "640",
  height = "360",
  frameborder = "0",
  allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
  allowfullscreen = FALSE
)
)



server <- function(input, output) {
  dice_rolls <- reactive({
    num_dice <- as.numeric(input$num_dice)
    num_rolls <- as.numeric(input$num_rolls)
    rolls <- matrix(sample(1:6, num_dice * num_rolls, replace = TRUE), 
                    ncol = num_dice)
    return(rolls)
  })
  
  output$numDice <- renderPrint({
    as.numeric(input$num_dice)
  })
  
  output$numRolls <- renderPrint({
    as.numeric(input$num_rolls)
  })
  
  output$rollResults <- renderPrint({
    dice_rolls()
  })

  output$histogram <- renderPlot({
    rolls <- dice_rolls()
    roll_sums <- rowSums(rolls)
    hist(roll_sums, main = "Histogram of Dice Sums", xlab = "Sum", 
         ylab = "Frequency", col = "lightblue", border = "black")
  })
}

shinyApp(ui = ui, server = server)
