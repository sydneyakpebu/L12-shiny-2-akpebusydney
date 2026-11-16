#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(tidyverse)
library(ggthemes)

# Data
cdc <- read_delim("data/cdc.txt", delim = "|")

# UI
ui <- page_sidebar(
  
  # Application title
  title = "CDC BRFSS Histograms",
  
  # Sidebar
  sidebar = sidebar(
    position = "right",
    
    # Widget 1: Select Variable
    selectInput(
      inputId = "variable",
      label = "Select Variable:",
      choices = c(
        "Actual Weight" = "weight",
        "Desired Weight" = "wtdesire",
        "Height" = "height",
        "Age" = "age"
      ),
      selected = "weight"
    ),
    
    # Widget 2: Number of bins
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 5,
      max = 50,
      value = 30,
      animate = TRUE
    ),
    
    # Widget 3: Select Fill/Legend Variable
    radioButtons(
      inputId = "fill_var",
      label = "Select Fill/Legend Variable:",
      choices = c(
        "General Health" = "genhlth",
        "Health Coverage" = "hlthplan",
        "Exercised in Past Month" = "exerany",
        "Smoked 100 Cigarettes" = "smoke100",
        "Gender" = "gender"
      ),
      selected = "genhlth"
    )
  ),
  
  # Histogram output
  plotOutput(outputId = "distPlot")
)


# Server
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    ggplot(
      data = cdc,
      aes(
        x = .data[[input$variable]],
        fill = .data[[input$fill_var]]
      )
    ) +
      
      geom_histogram(
        bins = input$bins,
        position = "identity",
        alpha = 0.5
      ) +
      
      scale_fill_discrete(
        name = NULL,
        breaks = if (input$fill_var == "genhlth") {
          c("excellent", "very good", "good", "fair", "poor")
        } else {
          waiver()
        },
        labels = if (input$fill_var == "genhlth") {
          c("Excellent", "Very Good", "Good", "Fair", "Poor")
        } else {
          waiver()
        }
      ) +
      
      theme_fivethirtyeight(base_size = 20) +
      
      labs(
        title = switch(
          input$fill_var,
          genhlth = "General Health",
          hlthplan = "Health Coverage",
          exerany = "Exercised in Past Month",
          smoke100 = "Smoked 100 Cigarettes",
          gender = "Gender"
        ),
        x = switch(
          input$variable,
          weight = "Actual Weight in Pounds",
          wtdesire = "Desired Weight in Pounds",
          height = "Height in Inches",
          age = "Age"
        ),
        y = "Count"
      ) +
      
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box = "vertical",
        plot.title = element_text(hjust = 0.5)
      )
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)