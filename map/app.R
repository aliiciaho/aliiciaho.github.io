library(shiny)
library(ggplot2)
library(tidyverse)
library(geojsonio)

new_data <- read.csv("refined_NMdata.csv")
countries <- geojsonio::geojson_read("https://raw.githubusercontent.com/lvictory/maps-data/master/world.geo.json", 
                                     what = "sp")

# Cleaning data (omitting regions)
spaces_indices <- which(trimws(new_data$Code) == "")
cleaned_data <- new_data[-spaces_indices, ] %>% 
  filter(Year >= 1990)
cleaned_data$Code

# Cleaning table data
matching_values <- cleaned_data$Code %in% countries$id 
matched_dataset1 <- cleaned_data[matching_values, ]

ui <- fluidPage(
  titlePanel("Interactive Heat Map"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput('dataset', 'Choose Country', choices = NULL)
    ),
    mainPanel(
      # Output: Header + plot
      h4("Plot"),
      plotOutput("plot")
    )))

server <- function(input, output, session){
  
  updateSelectizeInput(session, 'dataset', choices = names, server = TRUE)
  
  datasetInput <- eventReactive(input$dataset, {
    matched_dataset1 %>% filter(Entity == input$dataset)
  })
  
  # Generate plot
  output$plot <- renderPlot({
    dataset <- datasetInput()
    app_data <- dataset %>%
      filter(Year >=1990, Eating.disorders.... <100) %>%
      pivot_longer(cols = Schizophrenia....:Alcohol.use.disorders...., names_to = "disorder_type", values_to = "percentage") %>%
      select(Year, disorder_type, percentage) %>%
      na.omit()
    ggplot(app_data) +
      aes(x = Year, y = percentage, group = disorder_type, color=disorder_type) +
      geom_point() +
      geom_line() +
      theme_bw() +
      theme(axis.text.x = element_text(colour = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5) 
      ) + 
      labs(title = paste("Overall trend of mental health disorders in", input$dataset, "from 1990-2017"), x = "Year", y = paste("Percentage of", input$dataset, "population")) +
      scale_color_discrete(
        name = "Type of mental health\ndisorder",
        labels = c("Alcohol use disorder", "Anxiety disorder", "Bipolar disorder", "Depression", "Drug use disorder", "Eating disorder", "Schizophrenia")
      ) +
      scale_y_continuous(minor_breaks = seq(1, 5, 0.2)) +
      scale_x_continuous(breaks = seq(1990, 2017, 1))
  })
}


shinyApp(ui, server)