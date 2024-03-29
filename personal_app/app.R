library(shiny)
library(ggplot2)
library(tidyverse)

data <- read.csv("Mental health Depression disorder Data copy.csv")
names <- unique(data$Entity)
data$Schizophrenia.... <- as.numeric(data$Schizophrenia....)
data$Bipolar.disorder....<- as.numeric(data$Bipolar.disorder....)
data$Eating.disorders.... <- as.numeric(data$Eating.disorders....)

ui <- fluidPage(
  titlePanel("Mental Health Disorders Worldwide"),
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
    data %>% filter(Entity == input$dataset)
  })
  
  # Generate plot
    output$plot <- renderPlot({
      dataset <- datasetInput()
      new_data <- dataset %>%
        filter(Year >=1990, Eating.disorders.... <100) %>%
        pivot_longer(cols = Schizophrenia....:Alcohol.use.disorders...., names_to = "disorder_type", values_to = "percentage") %>%
        select(Year, disorder_type, percentage) %>%
        na.omit()
      ggplot(new_data) +
              aes(x = Year, y = percentage, color=disorder_type) +
              geom_point() +
              theme_bw() +
              theme(axis.text.x = element_text(colour = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5) 
              ) +
              labs(title = paste("Overall trend of mental health disorders in", input$dataset, "from 1990-2017"), x = "Year", y = paste("Percentage of", input$dataset, "population")) +
              scale_color_discrete(
                name = "Type of mental health\ndisorder",
                labels = c("Alcohol use disorder", "Anxiety disorder", "Bipolar disorder", "Depression", "Drug use disorder", "Eating disorder", "Schizophrenia")
              )
    })
  }


shinyApp(ui, server)