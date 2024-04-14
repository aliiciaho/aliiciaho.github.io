#Creating map
library(shiny)
library(geojsonio)
library(leaflet)
library(magrittr)
library(tidyverse)

countries <- geojsonio::geojson_read("https://raw.githubusercontent.com/lvictory/maps-data/master/world.geo.json", 
                                     what = "sp")
class(countries)
names(countries)

new_data <- read.csv("refined_NMdata copy.csv")

# Cleaning data (omitting regions)
spaces_indices <- which(trimws(new_data$Code) == "")
cleaned_data <- new_data[-spaces_indices, ] %>% 
  filter(Year >= 1990)

# Cleaning table data
matching_values <- cleaned_data$Code %in% countries$id 
matched_dataset1 <- cleaned_data[matching_values, ]

map_data <- matched_dataset1 %>% filter(Year == 2017) %>% 
  arrange(Entity) %>%
  rename(index = index,
         Entity = Entity,
         Code = Code,
         Year = Year,
         Schizophrenia = Schizophrenia....,
         Bipolar_Disorder = Bipolar.disorder....,
         Eating_Disorder = Eating.disorders....,
         Anxiety_Disorder = Anxiety.disorders....,
         Druguse_Disorder = Drug.use.disorders....,
         Depression = Depression....,
         Alcoholuse_Disorder = Alcohol.use.disorders....)

map_data$Entity[map_data$Entity == "Cote d'Ivoire"] <- "Ivory Coast"
map_data$Entity[map_data$Entity == "Congo"] <- "Republic of the Congo"
map_data$Entity[map_data$Entity == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
map_data$Entity[map_data$Entity == "Timor"] <- "East Timor"
map_data$Entity[map_data$Entity == "Bahamas"] <- "The Bahamas"
map_data$Entity[map_data$Entity == "Serbia"] <- "Republic of Serbia"
map_data$Entity[map_data$Entity == "United States"] <- "United States of America"
map_data$Entity[map_data$Entity == "Tanzania"] <- "United Republic of Tanzania"
map_data$Entity[map_data$Entity == "Guinea-Bissau"] <- "Guinea Bissau"
map_data$Entity[map_data$Entity == "Palestine"] <- "West Bank"

new_obs <- data.frame(
  index = c(169:177),
  Entity = c("French Southern and Antarctic Lands", "Northern Cyprus", "Falkland Islands", "French Guiana", "New Caledonia", "Western Sahara", "South Sudan", "Somaliland", "Kosovo"),
  Code = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  Year = c(2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017),
  Schizophrenia = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  Bipolar_Disorder = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  Eating_Disorder = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  Anxiety_Disorder = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  Druguse_Disorder = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  Depression = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  Alcoholuse_Disorder = c(NA, NA, NA, NA, NA, NA, NA, NA, NA)
)

map_data <- rbind(map_data, new_obs)

missing_countries <- setdiff(unique(countries$name), unique(map_data$Entity))
print(missing_countries)
reordered_data <- map_data[match(countries$name, map_data$Entity), ]

merged_sp <- sp::merge(countries, reordered_data, by.x = "name", by.y = "Entity")

ui <- fluidPage(
  titlePanel("Interactive Heat Map"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput('dataset', 'Choose Disorder', choices = list("Alcohol Use Disorder" = "Alcoholuse_Disorder", 
                                                                  "Anxiety Disorder" = "Anxiety_Disorder", 
                                                                  "Bipolar Disorder" = "Bipolar_Disorder",
                                                                  "Depression", 
                                                                  "Drug Use Disorder" = "Druguse_Disorder",
                                                                  "Eating Disorder" = "Eating_Disorder",
                                                                  "Schizophrenia"
      ))
    ),
    mainPanel(
      # Output: Header + plot
      h4("Plot"),
      leafletOutput("plot")
    )))

server <- function(input, output, session){
  
  datasetInput <- eventReactive(input$dataset, {
    reordered_data %>% filter(Year == 2017) %>% select(Entity, !!sym(input$dataset)) %>% arrange(Entity)
  })
  
  binValues <- reactive({
    dataset <- datasetInput()
    if (input$dataset == "Alcoholuse_Disorder") {
      c(0, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5, Inf)
    } else if(input$dataset == "Depression") {
      c(0, 2, 2.5, 3, 3.5, 4, 5, Inf)
    } else if(input$dataset == "Anxiety_Disorder") {
      c(0, 2, 3, 4, 5, 6, Inf)
    } else if(input$dataset == "Bipolar_Disorder") {
      c(0, 0.5, 0.75, 1, Inf)
    } else if(input$dataset == "Druguse_Disorder") {
      c(0, 0.5, 1, 2, 3, Inf)
    } else if(input$dataset == "Eating_Disorder") {
      c(0, 0.25, 0.5, 0.75, Inf)
    } else if(input$dataset == "Schizophrenia") {
      c(0, 0.15, 0.2, 0.25, 0.3, Inf)
    }
    else {
      NULL
    }
  })
  
  labels <- reactive({
    dataset <- datasetInput()
    lapply(seq(nrow(countries)), function(i) {
      country <- countries$name[i]
      country_id <- countries$id[i]
      country_data <- map_data[map_data$Code == country_id, ]
      p <- round(country_data[[input$dataset]][1], 4)
      label_text <- sprintf(
        "<strong>%s</strong> %s<br/> %s", 
        country, "(2017)", p
      )
      return(htmltools::HTML(label_text))
    })
  })
  
  
  # Generate plot
  output$plot <- renderLeaflet({
    bins <- binValues()
    mypalette <- colorBin(
      palette = "YlOrRd", domain = merged_sp[[input$dataset]], bins = bins
    )
    
    labels_value <- labels()
    
    print(datasetInput()[, 2])
    print(datasetInput())
    print(countries$name)
    print(reordered_data)
    print(map_data)
    
    m <- leaflet(merged_sp) %>%
      setView(-96, 37.8, 4) %>%
      addTiles("MapBox", options = tileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    m %>% addPolygons(
      fillColor = ~mypalette(merged_sp[[input$dataset]]),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels_value,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    ) %>% 
      addLegend(
        pal = mypalette, values = binValues(), opacity = 0.9,
        title = paste(input$dataset), position = "bottomleft"
      )
  })
}


shinyApp(ui, server)