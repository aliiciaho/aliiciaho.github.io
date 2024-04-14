new_data <- read.csv("/Users/aliciaho/Documents/GitHub/aliiciaho.github.io/personal_app/refined_NMdata.csv")

# Cleaning data (omitting regions)
spaces_indices <- which(trimws(new_data$Code) == "")
cleaned_data <- new_data[-spaces_indices, ] %>% 
  filter(Year >= 1990)
cleaned_data$Code

# Cleaning table data
matching_values <- cleaned_data$Code %in% countries$id 
matched_dataset1 <- cleaned_data[matching_values, ]
print(matched_dataset1)
unique(matched_dataset1$Code)

#Finding what countries will have NA values (what is -99)
matching_values2 <- countries$id %in% cleaned_data$Code
countries$id[matching_values2 == FALSE]

#map data
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
map_data

#singapore data graph (test)
newsgp_data <- data %>% select(-c(index, Entity)) %>%
  filter(Code == "SGP", Year >=1990, Eating.disorders.... <100) %>%
  pivot_longer(cols = Schizophrenia....:Alcohol.use.disorders...., names_to = "disorder_type", values_to = "percentage") %>%
  select(Year, disorder_type, percentage) %>%
  na.omit()
print(ggplot(newsgp_data) +
        aes(x = Year, y = percentage, color=disorder_type) +
        geom_point() +
        theme_bw() +
        theme(axis.text.x = element_text(colour = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5) 
        ) +
        labs(title = "Overall trend of mental health disorders in Singapore from 1990-2017", x = "Year", y = "Percentage of Singapore population") +
        scale_color_discrete(
          name = "Type of mental health\ndisorder",
          labels = c("Alcohol use disorder", "Anxiety disorder", "Bipolar disorder", "Depression", "Drug use disorder", "Eating disorder", "Schizophrenia")
        )
)

#Initial tests
```{r eval=TRUE, include=FALSE}
library(tidyverse)
data <- read.csv("/Users/aliciaho/Desktop/nus yst/NM2207/NM2207 Project/Mental health Depression disorder Data.csv")
small_data <- data %>% select(-c(index, Code))
```

Plot showing the trend of depression in Singapore from 1990-2017

```{r eval = TRUE, echo=FALSE}
sgp_data <- data %>% select(-c(index, Entity)) %>%
  filter(Code == "SGP", Year >=1990) %>%
  select(Year, Depression....) %>%
  na.omit()
ggplot(sgp_data) +
  aes(x = Year, y = Depression....) + 
  geom_point()+
  labs(x = "Year", y = "Percentage of population experiencing depression", title = "Trend of depression rates in Singapore from 1990-2017") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5)
  )
```

library(plotly)
overall_median <- matched_dataset1 %>%
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
         Alcoholuse_Disorder = Alcohol.use.disorders....) %>%
  pivot_longer(cols = Schizophrenia:Alcoholuse_Disorder, names_to = "disorder_type", values_to = "percentage") %>%
  group_by(Year, disorder_type) %>% summarise(median_year = median(percentage)) %>%
  ungroup()

overall_median$text <- paste("Year:", overall_median$Year,
                             "<br>Disorder Type:", overall_median$disorder_type,
                             "<br>Median Percentage:", round(overall_median$median_year, 2))

 p <- ggplot(overall_median) +
  aes(x = Year, y = median_year, color = disorder_type, text = text) + 
  geom_point(shape="cross") +
  labs(y = "Median Value of % of Population across all countries") +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5) 
  ) +
   scale_color_discrete(
     name = "Type of mental health\ndisorder",
     labels = c("Alcohol use disorder", "Anxiety disorder", "Bipolar disorder", "Depression", "Drug use disorder", "Eating disorder", "Schizophrenia")
   )
 ggplotly(p, tooltip = "text")
 
 
ggplot(matched_dataset1 %>% filter(Year == 2017) %>%
  select(Entity, Code, Year, Schizophrenia....)) +
  aes(x=Schizophrenia....) + geom_histogram()

####
labels <- reactive({
  dataset <- datasetInput()
  disorder_data <- map_data %>%
    filter(Year == 2017) %>%
    select(Entity, !!input$dataset)
  
  # Join with countries data to get the country names
  country_labels <- left_join(disorder_data, countries, by = c("Entity" = "id"))
  
  # Create label text
  label_text <- sprintf(
    "<strong>%s</strong> (2017)<br/> %s: %s",
    country_labels$name, 
    input$dataset, 
    as.character(country_labels[[input$dataset]])
  )
  return(htmltools::HTML(label_text)) 
})





country_data[country_data$Entity == country_id, input$dataset]



# important map things

```{r eval=TRUE, include=FALSE}
#Creating map
library(geojsonio)
library(leaflet)
library(magrittr)
library(tidyverse)

countries <- geojsonio::geojson_read("https://raw.githubusercontent.com/lvictory/maps-data/master/world.geo.json", 
                                     what = "sp")
class(countries)
names(countries)

new_data <- read.csv("/Users/aliciaho/Documents/GitHub/aliiciaho.github.io/personal_app/refined_NMdata.csv")

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

m <- leaflet(countries) %>%
  setView(-96, 37.8, 4) %>%
  addTiles("MapBox", options = tileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

labels <- lapply(seq(nrow(countries)), function(i) {
  country <- countries$name[i]
  country_id <- countries$id[i]
  country_data <- map_data[map_data$Code == country_id, ]
  label_text <- sprintf(
    "<strong>%s</strong> %s<br/> <span style='color: #FF6666;'>%s: %s</span> <br/><span style='color: #CC9900;'>%s: %s</span> <br/><span style='color: #33CC33;'>%s: %s</span> <br/><span style='color: #33FF99;'>%s: %s</span> <br/><span style='color: #00CCFF;'>%s: %s</span> <br/><span style='color: #CC99FF;'>%s: %s</span> <br/><span style='color: #FF66CC;'>%s: %s</span>",
    country, "(2017)", 
    "Alcohol Use Disorder", country_data$Alcoholuse_Disorder,
    "Anxiety Disorder", country_data$Anxiety_Disorder,
    "Bipolar Disorder", country_data$Bipolar_Disorder,
    "Depression", country_data$Depression,
    "Drug Use Disorder", country_data$Druguse_Disorder,
    "Eating Disorder", country_data$Eating_Disorder,
    "Schizophrenia", country_data$Schizophrenia
  )
  return(htmltools::HTML(label_text))
})

mybins <- c(0, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5, Inf)
mypalette <- colorBin(
  palette = "YlOrRd", domain = map_data$Alcoholuse_Disorder, bins = mybins
)

m %>% addPolygons(
  fillColor = ~mypalette(map_data$Alcoholuse_Disorder),
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
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")
) %>% 
  addLegend(
    pal = mypalette, values = mybins, opacity = 0.9,
    title = "Alcohol Use Disorder", position = "bottomleft"
  )
```





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
    map_data %>% filter(Year == 2017) %>% select(Entity, !!sym(input$dataset)) %>% arrange(Entity)
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
      p <- round(country_data[input$dataset], 4)
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
      palette = "YlOrRd", domain = datasetInput()[, 2], bins = bins
    )
    
    labels_value <- labels()
    
    print(datasetInput()[, 2])
    print(datasetInput())
    print(countries$name)
    
    m <- leaflet(countries) %>%
      setView(-96, 37.8, 4) %>%
      addTiles("MapBox", options = tileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    m %>% addPolygons(
      fillColor = ~mypalette(as.numeric(datasetInput()[, 2])),
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


"Alcohol Use Disorder" = "Alcoholuse_Disorder", 
"Anxiety Disorder" = "Anxiety_Disorder", 
"Bipolar Disorder" = "Bipolar_Disorder",
"Depression", 
"Drug Use Disorder" = "Druguse_Disorder",
"Eating Disorder" = "Eating_Disorder",
"Schizophrenia"


countries_new <- st_read("https://raw.githubusercontent.com/lvictory/maps-data/master/world.geo.json")
countries_new <- countries_new[order(countries_new$name), ]
countries_new
# Write the sorted GeoJSON to a new file
st_write(countries_new, "/Users/aliciaho/Documents/GitHub/aliiciaho.github.io/sorted_countries.geojson", driver = "GeoJSON")

x<- c(1, 2, 3, 4, 5)
y <- c(3, 4, 1, 5)
y[match(x, y)]

matched_dataset1 %>% arrange(desc(Schizophrenia....
                                  )) %>% 
  filter(Year == 2017) %>% slice(1:3)
