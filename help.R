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
 
 
matched_dataset1 %>% filter(Year == c(1990, 2017)) %>%
  select(Entity, Code, Year, Depression....)
