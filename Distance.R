# Load in libraries 
library(tidyverse)
library(readxl)
library(lubridate)
library(corrplot)
library(ggplot2)
library(GGally)

# Data Import and Cleaning ------------------------------------------------

# Import Apple Health data
Distance <- read_xlsx("Distance.xlsx")

# Make the raw date & time data into just dates
Distance <- Distance %>% 
  mutate(
    Start_Date = date(startDate),
    End_Date = date(endDate)
  )

# Filter out only apple watch data - removing data captured by iPhone
Distance <- Distance %>% 
  filter(sourceName == "Jared‚Äôs Apple¬†Watch")

# Aggregate the data by date and sum distances 
Distance_By_Day <- Distance %>% 
  group_by(Start_Date) %>% 
  summarise(
    sum(value)
  )

# rename the column
Distance_By_Day <- Distance_By_Day %>% 
  rename(Date = Start_Date,
         Miles = 'sum(value)')

# add day of week column
Distance_By_Day <- Distance_By_Day %>% 
  mutate(
    DOW = weekdays(Date, abbreviate = TRUE)
  )

# Average by DOW
Avg_DOW <- Distance_By_Day %>% 
  group_by(DOW) %>% 
  summarise(Miles = mean(Miles))

# Load in weather data
weather <- read_csv("weather.csv")

# Select just the date and precipitation columns
weather <- weather %>% 
  select(DATE, PRCP)

# rename
weather <- weather %>% 
  rename(Date = DATE,
         Precipitation = PRCP)

# aggregate dates and precip
weather <- weather %>% 
  group_by(Date) %>% 
  summarise(Precipitation = sum(Precipitation))

# Join the two tables by date
Distance_And_Weather <- inner_join(Distance_By_Day, weather, by = "Date")

# Create segments for precipitation in order to better understand
Distance_And_Weather <- Distance_And_Weather %>% 
  mutate(
    Precip_Segment = case_when(
      Precipitation == 0 ~ "No Precipitation",
      Precipitation > 0 & Precipitation <= .5 ~ "Minimal Precipitation",
      Precipitation > .5 & Precipitation <= 1.5 ~ "Moderate Precipitation",
      TRUE ~ as.character("Heavy Precipitation") 
    )
  )

# EDA ---------------------------------------------------------------------

# Build correlation plot
  
Matrix_Distance <- Distance_And_Weather %>% 
  select(Date, Miles, Precipitation) %>% 
  mutate(Date = as.integer(Date),
         Miles = as.integer(Miles)) %>% 
  as.matrix()


New_Cor <- cor(Matrix_Distance) %>% 
  as_tibble(rownames = "var1") %>% 
  pivot_longer(!var1, names_to = "var2", values_to = "corr")

# Use GGally to create pairs
ggpairs(data = Distance_And_Weather, columns = 1:5, title = "Distance & Weather")

# Group by day of week and precipitation segment
Grouped_Weather <- Distance_And_Weather %>% 
  group_by(DOW, Precip_Segment) %>% 
  summarise(Avg_Miles = mean(Miles))

# Plot grouped weather
Grouped_Weather %>% 
  ggplot(aes(DOW, Avg_Miles, fill = Precip_Segment))+
  geom_col()
