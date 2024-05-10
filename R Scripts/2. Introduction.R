library(glmnet)
library(dplyr)
library(tidyverse)
library(readr)
library(ggcorrplot)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(formattable)
library(cluster)
library(countrycode)
library(rnaturalearth)
library(rworldmap)
library(ggpubr)
library(readxl)
library(openxlsx)
library(caret)
library(sf)
library(maps)

mental_health <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/mental_health.xlsx')

mental_health$country[mental_health$country == "United States"] <- "USA"

world_map <- map_data("world")
world_map <- subset(world_map, region != "Antarctica")
mental_health <- mental_health %>% rename(region = country)

ggplot(mental_health) +
  geom_map(
    dat = world_map, map = world_map, aes(map_id = region),
    fill = "white", color = "#002244", size = 0.25
  ) +
  geom_map(map = world_map, aes(map_id = region, fill = depressive_dis), linewidth = 0.25) +
  scale_fill_gradient(low = "#B0C4DE", high = "#00008B", name = "Depressed population") +
  expand_limits(x = world_map$long, y = world_map$lat)

