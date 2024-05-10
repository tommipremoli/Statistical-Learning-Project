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
library(reshape2)
library(tidyr)
library(plotly)
library(ggthemes)
library(NbClust)
library("viridis")
library(RColorBrewer)
library(ggforce)
library(gridExtra)

mental_health <- read.xlsx('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/mental_health.xlsx')
mental_health <- mental_health[, -c(1, 4)]
mental_health$income <- factor(mental_health$income, levels = c("Low", "Middle", "High"), labels = c(1, 2, 3))
mental_health$income <- as.numeric(mental_health$income)

scaled.mental_health <- dist(scale(mental_health[, -c(1, 2)]))

agglo <- function(hc){
  data.frame(row.names=paste0("Cluster",seq_along(hc$height)),
             height=hc$height,
             components=ifelse(hc$merge<0, 
                               hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge)),
             stringsAsFactors=FALSE)}

h_average <- hclust(scaled.mental_health, method = "average")
h_average$labels <- mental_health$country

palette_colors <- viridis(5)

fviz_dend(h_average, k = 5, cex = 0.5, k_colors = palette_colors,
          color_labels_by_k = TRUE, rect = TRUE, show_labels = TRUE, main = "Hierarchical Cluster with Average Linkage")

h_complete <- hclust(scaled.mental_health, method="complete")
h_complete$labels <- mental_health$country

fviz_dend(h_complete, k = 5, cex = 0.5, k_colors = palette_colors,
          color_labels_by_k = TRUE, rect = TRUE, show_labels = TRUE, main = "Hierarchical Cluster with Complete Linkage")

h_ward <- hclust(scaled.mental_health, method="ward.D2")
h_ward$labels <- mental_health$country

fviz_dend(h_ward, k = 5, cex = 0.5, k_colors = palette_colors,
          color_labels_by_k = T, rect = T, show_labels = T, main = "Hierarchical  Cluster with Ward Linkage")

ward <- cutree(h_ward, k = 5)

table(ward)

hwardcluster <- cutree(h_ward, k = 5)
hwardcluster
plot(mental_health[, -c(1, 2)], col = palette_colors, main = "Ward Linkage")

means <- aggregate(mental_health[, -1], list(hwardcluster), mean)
means 

mental_health$cluster <- hwardcluster

country_map <- joinCountryData2Map(mental_health,
                                   joinCode = "ISO3",
                                   nameJoinColumn = "ISO3")

map1 <- mapCountryData(country_map, nameColumnToPlot = "cluster",
                       catMethod = "categorical",
                       mapRegion = "world",
                       missingCountryCol = gray(.9),
                       colourPalette = palette_colors,
                       mapTitle = "World Map with Hierarchical Cluster",
                       borderCol = "white")

# Boxplots
selected_vars <- c("depressive_dis", "unemployment", "anxiety_dis", "obesity", "eating_dis", "suicide_rate")
mental_health_subset <- mental_health[, c("cluster", selected_vars)]


melted_data <- melt(mental_health_subset, id.vars = "cluster")

ggplot(melted_data, aes(x = variable, y = value, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = "Boxplot for Selected Variables by Cluster", x = "Variable", y = "Value", fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_manual(values = palette_colors)

