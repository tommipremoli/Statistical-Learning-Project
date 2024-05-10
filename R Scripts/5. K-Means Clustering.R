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
library(viridis)
library(RColorBrewer)
library(ggforce)

mental_health <- read.xlsx('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/mental_health.xlsx')
mental_health <- mental_health[, -c(1, 2, 3, 4)]
mental_health$income <- factor(mental_health$income, levels = c("Low", "Middle", "High"), labels = c(1, 2, 3))
mental_health$income <- as.numeric(mental_health$income)

scaled.mental_health <- scale(mental_health)


set.seed(123)

# Within Sum of Squares Plot
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1) * sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(scaled.mental_health, nc=10) 

# Silhouette
set.seed(123)
calculate_silhouette_values <- function(scaled.mental_health, num_clusters) {
  set.seed(123)
  kmeans_fit <- kmeans(scaled.mental_health, num_clusters)
  
  silhouette_values <- silhouette(kmeans_fit$cluster, dist(scaled.mental_health))
  
  return(silhouette_values)
}

silhouette_values_2 <- calculate_silhouette_values(scaled.mental_health, 2)
silhouette_values_3 <- calculate_silhouette_values(scaled.mental_health, 3)
silhouette_values_4 <- calculate_silhouette_values(scaled.mental_health, 4)
silhouette_values_5 <- calculate_silhouette_values(scaled.mental_health, 5)
silhouette_values_6 <- calculate_silhouette_values(scaled.mental_health, 6)

set.seed(123)
plot(silhouette_values_2, main = 'Silhouette with 2 clusters')
plot(silhouette_values_3, main = 'Silhouette with 3 clusters', col = viridis(3))
plot(silhouette_values_4, main = 'Silhouette with 4 clusters', col = viridis(4))
plot(silhouette_values_5, main = 'Silhouette with 5 clusters')
plot(silhouette_values_6, main = 'Silhouette with 6 clusters')

# NbClust
nc <- NbClust(scaled.mental_health, min.nc=2,
              max.nc=6, method="kmeans")

# K-Means Clustering
set.seed(123)
kmeans.clus <- kmeans(x = scaled.mental_health, centers = 3)
mental_health$clusters <- kmeans.clus$cluster

aggr <- aggregate(mental_health,
                  list(mental_health$clusters), mean)

cluster_colors <- viridis(3)

colorFunction <- function(clusters) {
  return(cluster_colors[clusters])
}

upper.panel <- function(x, y) {
  points(x, y, pch = 19, col = colorFunction(mental_health$clusters))
  r <- round(cor(x, y), digits = 2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}

pairs(mental_health[,1:10], lower.panel = NULL, upper.panel = upper.panel)

pairs(mental_health[,11:19], lower.panel = NULL, upper.panel = upper.panel)

mental_health <- read.xlsx('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/mental_health.xlsx')
cluster_df <- data.frame(cluster = as.factor(kmeans.clus$cluster), scaled.mental_health)
cluster_df$country <- mental_health$country  
cluster_df$cluster_colors <- colorFunction(cluster_df$cluster)

pca <- prcomp(scaled.mental_health)
PC1 <- pca$x[, 1]
PC2 <- pca$x[, 2]

cluster_df <- cluster_df %>% 
  mutate(PC1 = PC1,
         PC2 = PC2)

ggplot(cluster_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = country), size = 3, colour = "#36454F", max.overlaps = 100) +  # Aggiungi i nomi dei paesi
  scale_color_manual(values = cluster_colors) +  
  labs(title = "Contries clustering") +
  theme_minimal()

cluster_df$ISO3 <- mental_health$ISO3  

country_map <- joinCountryData2Map(cluster_df,
                                   joinCode = "ISO3",
                                   nameJoinColumn = "ISO3")

map1 <- mapCountryData(country_map, nameColumnToPlot = "cluster",
                       catMethod = "categorical",
                       mapRegion = "world",
                       missingCountryCol = gray(.9),
                       colourPalette = viridis(3),
                       mapTitle = "World Map with K-Means Cluster",
                       borderCol = "white")

ggplot(cluster_df, aes(x = depressive_dis, fill = factor(cluster))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot for Depressive Disorders by Cluster", x = "Depressive Disorders", y = "Density", fill = "Cluster") +
  scale_fill_manual(values = cluster_colors) +
  theme_minimal()

ggplot(cluster_df, aes(x = anxiety_dis, fill = factor(cluster))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot for Anxiety Disorders by Cluster", x = "Depressive Disorders", y = "Density", fill = "Cluster") +
  scale_fill_manual(values = cluster_colors) +
  theme_minimal()

ggplot(cluster_df, aes(x = suicide_rate, fill = factor(cluster))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot for Suicide Rate by Cluster", x = "Depressive Disorders", y = "Density", fill = "Cluster") +
  scale_fill_manual(values = cluster_colors) +
  theme_minimal()

ggplot(cluster_df, aes(x = birthrate, fill = factor(cluster))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot for Birthrate by Cluster", x = "Depressive Disorders", y = "Density", fill = "Cluster") +
  scale_fill_manual(values = cluster_colors) +
  theme_minimal()

ggplot(cluster_df, aes(x = life_exp, fill = factor(cluster))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot for Life Expectancy by Cluster", x = "Depressive Disorders", y = "Density", fill = "Cluster") +
  scale_fill_manual(values = cluster_colors) +
  theme_minimal()

ggplot(cluster_df, aes(x = health_exp, fill = factor(cluster))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot for Health Expenditure by Cluster", x = "Depressive Disorders", y = "Density", fill = "Cluster") +
  scale_fill_manual(values = cluster_colors) +
  theme_minimal()
