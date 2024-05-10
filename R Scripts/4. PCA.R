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

mental_health <- read.xlsx('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/mental_health.xlsx')

# Data normalization
num_mental_health <- mental_health[, c(5:11, 13:23)]

std_mental_health <- scale(num_mental_health)
head(std_mental_health)

# Correlation matrix
corr_matrix <- cor(std_mental_health)
ggcorrplot(corr_matrix)

data.pca <- princomp(std_mental_health)
summary(data.pca)

data.pca$loadings[, 1:2]

# Scree plot
fviz_eig(data.pca, addlabels = TRUE)

# Scree plot with eigenvalues
eigenvalues <- data.pca$sdev^2
plot(eigenvalues, type = "b",
     xlab = "Principal Component",
     ylab = "Eigenvalue")

abline(v = 2, col = "red")

# Loading matrix
data.pca$loadings[, 1:2]

# Biplot
fviz_pca_var(data.pca, col.var = "black") + theme_fivethirtyeight()
rownames(std_mental_health) <- mental_health$ISO3
pca <- princomp(std_mental_health) #cor = T)
rownames(std_mental_health) <- mental_health$country
pca <- princomp(std_mental_health)
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "red",
                col.ind = "#36454F",
                labelsize = 3)
