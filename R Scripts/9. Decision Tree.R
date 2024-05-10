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
library(car)
library(MASS)
library(leaps)
library(glmnet)
library("rpart")
library("rpart.plot")
library("party")
library("partykit")
library(randomForest)


states_to_remove <- c("Mongolia", "El Salvador", "Portugal", "New Zealand", "Niger", "Australia", "Luxembourg", "Nigeria", "Tonga", "Samoa", "Lesotho", "USA", "Afghanistan")
mental_health <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/mental_health.xlsx')
mental_health <- mental_health[!mental_health$country %in% states_to_remove, ]
mental_health$income <- as.factor(mental_health$income)

# Split data
set.seed(123)
ind <- sample(2, nrow(mental_health), replace = T, prob = c(0.70, 0.30))
train <- mental_health[ind == 1,]
test <- mental_health[ind == 2,]
# Regression tree
tree <- rpart(depressive_dis ~ anxiety_dis + bipolar_dis + drug_dis + health_exp + income + log(unemployment) + log(suicide_rate) + life_exp + urban + alcohol + education + phones + birthrate, data = train)
rpart.plot(tree)

printcp(tree)
rpart.rules(tree)

plotcp(tree)

p <- predict(tree, train)

# Root Mean Square Error
sqrt(mean((train$depressive_dis-p)^2))

# R-Squared
(cor(train$depressive_dis,p))^2

# Mean square error
mse <- mean((train$depressive_dis - p)^2)
mse
