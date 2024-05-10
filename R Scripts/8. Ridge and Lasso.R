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

states_to_remove <- c("Mongolia", "El Salvador", "Portugal", "New Zealand", "Niger", "Australia", "Luxembourg", "Nigeria", "Tonga", "Samoa", "Lesotho", "USA", "Afghanistan")
mental_health <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/mental_health.xlsx')
mental_health <- mental_health[!mental_health$country %in% states_to_remove, ]
mental_health$income <- as.factor(mental_health$income)

# Lasso regression
set.seed(123)
x=model.matrix(depressive_dis ~ anxiety_dis + bipolar_dis + drug_dis + log(eating_dis) + log(GDP_per_capita) + health_exp + income + log(unemployment) + log(suicide_rate) + life_exp + urban + internet + alcohol + education + obesity + phones + birthrate, data = mental_health) 
y=mental_health$depressive_dis
cv_model <- cv.glmnet(x, y, alpha = 1)
summary(cv_model)
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model) 
best_modell <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_modell)
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)

# Ridge Regression
set.seed(123)
cv_model <- cv.glmnet(x, y, alpha = 0)
summary(cv_model)
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model) 
best_modelr <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_modelr)
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)




pred_lasso <- predict(best_modell, newx = x, s = best_lambda)
mse_lasso <- mean((pred_lasso - y)^2)
mse_lasso

pred_ridge <- predict(best_modelr, newx = x, s = best_lambda)
mse_ridge <- mean((pred_ridge - y)^2)
mse_ridge


# Ridge
num_folds <- 10
ctrl <- trainControl(method = "cv", number = num_folds)
grid <- expand.grid(lambda = seq(0.001, 1, length = 100), alpha = 0)
model <- train(x, y, method = "glmnet", trControl = ctrl, tuneGrid = grid)
predictions <- predict(model, newdata = x)
performance <- postResample(predictions, y)
print(performance)

# Lasso
num_folds <- 10
ctrl <- trainControl(method = "cv", number = num_folds)
grid <- expand.grid(lambda = seq(0.001, 1, length = 100), alpha = 1)
model <- train(x, y, method = "glmnet", trControl = ctrl, tuneGrid = grid)
predictions <- predict(model, newdata = x)
performance <- postResample(predictions, y)
print(performance)
