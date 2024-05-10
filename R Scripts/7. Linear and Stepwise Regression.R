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

states_to_remove <- c("Mongolia", "El Salvador", "Portugal", "New Zealand", "Niger", "Australia",
                      "Luxembourg", "Nigeria", "Tonga", "Samoa", "Lesotho", "USA", "Afghanistan")
mental_health <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/mental_health.xlsx')
mental_health <- mental_health[!mental_health$country %in% states_to_remove, ]
mental_health$income <- as.factor(mental_health$income)


ggqqplot(mental_health$depressive_dis)
ggqqplot(mental_health$anxiety_dis)
ggqqplot(mental_health$bipolar_dis)
ggqqplot(mental_health$drug_dis)
ggqqplot(mental_health$eating_dis)
ggqqplot(mental_health$GDP_per_capita)
ggqqplot(mental_health$health_exp)
ggqqplot(mental_health$unemployment)
ggqqplot(mental_health$suicide_rate)
ggqqplot(mental_health$life_exp)
ggqqplot(mental_health$urban)
ggqqplot(mental_health$internet)
ggqqplot(mental_health$alcohol)
ggqqplot(mental_health$education)
ggqqplot(mental_health$obesity)
ggqqplot(mental_health$phones)
ggqqplot(mental_health$literacy)
ggqqplot(mental_health$birthrate)

ggdensity(mental_health, x = "depressive_dis", fill = "lightgray", title = "depressive_dis") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "anxiety_dis", fill = "lightgray", title = "anxiety_dis") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "bipolar_dis", fill = "lightgray", title = "bipolar_dis") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "drug_dis", fill = "lightgray", title = "drug_dis") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "eating_dis", fill = "lightgray", title = "eating_dis") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "GDP_per_capita", fill = "lightgray", title = "GDP_per_capita") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "health_exp", fill = "lightgray", title = "health_exp") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "unemployment", fill = "lightgray", title = "unemployment") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "suicide_rate", fill = "lightgray", title = "suicide_rate") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "life_exp", fill = "lightgray", title = "life_exp") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "urban", fill = "lightgray", title = "urban") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "internet", fill = "lightgray", title = "internet") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "alcohol", fill = "lightgray", title = "alcohol") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "education", fill = "lightgray", title = "education") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "phones", fill = "lightgray", title = "phones") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "literacy", fill = "lightgray", title = "literacy") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(mental_health, x = "birthrate", fill = "lightgray", title = "birthrate") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

mental_health$logeating=log(mental_health$eating_dis)
ggqqplot(mental_health$logeating)
ggdensity(mental_health, x = "logeating", fill = "lightgray", title = "logeating") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
mental_health$loggdp=log(mental_health$GDP_per_capita)
ggqqplot(mental_health$loggdp)
ggdensity(mental_health, x = "loggdp", fill = "lightgray", title = "loggdp") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
mental_health$logunemployment=log(mental_health$unemployment)
ggqqplot(mental_health$logunemployment)
ggdensity(mental_health, x = "logunemployment", fill = "lightgray", title = "logunemployment") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
mental_health$logsuicide_rate=log(mental_health$suicide_rate)
ggqqplot(mental_health$logsuicide_rate)
ggdensity(mental_health, x = "logsuicide_rate", fill = "lightgray", title = "logsuicide_rate") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
mental_health$logliteracy=log(mental_health$literacy)
ggqqplot(mental_health$logliteracy)
ggdensity(mental_health, x = "logliteracy", fill = "lightgray", title = "logliteracy") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

# Multicollinearity
model <- lm(depressive_dis ~ anxiety_dis + bipolar_dis + drug_dis + log(eating_dis) + log(GDP_per_capita) + health_exp + income + log(unemployment) + log(suicide_rate) + life_exp + urban + internet + alcohol + education + obesity + phones + birthrate, data = mental_health)
summary(model)

vif(model)
sqrt(vif(model)) > 2 

model <- lm(depressive_dis ~ anxiety_dis + bipolar_dis + drug_dis+ health_exp + income + log(unemployment) + log(suicide_rate) + life_exp + urban + alcohol + education + obesity + phones + birthrate, data = mental_health)
summary(model)

summary(lm(depressive_dis ~ anxiety_dis + bipolar_dis + drug_dis+ health_exp + income + log(unemployment) + log(suicide_rate) + life_exp + urban + alcohol + education + phones + birthrate, data = mental_health))
summary(lm(depressive_dis ~ anxiety_dis + bipolar_dis + drug_dis+ health_exp + income + log(unemployment) + log(suicide_rate) + life_exp + alcohol + education + phones + birthrate, data = mental_health))
summary(lm(depressive_dis ~ anxiety_dis + bipolar_dis + drug_dis+ health_exp + income + log(unemployment) + log(suicide_rate) + life_exp + alcohol + phones + birthrate, data = mental_health))
summary(lm(depressive_dis ~ anxiety_dis + bipolar_dis + drug_dis+ health_exp + log(unemployment) + log(suicide_rate) + life_exp + alcohol + phones + birthrate, data = mental_health))
summary(lm(depressive_dis ~ anxiety_dis + bipolar_dis + drug_dis + log(unemployment) + log(suicide_rate) + life_exp + alcohol + phones + birthrate, data = mental_health))
summary(lm(depressive_dis ~ anxiety_dis + bipolar_dis + log(unemployment) + log(suicide_rate) + life_exp + alcohol + phones + birthrate, data = mental_health))
summary(lm(depressive_dis ~ anxiety_dis + bipolar_dis + log(unemployment) + log(suicide_rate) + alcohol + phones + birthrate, data = mental_health))
summary(lm(depressive_dis ~ anxiety_dis + bipolar_dis + log(unemployment) + log(suicide_rate) + alcohol + birthrate, data = mental_health))

fmodel <- lm(depressive_dis ~ anxiety_dis + bipolar_dis + log(unemployment) + log(suicide_rate) + alcohol + birthrate, data = mental_health)
predicted_values <- predict(fmodel)
data <- data.frame(Actual = mental_health$depressive_dis, Predicted = predicted_values)

ggplot(data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#002D62") +  # Imposta il colore dei punti
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "Actual vs Predicted Depressive Dis", x = "Actual Score", y = "Predicted Score") +  
  theme(panel.background = element_rect(fill = "white"))

residuals <- resid(model)

ggplot() +
  geom_histogram(aes(x = residuals), fill = "#7393B3", color = "black", bins = 20) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Stepwise regression
full.model <- lm(depressive_dis ~ anxiety_dis + bipolar_dis + drug_dis + health_exp + income + log(unemployment) + log(suicide_rate) + life_exp + urban + alcohol + education + phones + birthrate, data = mental_health)
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

models <- regsubsets(depressive_dis ~ anxiety_dis + bipolar_dis + drug_dis + health_exp + income + log(unemployment) + log(suicide_rate) + life_exp + urban + alcohol + education + phones + birthrate, data = mental_health, nvmax = 5,
                     method = "seqrep")
summary(models)



