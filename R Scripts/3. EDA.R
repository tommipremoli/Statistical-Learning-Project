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
library(viridis)

mental_health <- read.xlsx('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/mental_health.xlsx')

# Boxplot
vars <- c("depressive_dis", "anxiety_dis", "bipolar_dis", "drug_dis", "eating_dis", "GDP_per_capita", "health_exp", "unemployment", "suicide_rate", "life_exp", "urban", "internet", "alcohol", "obesity", "phones", "literacy", "birthrate")

mental_health_long <- mental_health %>%
  select(vars) %>%
  scale() %>%
  as.data.frame() %>%
  mutate(row = 1:n()) %>%
  pivot_longer(cols = -row, names_to = "variable", values_to = "value") %>%
  mutate(variable = rep(vars, each = nrow(mental_health)))

ggplot(mental_health_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(x = "Variables", y = "Values", title = "Distribution of mental health variables and socio-economic factors with boxplots") +
  scale_color_viridis(option = "D") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histogram for depressive_dis
mental_healthtop <- head(mental_health[order(-mental_health$depressive_dis), ], 15)

mental_health_topsubset <- mental_healthtop[order(-mental_healthtop$depressive_dis), ]

mental_health_topsubset$country <- factor(mental_health_topsubset$country, levels = mental_health_topsubset$country)

ggplot(data = mental_health_topsubset, aes(x = country, y = depressive_dis)) +
  geom_bar(stat = "identity", aes(fill = depressive_dis), color = "black") +
  labs(title = "Distribution of the 15 countries with the highest depressive_dis values", x = "Country", y = "Depressive Dis") +
  scale_fill_gradient(low = "#ffbfaa", high = "#d11507") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

mental_healthbottom <- head(mental_health[order(mental_health$depressive_dis), ], 15)
mental_health_bottomsubset <- mental_healthbottom[order(mental_healthtop$depressive_dis), ]

ggplot(data = mental_health_bottomsubset, aes(x = reorder(country, -depressive_dis), y = depressive_dis)) +
  geom_bar(stat = "identity", aes(fill = depressive_dis), color = "black") +
  labs(title = "Distribution of the 15 countries with the lowest depressive_dis values", x = "Country", y = "Depressive Dis") +
  scale_fill_gradient(low = "#003399", high = "#b0c4de") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# Heatmap with correlation matrix
order_vars <- c("anxiety_dis", "bipolar_dis", "drug_dis", "eating_dis", "GDP_per_capita", "health_exp", "income", "unemployment", "suicide_rate", "depressive_dis", "life_exp", "urban", "internet", "alcohol", "obesity", "phones", "literacy", "birthrate")

mental_health$income <- factor(mental_health$income, ordered = TRUE, levels = c("Low", "Middle", "High"))
mental_health$income <- as.numeric(mental_health$income)
selected_variables <- c("depressive_dis", "anxiety_dis", "bipolar_dis", "drug_dis", "eating_dis", "GDP_per_capita", "health_exp", "unemployment", "suicide_rate", "life_exp", "urban", "internet", "alcohol", "obesity", "phones", "literacy", "birthrate")
mental_health_selected <- mental_health[, selected_variables]

correlation_matrix <- cor(mental_health_selected)
correlation_df <- as.data.frame(correlation_matrix)
correlation_df$Var1 <- rownames(correlation_matrix)
correlation_df <- melt(correlation_df, id.vars = "Var1")
correlation_df$Var1 <- factor(correlation_df$Var1, levels = order_vars)
correlation_df$variable <- factor(correlation_df$variable, levels = rev(order_vars))

ggplot(data = correlation_df, aes(x = Var1, y = variable, fill = value, label = round(value, 2))) +
  geom_tile(color = "white") +
  geom_text(size = 3.5, color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Variable Correlation Heatmap") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(angle = 0, hjust = 1)) +
  coord_fixed()





