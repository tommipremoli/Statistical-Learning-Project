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

# Import datasets
depression <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Depression.xlsx')
anxiety_dis <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Anxiety Disorder.xlsx')
bipolar_dis <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Bipolar Disorder.xlsx')
drug_users <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Drug Use Disorders.xlsx')
eating_dis <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Eating Disorder.xlsx')
GDP_per_capita <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/GDP per Capita.xlsx')
health_exp <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Health Expenditure.xlsx')
income <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Income.xlsx')
unemployment <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Unemployment.xlsx')
population <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Population.xlsx')
suicide_rate <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Suicide Rate.xlsx')
life_expectancy <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Life Expectancy.xls')
urban <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Urban.xls')
internet <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Internet Access.xls')
alcohol <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Alcohol disorder.xlsx')
education <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Education.xlsx')
obesity <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Obesity.xlsx')
phones <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Phones.xlsx')
literacy <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Literacy.xlsx')
birthrate <- read_excel('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/Birthrate.xlsx')


# Creation of the drug_dis ratio
drug_users <- merge(drug_users, population[, c("ISO3", "population")], by = "ISO3", all.x = TRUE)
drug_users <- na.omit(drug_users)
drug_users$drug_dis <- drug_users$drug_users / drug_users$population * 100


# Creation of the final dataset
final_table <- merge(depression, anxiety_dis, by = "ISO3", all = TRUE)
final_table <- merge(final_table, bipolar_dis, by = "ISO3", all = TRUE)
final_table <- merge(final_table, drug_users, by = "ISO3", all = TRUE)
final_table <- merge(final_table, eating_dis, by = "ISO3", all = TRUE)
final_table <- merge(final_table, GDP_per_capita, by = "ISO3", all = TRUE)
final_table <- merge(final_table, health_exp, by = "ISO3", all = TRUE)
final_table <- merge(final_table, income, by = "ISO3", all = TRUE)
final_table <- merge(final_table, unemployment, by = "ISO3", all = TRUE)
final_table <- merge(final_table, suicide_rate, by = "ISO3", all = TRUE)
final_table <- merge(final_table, life_expectancy, by = "ISO3", all = TRUE)
final_table <- merge(final_table, urban, by = "ISO3", all = TRUE)
final_table <- merge(final_table, internet, by = "ISO3", all = TRUE)
final_table <- merge(final_table, alcohol, by = "ISO3", all = TRUE)
final_table <- merge(final_table, education, by = "ISO3", all = TRUE)
final_table <- merge(final_table, obesity, by = "ISO3", all = TRUE)
final_table <- merge(final_table, phones, by = "ISO3", all = TRUE)
final_table <- merge(final_table, literacy, by = "ISO3", all = TRUE)
final_table <- merge(final_table, birthrate, by = "ISO3", all = TRUE)

mental_health <- final_table[, c("country.x", "ISO3", "Year.x", "depressive_dis", "anxiety_dis", "bipolar_dis", "drug_dis", "eating_dis", "GDP_per_capita", "health_exp", "income", "unemployment", "suicide_rate", "life_exp", "urban", "internet", "alcohol", "education", "obesity", "phones", "literacy", "birthrate")]
mental_health <- rename(mental_health, year = Year.x)
mental_health <- rename(mental_health, country = country.x)
mental_health <- na.omit(mental_health)

# Creation of the categorical variable "income"
mental_health$income <- recode(mental_health$income,
                               "Low" = "Low",
                               "Lower-middle" = "Middle",
                               "Upper-middle" = "Middle",
                               "High" = "High")
mental_health$income <- factor(mental_health$income)

# Extract file
file_path <- "/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Individual Project/Datasets/mental_health.xlsx"
write.xlsx(mental_health, file_path, rowNames = TRUE)