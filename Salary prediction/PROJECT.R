# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(summarytools)
library(caret)


# Step 1: Loading the Data
data <- read.csv("D:/Data Science 6pm/ML in R/Salary prediction project in R/Data.csv")

# Step 2: Exploring and Understanding the Data
# Display the structure of the dataset
str(data)

# Display summary statistics
summary(data)

# Display the first few rows of the dataset
head(data)

# Display more detailed summary statistics
print(dfSummary(data), method = 'render')

# Step 3: Data Cleaning
# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

# Example of handling missing values (e.g., removing rows with missing values)
data_clean <- data %>%
  drop_na()

# Handling missing values by replacing them with the mean of the respective columns
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

print(data)

# Verify if missing values are filled
missing_values_after_cleaning <- sapply(data, function(x) sum(is.na(x)))
print(missing_values_after_cleaning)

# Step 4: Data Analysis
# Example analysis: Summary statistics for a specific column (replace 'column_name' with actual column name)
summary(data$column_name)

# Example analysis: Grouped summary statistics
grouped_summary <- data_clean %>%
  group_by(column_name) %>%
  summarize(
    count = n(),
    mean_value = mean(target_column, na.rm = TRUE),
    sd_value = sd(target_column, na.rm = TRUE)
  )
print(grouped_summary)

# Age distribution by country
ggplot(data, aes(x = Country, y = Age, fill = Country)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Country", x = "Country", y = "Age")

# Salary distribution by Purchased
ggplot(data, aes(x = Purchased, y = Salary, fill = Purchased)) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Purchase Decision", x = "Purchased", y = "Salary")

# Scatter plot of Age vs Salary, colored by Purchase decision
ggplot(data, aes(x = Age, y = Salary, color = Purchased)) +
  geom_point() +
  labs(title = "Age vs Salary, colored by Purchase Decision", x = "Age", y = "Salary")


# Define a mapping for countries to numeric codes
data = factor(data,
              levels = c('France','Spain','Germany'),
              labels = c(1,2,3))

data = factor(data,
              levels = c('No','Yes'),
              labels = c(0,1))

# Check the encoded data
print(data)

# Split data into 70% training and 30% testing
train_index <- createDataPartition(data$country, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
