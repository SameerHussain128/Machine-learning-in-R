# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(summarytools)
library(caret)


# Step 1: Loading the Data
data <- read.csv("D:/Data Science 6pm/ML in R/Salary prediction project in R/Data.csv")

# Step 2: Impute missing values in the Age column
# Impute missing values in the Age column
data$Age <- ifelse(is.na(data$Age),
                   ave(data$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                   data$Age)

data$Salary <- ifelse(is.na(data$Salary),
                   ave(data$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                   data$Salary)

# Step 3: Encode categorical values
data$Country = factor(data$Country,
                      levels = c('France','Spain','Germany'),
                      labels = c(1,2,3))

data$Purchased = factor(data$Purchased,
                      levels = c('No','Yes'),
                      labels = c(0,1))


# Step 4: Split the dataset into training and testing sets
# Install and load caTools package
if(!require(caTools)) install.packages('caTools')
library(caTools)
                
# Set seed for reproducibility
set.seed(123)

# Split the dataset
split = sample.split(data$Purchased, SplitRatio = 0.8)
training_set = subset(data, split == TRUE)
testing_set = subset(data, split == FALSE)

# Feature Scaling - standard deviation, range 0-1
training_set[,2:3] = scale(training_set[,2:3])
testing_set[,2:3] = scale(testing_set[,2:3])

# Step 6: Fit the linear regression model
# Include the target variable in the formula
regressor <- lm(Salary ~ ., data = training_set)

# Step 7: Make predictions on the testing set
y_pred <- predict(regressor, newdata = testing_set)

# Step 7: Evaluate the model's performance
# Print the summary of the model
summary(regressor)

# Calculate and print the Mean Squared Error (MSE)
mse <- mean((testing_set$Salary - y_pred)^2)
print(paste("Mean Squared Error:", mse))

