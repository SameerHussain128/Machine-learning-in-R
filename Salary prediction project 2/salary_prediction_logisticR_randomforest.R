# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(reshape2)
library(randomForest)
install.packages("DMwR")
library(DMwR)

# Load the dataset
data <- read.csv('D:/Data Science 6pm/ML in R/Data.csv')

# Initial exploration
head(data)
summary(data)
str(data)
colnames(data)  # View column names to identify the target variable

# Data Preprocessing
colSums(is.na(data))
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
data <- data %>%
  mutate_if(is.numeric, ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  mutate_if(is.character, ~ ifelse(is.na(.), Mode(.), .)) %>%
  mutate_if(is.character, as.factor)

# EDA
pairs(data)
cor_data <- data %>%
  select_if(is.numeric) %>%
  cor()
melted_cor <- melt(cor_data)
ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))

# Model Building: Logistic Regression
set.seed(123)
train_indices <- sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Ensure target variable is a factor
train_data$Purchased <- as.factor(train_data$Purchased)
test_data$Purchased <- as.factor(test_data$Purchased)

# Fit logistic regression model
model <- glm(Purchased ~ ., data = train_data, family = binomial)
summary(model)

# Model Evaluation: Logistic Regression
predictions <- predict(model, test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, "Yes", "No")
predicted_classes <- factor(predicted_classes, levels = levels(test_data$Purchased))
confusion_matrix <- table(predicted_classes, test_data$Purchased)
print(confusion_matrix)
accuracy <- sum(predicted_classes == test_data$Purchased) / nrow(test_data)
cat("Accuracy:", accuracy, "\n")

# Check class balance
table(train_data$Purchased)

# Apply SMOTE to balance the classes
train_data_balanced <- SMOTE(Purchased ~ ., train_data, perc.over = 200, perc.under = 200)
table(train_data_balanced$Purchased)

# Model Building: Random Forest (Classification) on balanced data
rf_model <- randomForest(Purchased ~ ., data = train_data_balanced)
rf_predictions <- predict(rf_model, test_data)
rf_confusion_matrix <- table(rf_predictions, test_data$Purchased)
print(rf_confusion_matrix)
rf_accuracy <- sum(rf_predictions == test_data$Purchased) / nrow(test_data)
cat("Random Forest Accuracy:", rf_accuracy, "\n")
