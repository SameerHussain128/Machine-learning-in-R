## Step 1 : Load and Explore the Data

# Load the dataset
data <- read.csv('D:/Data Science 6pm/ML in R/Data.csv')

# Load necessary libraries
library(tidyverse)

# View the first few rows of the dataset
head(data)

# Get a summary of the dataset
summary(data)

# Check the structure of the dataset
str(data)

## Step 2 : Data Preprocessing

# Check for missing values
colSums(is.na(data))

# Handle missing values (example: replace with mean for numerical, mode for categorical)
data <- data %>%
  mutate_if(is.numeric, ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  mutate_if(is.character, ~ ifelse(is.na(.), Mode(.), .))

head(data)

# Convert categorical variables to factors
data <- data %>%
  mutate_if(is.character, as.factor)


# Function to calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


head(data)


# Load visualization libraries
library(ggplot2)

# Pairplot for numerical variables
pairs(data)


# Correlation matrix for numerical variables
cor_data <- data %>%
  select_if(is.numeric) %>%
  cor()


# Heatmap of the correlation matrix
library(reshape2)
melted_cor <- melt(cor_data)
ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))

# Boxplots for categorical variables
ggplot(data, aes(x = categorical_variable, y = numerical_variable)) +
  geom_boxplot() +
  theme_minimal()

# Check the levels of the target variable
levels(train_data$Purchased)


train_data$Purchased <- as.factor(train_data$Purchased)
test_data$Purchased <- as.factor(test_data$Purchased)

head(data)

# Logistic Regression Model
model <- glm(Purchased ~ ., data = train_data, family = binomial)
summary(model)

# Make predictions on the test set
predictions <- predict(model, test_data, type = "response")

# Convert probabilities to class labels
predicted_classes <- ifelse(predictions > 0.5, "Yes", "No")
predicted_classes <- factor(predicted_classes, levels = levels(test_data$Purchased))

# Confusion Matrix
confusion_matrix <- table(predicted_classes, test_data$Purchased)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(predicted_classes == test_data$Purchased) / nrow(test_data)
cat("Accuracy:", accuracy, "\n")

install.packages("randomForest")
library(randomForest)
# Apply SMOTE
train_data_balanced <- SMOTE(Purchased ~ ., train_data, perc.over = 200, perc.under = 200)
table(train_data_balanced$Purchased)


# Model Building: Random Forest (Classification)
rf_model <- randomForest(Purchased ~ ., data = train_data)
rf_predictions <- predict(rf_model, test_data)
rf_confusion_matrix <- table(rf_predictions, test_data$Purchased)
print(rf_confusion_matrix)
rf_accuracy <- sum(rf_predictions == test_data$Purchased) / nrow(test_data)
cat("Random Forest Accuracy:", rf_accuracy, "\n")
