# Load necessary libraries
library(plumber)
library(caret)

# Load the saved model
load("regressor_model.RData")

#* @apiTitle Salary Prediction API

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Predict salary based on input data
#* @param Age Age of the person
#* @param Country Country code (1=France, 2=Spain, 3=Germany)
#* @param Purchased Whether the product was purchased (0=No, 1=Yes)
#* @post /predict
function(Age, Country, Purchased) {
  # Create a data frame with the input values
  new_data <- data.frame(
    Age = as.numeric(Age),
    Country = factor(as.numeric(Country), levels = c(1, 2, 3)),
    Purchased = factor(as.numeric(Purchased), levels = c(0, 1))
  )
  
  # Feature Scaling - using the same scaling as in training
  new_data[, 1:2] <- scale(new_data[, 1:2], center = attr(training_set[, 2:3], "scaled:center"), 
                           scale = attr(training_set[, 2:3], "scaled:scale"))
  
  # Make prediction
  prediction <- predict(regressor, newdata = new_data)
  
  # Return prediction
  list(predicted_salary = prediction)
}
