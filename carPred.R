# Classification
read.csv("C:\\Users\\SAMEER\\OneDrive\\Desktop\\Programming\\R\\Projects\\Car purchased prediction\\car_data.csv") -> car_purchase
car_purchase # see data
# removing first column
library(dplyr) # used for data manipulation
car_purchase %>% select(-1) -> car_purchase
car_purchase

# splitting data
library(caTools)
sample.split(car_purchase$Purchased,SplitRatio = 0.65)->split_values
subset(car_purchase,split_values==T)->train_set
subset(car_purchase,split_values==F)->test_set

# Loading RPART
library(rpart)

# Building classification model
rpart(Purchased~.,data = train_set)->mod_class
# Assuming mod_class is your trained "rpart" model, and test_set is your test data
predict(mod_class, test_set, type = "class")


predict(mod_class,test_set,type = "class")->result_class
table(test_set$Purchased,result_class)

library(ggplot2)
car_purchase
??car_purchase

library(caret)
library(randomForest)
library(xgboost)
library(e1071)
library(glmnet)

#--------------------------------------------------------------------------------------------------------------------------------------
# Regression
read.csv("C:\\Users\\SAMEER\\OneDrive\\Desktop\\Programming\\R\\Projects\\Diamonds\\diamonds.csv") -> diamonds
diamonds

library(caTools)
sample.split(diamonds$price,SplitRatio = 0.65)->split_values
susubset(diamonds,split_values==T)->train_regbset(diamonds,split_values==F)->test_reg
