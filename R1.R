# Classification
read.csv("C:\\Users\\SAMEER\\OneDrive\\Desktop\\Programming\\R\\Projects\\Car purchased prediction\\car_data.csv") -> car_purchase
car_purchase # see data
# removing first column
library(dplyr) # used for data manipulation
car_purchase %>% select(-1) -> car_purchase
car_purchase

cat('\014')
