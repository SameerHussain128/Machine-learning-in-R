credit_card <- read.csv('D:\\Data Science 6pm\\Projects\\Imbalance dataset\\creditcard.csv')

summary(credit_card)

table(credit_card$Class)

labels <- c("legit","fraud")
labels <- paste(labels,round(100*prop.table(table(credit_card$Class)), 2))
labels <- paste0(labels, '%')

pie(table(credit_card), labels, col = c("orange", "red"),
    main = "Pie chart of credit card transcation"

    
predictions <- rep.int(0, nrow(credit_card))
predictions <- factor(predictions, levels = c(0,1))
