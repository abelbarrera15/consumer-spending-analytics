library(MASS)
library(ggplot2)

data <- read.csv(file.choose(), header = TRUE) #training sample
#testing <- read.csv(file.choose(), header = TRUE)
#data <- rbind(training, testing)
set.seed(1)
train <- sample(nrow(data), nrow(data)*.80)

training <- data[train,]
testing <- data[-train,]

stat_measures <- function(matrix){
  TN = matrix[1,1]
  FN = matrix[1,2]
  FP = matrix[2,1]
  TP = matrix[2,2]
  
  sensitivity = round(TP/(TP+FN), 4)
  specificity = round(TN/(TN+FP), 4)
  accuracy = round((TN+TP)/(TN+FN+FP+TP), 4)
  #answer = cat(paste("Sensitivity = ", sensitivity, "Specificity = ", specificity, "\n Accuracy = ", accuracy, sep = " "))
  answer2 = cat( "Sensitivity = ", sensitivity, "Specificity = ", specificity, "\n Accuracy = ", accuracy, sep = " ")
  return(answer2)
}

cor(training[,2:25])
#cor(data$saw_checkout, data$sign_in)


x <- which(testing$ordered == "1")

LDA <- lda(ordered ~ basket_icon_click + basket_add_detail + checked_delivery_detail 
           + promo_banner_click + sign_in + saw_checkout + basket_icon_click, training)
LDA
LDA_Predictions <- predict(LDA, testing)
stat_matrix <- table("Prediction" = LDA_Predictions$class, "Order Status" = testing$ordered)
stat_matrix
stat_measures(stat_matrix)

QDA <- qda(ordered ~ checked_delivery_detail +  basket_icon_click + device_mobile + sign_in, training)
QDA
QDA_Predictions <- predict(QDA, testing)
stat_matrix <- table("Prediction" = QDA_Predictions$class, "Order Status" = testing$ordered)
stat_matrix
stat_measures(stat_matrix)


dataset <- data
dataset$total_interactions <- rowSums(dataset[,2:24])
cor(dataset$total_interactions, dataset$ordered)

table <- data.frame()
for(i in 1:max(dataset$total_interactions))
{
  index <- which(dataset$total_interactions == i)
  set <- dataset[index,]
  percent <- round(length(which(set$ordered == 1))/nrow(set), 4) * 100
  table <- rbind(table, c(i, percent, length(index)))
}
colnames(table) <- c("Interactions", "Percent_Ordered", "Number")

ggplot(table, aes(x = Interactions, y = Percent_Ordered)) + 
  geom_point(color = "blue", size = 2) + scale_y_continuous(limits = c(0, 100))

Interaction_sums <- as.data.frame(colSums(dataset[,2:24]))
colnames(Interaction_sums) <- c("Total")
