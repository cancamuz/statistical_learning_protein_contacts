library(caret) #library for machine learning operations

#convert Interaction feature to a categorical feature.
data$Interaction <- as.factor(data$Interaction)


#Normalization
normalize<- function(x){
  return((x-min(x)) / (max(x) - min(x)))
}

normalized.data <- as.data.frame(lapply(data[,1:17], normalize)) #didn't work well


#split the data
set.seed(100) # to make reproducible the partition
index <- createDataPartition(data$Interaction, p=0.8, list = FALSE)
train_data = data[index,1:17]
test_data = data[-index,1:17 ]

train_labels <- data[index, 18]
test_labels <- data[-index, 18]

#Defining error metrics to check the error rate and accuracy of the Regression ML algorithms

#1. MEAN ABSOLUTE PERCENTAGE ERROR (MAPE)
MAPE = function(y_actual,y_predict){
  mean(abs((y_actual-y_predict)/y_actual))*100
}

#2. R SQUARE error metric -- Coefficient of Determination
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}


#KNN Model
library(class)
knn.100 <- knn(train = train_data, test = test_data, cl=train_labels, k=100)

#Accuracy
acc.knn100 <- 100*sum(test_labels == knn.100)/NROW(test_labels)
