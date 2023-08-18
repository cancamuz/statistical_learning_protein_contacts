library(caret)
library(MASS) #package for lda function
library(xtable)


index <- createDataPartition(data_selected$Interaction, p=0.8, list = FALSE)
train <- data_selected[index, ]
test <- data_selected[-index, ]

lda_full <- 
prediction <- predict(lda_full, newdata=train)
tab_lda <- table(prediction, test$Interaction)
#obtain the accuracy
round((sum(diag(tab))/sum(tab))*100,2)
library(xtable)
print(xtable(tab_lda, type = "latex"), file = "cf_full_lda.tex")

#Cross validation on a full model
fit.control <- trainControl(method = 'cv', number = 10)
set.seed(123)
fit <- train(Interaction~. , data=data_selected, method='LDA', trControl = fit.control,
             trace=FALSE )
#to see the accuracy of 10 fold cross validation
print(fit)

################################################################################################
#Stepwise selection:

slda <- train(Interaction ~ ., data = data_selected,
              method = "stepLDA",
              trControl = trainControl(method = "cv"))

lda_selected <- lda(Interaction~., data=train)

  
prediction <- predict(lda_selected, newdata=test)
tab_lda <- table(prediction$class, test$Interaction)
#obtain the accuracy
round((sum(diag(tab_lda))/sum(tab_lda))*100,2)
library(xtable)
print(xtable(tab_lda, type = "latex"), file = "cf_full_lda.tex")

#########10-k-fold-features-selection#########
set.seed(123)
cv.error.10 <- rep(0,10)
for (i in 1:10) {
  index <- createDataPartition(data_selected$Interaction, p=0.8, list = FALSE)
  test1 <- data_selected[-index, ]
  prediction <- predict(lda_selected, newdata=test1)
  tab <- table(prediction$class, test1$Interaction)
  cv.error.10[i] <- round((sum(diag(tab))/sum(tab))*100,2)
}
mean(cv.error.10)

#save the confusion matrix
print(xtable(tab, type = "latex"), file = "cf_nbc.tex")

