library(caret)
index <- createDataPartition(data_selected$Interaction, p=0.8, list = FALSE)
train <- data_selected[index, ]
test <- data_selected[-index, ]

#train the full model 
require(nnet)
full_multinom <- multinom(Interaction~., data = train)
prediction <- predict(full_multinom, newdata = test)
tab <- table(prediction, test$Interaction)
#obtain the accuracy
round((sum(diag(tab))/sum(tab))*100,2)
library(xtable)

#Cross validation on a full model
fit.control <- trainControl(method = 'cv', number = 10)
set.seed(123)
fit <- train(Interaction~. , data=data_selected, method='multinom', trControl = fit.control,
             trace=FALSE )
#to see the accuracy of 10 fold cross validation
print(fit)


#Stepwise selection features
library(MASS)
#now apply an hybrid forward and backward features selection (direction = 'both'), see the link below for more info
#http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
step.model <- stepAIC(full_multinom, direction = 'backward', trace = FALSE)

summary(step.model) # this will return the best subset of features obtained with an hybrid forward and backward selection
stepwise_model <- multinom(Interaction ~ s_up + s_down + s_psi + s_a1 + 
           s_a2 + s_a3 + s_a4 + t_up + t_psi + t_a1 + t_a2 + t_a3 + 
           t_a4, data = train)


prediction <- predict(stepwise_model, newdata = test)
tab_stepwise <- table(prediction, test$Interaction)
#obtain the accuracy of the step model
round((sum(diag(tab))/sum(tab))*100,2)
print(xtable(tab_stepwise, type = "latex"), file = "cf_step_multinom.tex")

#Cross validation on the step model, also to asses the differences between the full and stepwise feature selection
fit.control <- trainControl(method = 'cv', number = 10)
set.seed(123)
cv_stepwise <- train(Interaction~. , data=data_selected, method='multinom', trControl = fit.control,
             trace=FALSE )
#to see the accuracy of 10 fold cross validation
print(fit_stepwise)


#Now we try a shrinkage approach on multinomial model:
library(glmnet)

index <- createDataPartition(data_selected$Interaction, p=0.8, list = FALSE)


Y = train$Interaction
X = as.matrix(train[,1:16])

#alpha=0 means that we want a ridge regression, alpha=1 means that we want a lasso regression
multinom_shrinked_r = cv.glmnet(X, Y, family='multinomial', type.measure = 'class', alpha=0) 
plot(multinom_shrinked_r)

