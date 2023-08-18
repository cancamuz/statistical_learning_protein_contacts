####Naive bayes classifier###############
#made a model on data and see its accuracy 
library(caret)
library(naivebayes)
library(xtable)

data1 <- data[, c( 's_up', 's_down', 's_phi', 's_psi', 's_a1', 's_a2', 's_a3', 's_a4', 't_up', 't_down', 't_phi', 't_psi', 't_a1', 't_a2', 't_a3', 't_a4', 'Interaction')]
index0 <- createDataPartition(data1$Interaction, p=0.5, list = FALSE)
data1 <- data1[index0, ]

data1 <- na.omit(data1)

#create train set
index <- createDataPartition(data1$Interaction, p=0.8, list = FALSE)
train <- data1[index, ]

#train the model
nbc <- naive_bayes(Interaction~. , data = train)


#########10-k-fold-features-selection#########
set.seed(123)
cv.error.10 <- rep(0,10)
for (i in 1:10) {
  index <- createDataPartition(data1$Interaction, p=0.8, list = FALSE)
  test1 <- data1[-index, ]
  prediction <- predict(nbc, newdata=test1)
  tab <- table(prediction, test1$Interaction)
  cv.error.10[i] <- round((sum(diag(tab))/sum(tab))*100,2)
}
mean(cv.error.10)

#watch the confusion matrix
tab
#save the confusion matrix
print(xtable(tab, type = "latex"), file = "cf_nbc.tex")


#resampling data set

hbond.data <- data1[data1$Interaction == "HBOND",]
vdw.data <- data1[data1$Interaction == "VDW",]
ssbond.data <- data1[data1$Interaction == "SSBOND",]
pication.data <- data1[data1$Interaction == "PICATION",]
pip.data <- data1[data1$Interaction == "PIPISTACK",]
io.data <- data1[data1$Interaction == "IONIC",]

set.seed(42)
split.hbond1 <- sample(c(rep(0, 0.97 * nrow(hbond.data)), rep(1, 0.03 * nrow(hbond.data))))
hbond.out1 <- hbond.data[split.hbond1 == 0, ] 
hbond.in1 <- hbond.data[split.hbond1 == 1, ]

split.vdw1 <- sample(c(rep(0, 0.93 * nrow(vdw.data)), rep(1, 0.07 * nrow(vdw.data))))
vdw.out1 <- vdw.data[split.vdw1 == 0, ] 
vdw.in1 <- vdw.data[split.vdw1 == 1, ] 

split.ssbond1 <- sample(c(rep(0, 0.2 * nrow(ssbond.data)),rep(1, 0.8 * nrow(ssbond.data))))
ssbond.out1 <- ssbond.data[split.ssbond1 == 0, ] 
ssbond.in1 <- ssbond.data[split.ssbond1 == 1, ]
ssbond.in1 <- rbind(ssbond.in1, ssbond.data, ssbond.data, ssbond.data, ssbond.data, ssbond.data, ssbond.data, ssbond.data, ssbond.data, ssbond.data, ssbond.data, ssbond.data)

split.pip1 <- sample(c(rep(0, 0.92 * nrow(pip.data)), rep(1, 0.08 * nrow(pip.data))))
pip.out1 <- pip.data[split.pip1 == 0, ] 
pip.in1 <- pip.data[split.pip1 == 1, ] 
pip.in1 <- rbind(pip.in1, pip.data)

split.pication1 <- sample(c(rep(0, 0.65 * nrow(pication.data)), rep(1, 0.35 * nrow(pication.data))))
pication.out1 <- pication.data[split.pication1 == 0, ]
pication.in1 <- pication.data[split.pication1 == 1, ]
pication.in1 <- rbind(pication.in1, pication.data, pication.data, pication.data, pication.data, pication.data, pication.data, pication.data, pication.data, pication.data, pication.data, pication.data, pication.data, pication.data, pication.data, pication.data, pication.data, pication.data,pication.data)

split.io1 <- sample(c(rep(0, 0.75 * nrow(io.data)), rep(1, 0.25 * nrow(io.data))))
io.out1 <- io.data[split.io1 == 0, ] 
io.in1 <- io.data[split.io1 == 1, ] 
io.in1 <- rbind(io.in1, io.data)

# we produce a data set with resampled data
data2 <- rbind(hbond.in1, vdw.in1, ssbond.in1, io.in1, pip.in1, pication.in1)
data2 <- data2[sample(1:nrow(data2)), ]

#show the data distributaion 
classes1 <- table(data2$Interaction)
classes1


#train the model with resampled data set
nbc_resampled <- naive_bayes(Interaction~., data = data2)

#########10-k-fold-features-selection#########
set.seed(123)
cv.error.10 <- rep(0,10)
for (i in 1:10) {
  index <- createDataPartition(data2$Interaction, p=0.8, list = FALSE)
  test1 <- data2[-index, ]
  prediction <- predict(nbc_resampled, newdata=test1)
  tab <- table(prediction, test1$Interaction)
  cv.error.10[i] <- round((sum(diag(tab))/sum(tab))*100,2)
}
mean(cv.error.10)

#watch the confusion matrix
tab
#save the confusion matrix
print(xtable(tab, type = "latex"), file = "cf_nbc_resamples.tex")

#now we can see how shrinked multinomial model work with resampled dataset
library(glmnet)
Y = data2$Interaction
X = as.matrix(data2[,1:16])

multinom_shrinked_r = cv.glmnet(X, Y, family='multinomial', 
                                type.measure = 'class', alpha=1)

set.seed(123)
cv.error.10 <- rep(0,10)
for (i in 1:10) {
  index <- createDataPartition(data2$Interaction, p=0.8, list = FALSE)
  test1 <- data2[-index, ]
  prediction <- predict(nbc_resampled, newdata=test1)
  tab <- table(prediction, test1$Interaction)
  cv.error.10[i] <- round((sum(diag(tab))/sum(tab))*100,2)
}
mean(cv.error.10)
tab

print(xtable(tab, type = "latex"), file = "cf_multinom_resamples.tex")
