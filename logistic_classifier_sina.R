

# load the data
data = read.table(file = 'aa_features.tsv', sep = '\t', header = TRUE)
data <- data[!(data$Interaction == ""), ]


# choosing numeric features
data1 <- data[, c('s_rsa', 's_up', 's_down', 's_phi', 's_psi', 's_a1', 's_a2', 's_a3', 's_a4', 's_a5',  't_rsa', 't_up', 't_down', 't_phi', 't_psi', 't_a1', 't_a2', 't_a3', 't_a4', 't_a5', 'Interaction')]


# preparing
data1 <- na.omit(data1)


# data observation
sapply(data1[, c('s_rsa', 's_up', 's_down', 's_phi', 's_psi', 's_a1', 's_a2', 's_a3', 's_a4', 's_a5',  't_rsa', 't_up', 't_down', 't_phi', 't_psi', 't_a1', 't_a2', 't_a3', 't_a4', 't_a5')], var)
classes <- table(data1$Interaction)
classes

# detaching data set
hbond.data <- data1[data1$Interaction == "HBOND",]
vdw.data <- data1[data1$Interaction == "VDW",]
ssbond.data <- data1[data1$Interaction == "SSBOND",]
pication.data <- data1[data1$Interaction == "PICATION",]
pip.data <- data1[data1$Interaction == "PIPISTACK",]
io.data <- data1[data1$Interaction == "IONIC",]

set.seed(42)
split.hbond <- sample(c(rep(0, 0.998 * nrow(hbond.data)), rep(1, 0.002 * nrow(hbond.data))))
hbond.out <- hbond.data[split.hbond == 0, ] 
hbond.in <- hbond.data[split.hbond== 1, ]

split.vdw <- sample(c(rep(0, 0.994 * nrow(vdw.data)), rep(1, 0.004 * nrow(vdw.data))))
vdw.out <- vdw.data[split.vdw == 0, ] 
vdw.in <- vdw.data[split.vdw== 1, ] 

split.ssbond <- sample(c(rep(0, 0.3 * nrow(ssbond.data)), rep(1, 0.7 * nrow(ssbond.data))))
ssbond.out <- ssbond.data[split.ssbond == 0, ] 
ssbond.in <- ssbond.data[split.ssbond== 1, ] 

split.pip <- sample(c(rep(0, 0.94 * nrow(pip.data)), rep(1, 0.06 * nrow(pip.data))))
pip.out <- pip.data[split.pip == 0, ] 
pip.in <- pip.data[split.pip == 1, ] 


pication.in <- pication.data


split.io <- sample(c(rep(0, 0.93 * nrow(io.data)), rep(1, 0.07 * nrow(io.data))))
io.out <- io.data[split.io == 0, ] 
io.in <- io.data[split.io == 1, ] 



data2 <- rbind(hbond.in, vdw.in, ssbond.in, io.in, pip.in, pication.in)
data2 <- data2[sample(1:nrow(data2)), ]

#train and test split
set.seed(123)
split1<- sample(c(rep(0, 0.9 * nrow(data2)), rep(1, 0.1 * nrow(data2))))
table(split1)
# train data
train <- data2[split1 == 0, ] 


#test data
test <- data2[split1== 1, ]  

# data scaling
# train <- scale(train[,1:20])
# test <- scale(test[,1:20])

# Naive Bayes Multi-class classifier 
library(naivebayes)
model <- naive_bayes(Interaction ~ . , data = train)

summary(model)


# Make predictions
predicted.classes <- predict(model, test)
head(predicted.classes)

# output and accuracy
tab1 <- table(predicted.classes, test$Interaction)
tab1
accuracy <- 1 - sum(diag(tab1)) / sum(tab1)
accuracy

# reduced model
model <- naive_bayes(Interaction ~ .-s_up , data = train)
predicted.classes <- predict(model, test)
tab1 <- table(predicted.classes, test$Interaction)
accuracy <- 1 - sum(diag(tab1)) / sum(tab1)
accuracy
