# KNN PRACTICE

# setup
rm(list=ls())
# Note: change the work directory to the one on your computer
setwd("~/Desktop/5415 R")

credit = read.csv('/Users/lexidingle/Desktop/5415 R/germancredit-1.csv', stringsAsFactors = T)

library(class)
set.seed(200, sample.kind = "Rounding")
train <- sample(nrow(credit),nrow(credit)*0.60)
credit.train <- credit[train,]
credit.test <- credit[-train,]
credit$Default <- as.factor(credit$Default)



nrow(credit)

credit.knn <- knn(scale(credit.train[,sapply(credit,is.numeric)]), 
                  scale(credit.test[,sapply(credit,is.numeric)]),
                  credit.train$Default,k=5)
accuracy <- mean(credit.test$Default==credit.knn) 
accuracy


# knn with scaling
credit.knn <- knn(scale(credit.train[,sapply(credit,is.numeric)]), 
                  scale(credit.test[,sapply(credit,is.numeric)]),
                  credit.train$Default,k=11)
accuracy <- mean(credit.test$Default==credit.knn) 
accuracy


# looping over k
vk <- c(5,11,21,31)
accuracy = vk
for (i in 1:length(vk)){
  credit.knn <- knn(scale(credit.train[,sapply(credit,is.numeric)]), 
                    scale(credit.test[,sapply(credit,is.numeric)]), 
                    credit.train$Default,k=vk[i])
  accuracy[i] <- mean(credit.test$Default==credit.knn)  
}
plot(vk,accuracy,xlab='k',ylab='test accuracy',col='blue')
accuracy


#confusion matrix from knn prediction
credit.knn <- knn(scale(credit.train[,sapply(credit,is.numeric)]), 
                  scale(credit.test[,sapply(credit,is.numeric)]),
                  credit.train$Default,k=21)
tb <- table(credit.test$Default,credit.knn)
# True positive rate
TPR <- tb[2,2]/(tb[2,2] + tb[2,1])
TPR

