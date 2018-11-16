### ISL chapter 4 number 10
rm(list=ls())
library(ISLR)
library(ggcorrplot)
library(data.table)
D <- Weekly

#pairs(D)

par(mfrow=c(3,3))
for(i in 1:9){
  hist(D[,i], main = colnames(D)[i])
}

ggcorrplot(cor(D[,-9]))

log.fit <- glm(as.numeric(Direction)~.-Year - Today,data=D)
summary(log.fit)

for (i in 1:8){
  print(summary(lm(as.numeric(Direction)~.,data=D[,c(i,9),drop=F])))
}
head(D[,i])
head(D$Direction)
head(as.numeric(D$Direction))
# 1 = Down, 2 = Up

D.fit <- (predict(log.fit,D)>1.5) + 1
table(D$Direction,D.fit)
mean(as.numeric(D$Direction)==D.fit)
# = .563. Yikes that is bad.

D$Dir2 = as.numeric(D$Direction)-1
train <- D[D$Year<=2008,]
test <- D[D$Year >= 2009,]

train.fit=glm(Dir2~Lag2,data=train, family = binomial)
test.pred <- predict(train.fit,test,type='response')
table(test$Dir2,test.pred>.5)

D.fit <- (predict(log.fit,D)>0.5)
table(D$Dir2,D.fit)
# That aint right. .45 accuracey? I did something wrong.

#### Linear Discriminant Analyis
lda.fit <- lda(Dir2~Lag2, data=train)
lda.pred <- predict(lda.fit,test)$class
mean(lda.pred==test$Dir2)

### Quadratic Discriminant Analysis
qda.fit <- qda(Direction~Lag2, data=train)
qda.pred <- predict(qda.fit,test)$class
mean(qda.pred==test$Dir2)

train = as.data.table(train)
test = as.data.table(test)
### KNN classification
knn.res <- cbind(1:20,0); colnames(knn.res) <- c('k','accuracy')
for (kk in 1:20){
  knn.pred <- knn(train[,-c(8,9,10)], test[,-c(8,9,10)],
                  cl = train$Direction, k = kk)
  knn.res[kk,2] <- mean(knn.pred == test$Direction)
}
# accuracy = 56%. Any reason to use LDA over logit when only two vars?
# When there is a linear boundary? Not sure how to interpret that.

### Let's try some model majority voting with LDA, QDA, and KNN (k=13)
# also can do some probability averaging
knn.pred <- knn(train[,-c(8,9,10)], test[,-c(8,9,10)],
                cl = train$Direction, k = 13, prob = TRUE)
knn.pred <- as.integer(knn.pred)-1
lda.pred <- as.integer(lda.pred)-1
qda.pred <- as.integer(qda.pred)-1

majority.pred <- ifelse(knn.pred + lda.pred + qda.pred >= 2, 1, 0)
mean(majority.pred == test$Dir2)

library(data.table)
test = as.data.table(D)
# Want to see the distribution of volumes by year
test[,mean(Volume),by=Year]



