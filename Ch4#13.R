rm(list=ls())
library(MASS)
str(Boston)
summary(Boston$rad)
dim(Boston)
test = sample(506,100)
train = setdiff(1:506,test)

sum(is.na(Boston))

med.crim <- median(Boston$crim)

Boston$crime_level <- ifelse(Boston$crim>med.crim,1,0)

library(ggcorrplot)
ggcorrplot(cor(Boston))
cor(Boston)
which(cor(Boston)>.5 & cor(Boston) < 1)
## There is some serious correlation here.

### Full logistic (- crime level of course)
log.fit <- glm(crime_level ~. - crim, data=Boston[train,], family=binomial)
log.pred <- predict(log.fit,Boston[test,], type='response')
log.cat <- log.pred > .5
mean(log.cat==Boston[test,'crime_level']) # .92. Pretty good!
summary(log.fit)

### Ridge Regression
## woops this is not for categorical vars but cts. linear regression.
library(glmnet)
cols.to.use <- setdiff(colnames(Boston),c('crime_level','crim'))
X <- as.matrix(Boston[,cols.to.use])
Y <- Boston[,'crime_level']
ridge.out=cv.glmnet(X[train,],Y[train],alpha=0, family='binomial')
predict(ridge.out, type='coefficients',s=ridge.out$lambda.min) # prints out coefficients.
ridge.pred <- predict(ridge.out,s=ridge.out$lambda.min,newx=X[test,],type='response')
mean((ridge.pred - Y[test])^2) # .0926. Not bad.
ridge.cat <- ridge.pred > .5
mean(ridge.cat==Y[test]) # .85 that's ok.

### Lasso Regression
lasso.out = cv.glmnet(X[train,],Y[train],alpha=1,family='binomial')
lasso.out$lambda.min
predict(lasso.out, type='coefficients',s=lasso.out$lambda.min) 
lasso.pred <- predict(lasso.out,s=lasso.out$lambda.min,newx=X[test,],type='response')
lasso.cat <- lasso.pred > .5
mean(lasso.cat == Y[test]) # .87.

### Elastic Net
# I am not optimizing for alpha just yet, but rather choosing one in the middle. I will
# optimize for alpha in the next round.
EN.out = cv.glmnet(X[train,],Y[train],alpha=0.5,family='binomial')
EN.out$lambda.min
predict(EN.out, type='coefficients',s=EN.out$lambda.min) # none removed. nox = 34.7?
EN.pred <- predict(EN.out,s=EN.out$lambda.min,newx=X[test,],type='response')
EN.cat <- EN.pred > .5
mean(EN.cat == Y[test]) # 0.9, so def better. I can't imagine KNN is as good as this,
# but I'll try.

### LDA 
lda.fit <- lda(crime_level~.-crim, data=Boston[train,])
lda.pred <- predict(lda.fit,Boston[test,])$class
mean(lda.pred == Y[test]) #.86, but this is using every var. How can I see significane
# of specific vars?

### QDA
qda.fit <- qda(crime_level~.-crim, data=Boston[train,])
qda.pred <- predict(qda.fit,Boston[test,])$class
mean(qda.pred == Y[test]) #.88

### KNN
library(class)
knn.res <- cbind(1:20,0); colnames(knn.res) <- c('k','accuracy')
cols <- setdiff(colnames(Boston),c('crim','crime_level'))
for (kk in 1:20){
  knn.pred <- knn(train=Boston[train,cols], test=Boston[test,cols],
                  cl = Boston[train,'crime_level'], k = kk)
  knn.res[kk,2] <- mean(knn.pred == Y[test])
}
# knn results are really good! Wow. They are all in the 90's. They even reach .95. 
# Im going to pick k = 8 b/c accuracy = .94 and closeby k's are better.
knn.pred <- knn.pred <- knn(train=Boston[train,cols], test=Boston[test,cols],
                            cl = Boston[train,'crime_level'], k = 8)

### Majority voting: kNN, Elastic, Logistic
total.pred <- as.integer(knn.pred)-1 + EN.cat + log.cat
total.cat <- ifelse(total.pred < 2, 0, 1)
mean(total.cat == Y[test]) # .92 - worse than just knn straight up. Can you 
# even make this better? Should I rerun with different test
# in almost all of the majority votes knn had the right choice. It's just pure
# knn for this one. What if I did some PCA before to help the process? That could
# help but it's not essential right now. I still need to analyze ovarian cancer 
# data and then do the Kaggle quest. I do like the process of data analysis though.
# In a second after I go to the BR I will review what I've learned, because I should
# be learning while doing this and not doing it rotely.

