### Chapter 8 Question 7
# Using random forests with various mtry and ntree values on the data set.

# mtry = the size of the random subset we take each time.
# ntree = number of trees. Should be monotonically decreasing (generally)

library(caret)
library(randomForest)
library(MASS)
library(tree)
library(gbm)


### Using random forests on Boston.
test.sample = sample(nrow(Boston),100)
train = Boston[-test.sample,]
test = Boston[test.sample,]
y = Boston[test.sample,14]

par(mfrow=c(3,3))
tree.seq = seq(25,500,25)
results = data.frame(matrix(0,
                     nrow=13,ncol=3))
colnames(results) = c('m','min RMSE','# trees')
for (m in 1:13){
  RMSE = c()
  for (num.trees in tree.seq){
    boston.rf = randomForest(medv~.,data=train,mtry=m,
                            importance=TRUE,ntree=num.trees)
    boston.pred = predict(boston.rf,newdata=test)
    RMSE = c(RMSE,sqrt(sum((y - boston.pred)^2)))
  }
  results[m,1]=m
  results[m,2]=min(RMSE)
  results[m,3]=tree.seq[which.min(RMSE)]
  plot(tree.seq,RMSE,main=paste0('m = ',m),xlab='# trees')
}
print(results)

# I would look at 5-10 with more trees, since this
# should stabilize with more trees.
for (m in 5:10){
  boston.rf = randomForest(medv~.,data=train,mtry=m,
                           importance=TRUE,ntree=3000)
  boston.pred = predict(boston.rf,newdata=test)
  print(m)
  print(sqrt(sum((y - boston.pred)^2)))
}

### Now using gradient boosted trees.
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
test = setdiff(1:nrow(Boston), train)
boston.test = Boston[test,'medv']
set.seed(1)
boost.boston = gbm(medv~.,data=Boston[train,],distribution='gaussian',
                   n.trees = 10000, interaction.depth = 4)
yhat.boost = predict(boost.boston,newdata=Boston[-train,],n.trees=5000)                   
mean((yhat.boost-boston.test)^2)

error.rates = c()
i = 0
for (num.trees in seq(1,10000,100)){
  yhat.boost = predict(boost.boston,newdata=Boston[-train,],n.trees=num.trees)
  error.rates = c(error.rates,mean((yhat.boost-boston.test)^2))
}
plot(seq(1,10000,100),error.rates,main='error rates vs number of trees')

error.rates = c()
for (depth.trees in 1:15){
  boost.boston = gbm(medv~.,data=Boston[train,],distribution='gaussian',
                     n.trees = 5000, interaction.depth = depth.trees)
  yhat.boost = predict(boost.boston,newdata=Boston[-train,],n.trees=5000)                   
  error.rates = c(error.rates,mean((yhat.boost-boston.test)^2))
}

plot(1:15, error.rates, main = 'error rate vs. depth of trees')
# why distribution gaussian when we are doing trees?


                   
### Question 10 -
library(gbm)
library(ISLR)
D = Hitters
dim(D)
# 322 20 
sum(is.na(D$Salary))
# 59

D = D[!is.na(D$Salary),]
D$Salary = log1p(D$Salary)

train = sample(1:nrow(D),200)
D.train = D[train,]
lambda.grid = 10^(seq(-4,-1,by=0.25))
#lambda.grid=.001

# what should interaction depth be?
train.MSE  = c()
test.MSE = c()

num.trees = 500
for (lam in lambda.grid){
  boost.hitters=gbm(Salary~.,data=D.train,shrinkage=lam,distribution="gaussian",
                    n.trees=num.trees,interaction.depth=4)
  train.MSE = c(train.MSE, boost.hitters$train.error[num.trees])
  yhat = predict(boost.hitters,newdata=D[-train,-19],n.trees=num.trees)
  test.MSE = c(test.MSE, mean((yhat-D[-train,"Salary"])^2))
  #my.tr.error=sum((D.train$Salary-predict(boost.hitters,newdata=D.train,n.trees=1000))^2)/length(train)
}
plot(log10(lambda.grid),train.MSE,
     main=paste0('train vs test MSE w/ ',num.trees,' trees'),col='blue')

points(log10(lambda.grid),test.MSE,col='red')
legend(1,95,legend=c('train MSE','test MSE'), lty=1:2,cex=.8,col=c('red','blue'))

# MSE gets down to .18


## Two other approaches: ridge regression, lasso, best subset, PCR, kNN
knn.hitters = knn(D.train[-14],D[-train,-14],y=D.train[14])
library(FNN) # :( I don't have it

### Ridge/Lasso/Elastic Net
library(glmnet)
x=model.matrix(Salary~.,D)[,-1]
y=D$Salary

grid=10^seq(10,-2,length=100)
ridge.mod=cv.glmnet(x[train,],y[train],alpha=0,lambda=grid)
lasso.mod=cv.glmnet(x[train,],y[train],alpha=1,lambda=grid)
elastic.mod=cv.glmnet(x[train,],y[train],alpha=0.5,lambda=grid)

ridge.pred = predict(ridge.mod,s=ridge.mod$lambda.min,newx=x[-train,])
lasso.pred = predict(lasso.mod,s=lasso.mod$lambda.min,newx=x[-train,])
elastic.pred = predict(elastic.mod,s=elastic.mod$lambda.min,newx=x[-train,])

ridge.MSE = mean((ridge.pred - y[-train])^2)
# .435
lasso.MSE = mean((lasso.pred - y[-train])^2)
# .420
elastic.MSE = mean((elastic.pred - y[-train])^2)
# .423

# yikes these are much, much worse than the .19 MSE of boosting.
bagging.hitters = randomForest(Salary~.,data=D[train,],mtry=(ncol(D)-1),
                               ntree=1000)
bagging.pred = predict(bagging.hitters,newdata=D[-train,])
bagging.MSE = mean((bagging.pred-D[-train,'Salary'])^2)
# 0.203. Pretty darn close to boosting. Naht bad.

randomForest(medv~.,data=train,mtry=m,
             importance=TRUE,ntree=num.trees)















