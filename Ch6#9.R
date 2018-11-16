### Chapter 6 Question 9. Applying the different dimension reduction methods
# to predicting stuff in the college data set,
library(ISLR)
dim(College)
names(College)
D = College
head(D)

my_RMSE <- function(x,y){
  sqrt(sum((x-y)^2)/length(y))
}
# (a)
test.sample = sample(1:777, 100)
test = D[test.sample,]
test.y = test$Apps
cols <- setdiff(colnames(test),'Apps')
test.data = test[,cols]
train = D[-test.sample,]

# (b) plain old linear regression
lm.fit <- lm(Apps~.,data=train)
my_RMSE(predict(lm.fit,test),test$Apps)
# 1538

# (c) ridge
library(glmnet)
x = model.matrix(Apps~.,data=College)
y = train$Apps
grid = 10^seq(4,-2,length=100)
ridge.mod=cv.glmnet(x[-test.sample,],y,alpha=0,lambda=grid)
lambda.best = ridge.mod$lambda.min
my_RMSE(predict(ridge.mod,s=lambda.best,newx = x[test.sample,]),test.y)
# 1606

# (d) lasso
lasso.mod = cv.glmnet(x[-test.sample,],y,alpha=1,lambda=grid)
lambda.best = lasso.mod$lambda.min
my_RMSE(predict(lasso.mod,s=lambda.best,newx = x[test.sample,]),test.y)
#1598

# (e) PCR yo. 
Nah I need a break.
