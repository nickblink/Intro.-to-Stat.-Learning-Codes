# I guess all the previous stuff got deleted. Well screw it I'm gonna 
# continue with 8. Whose gonna come back and look at this anyway?

library(ISLR)
D = Auto
D$mpg.level = ifelse(D$mpg>median(D$mpg),1,0)
D$mpg.level = as.factor(D$mpg.level)
D = D[,setdiff(Colnames(D),'mpg')]

library(e1071)
tune.out = tune(svm,mpg.level~.-mpg,data=D,kernel="linear",
                ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
# error is pretty similar for .1, 1, 5, but is the lowest with cost=1 (error =.095)
# that's a pretty good error. = 90.5% classification accuracy.
svm.lin = svm(mpg.level ~., data = D, kernel='linear',cost=0.01)

tune.rad = tune(svm,mpg.level~.-mpg,data=D, kernel="radial",
                ranges = list(cost = c(.01,1,10,100),
                              gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.rad)
# min at cost = 1, gamma = 0.5. Error = .063.
# What's the role of gamma again? This will 
# be a tight fit. Idk now. If I end up needing to use svms I should look
# into this more, but for now I will pass.
svm.rad = svm(mpg.level~.-mpg,data=D, kernel="radial",cost=10,gamma=0.5)

tune.poly = tune(svm,mpg.level~.-mpg,data=D, kernel="polynomial",
                 ranges = list(cost=c(.01, 1, 10, 100),
                               degree = c(1,2,3,4)))
summary(tune.poly)
# bad all around. It did best with degree 1, aka linear. Obvi similar
# results to linear (though slightly diff because of randomness).
svm.poly = svm(mpg.level~.-mpg,data=D, kernel="polynomial",degree=2,cost=1)


par(mfrow=c(1,3))
plot(svm.lin,D)
plot(svm.rad,D)
plot(svm.poly,D)


#####
rm(list=ls())
library(ISLR)
train.sample = sample(1070,800)
train = OJ[train.sample,]
test = OJ[-train.sample,]

svm.fit = svm(Purchase~.,data=train,cost=0.01,kernel='linear')
summary(svm.fit)

mean(train$Purchase==predict(svm.fit,train))
# 0.82 accuracy - weirdly lower than test, though they're 
# close enough to be random. I guess the linear
# model is unlikely to overfit, esp. with all of this data.

mean(test$Purchase==predict(svm.fit,test))
# 0.84 accuracy. It's aite.

library(e1071)
tune.out = tune(svm, Purchase~.,data=train,kernel = 'linear', 
                ranges = list(cost = c(0.01,0.1,1,5,10)))

# (e) no point in this one - computing new training and test error rates

tune.out = tune(svm, Purchase~.,data=train,kernel =  'radial',
                ranges = list(gamma = c(0.5,1,2,3,4),
                              cost = c(0.01,0.1,1,10)))
# best has error ~ 0.21

radial.best = svm(Purchase~.,data=train,kernel =  'radial',
                  gamma = 0.5, cost = 1)
radial.worst = svm(Purchase~.,data=train,kernel =  'radial',
                   gamma = 3, cost = .1)
mean(test$Purchase==predict(radial.best,test))
# 0.82

mean(test$Purchase==predict(radial.worst,test))
# 0.64. No bueno

tune.out = tune(svm, Purchase~.,data=train,kernel =  'polynomial',
                ranges = list(degree = c(1,2,3,4),
                              cost = c(0.01,0.1,1,10)))
poly.best = svm(Purchase~.,data=train,kernel =  'polynomial',
                degree = 1, cost = 1)
poly.worst = svm(Purchase~.,data=train,kernel =  'polynomial',
                 degree = 1, cost = .01)
mean(test$Purchase==predict(poly.best,test))
# 0.84

mean(test$Purchase==predict(poly.worst,test))
# 0.82

# Again we have a linear boundary is the best. Shows that it
# all depends on the data set. Radial does not do as well
# on this data set. I am really tired. I just want to go home
# and nap.





