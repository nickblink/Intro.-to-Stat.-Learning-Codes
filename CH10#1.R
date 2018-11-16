# Chapter 10 questions - have I already done some of these?
# Well either way it's take two!
rm(list=ls())
library(ISLR)
set.seed(1)
D = t(scale(t(USArrests)))
dist.D = dist(D)^2
cor.D = as.dist(1-cor(t(D)))


dist.D= dist(t(D))^2
cor.D = 1-cor(D)
cor.D = as.dist(cor.D)
summary(cor.D/dist.D)
# so this def works column-wise, not row-wise. That's the issue
# I had with the answer from prince honest.


#######
# Question 8)
rm(list=ls())
library(ISLR)
set.seed(1)
D = USArrests
pr.out = prcomp(D, scale=TRUE)
pr.var = pr.out$sdev^2
pr.cum.var = pr.var/sum(pr.var)
pr.cum.var


# Question 9)
complete = hclust(dist(D), method='complete')
# without scaling, assault dominates since it is the biggest
# variable. Clustering is almost entirely done based on assault.
# Variables should be scaled, assuming we treat variation in assault,
# rape, urbanPop, and murder equally. Is there ever some heuristic in how
# to scale these variables based on what we care about more? That would 
# some hand-wavy work, but isn't it kind of arbitrary that we scale all 
# the variables to one anyway?


# Question 10
data.1 = as.data.frame(matrix(rnorm(20*50), ncol=50))
data.2 = as.data.frame(matrix(rnorm(20*50), ncol=50))+0.7
data.3 = as.data.frame(matrix(rnorm(20*50), ncol=50))+1.3
D = cbind(rbind(data.1,data.2,data.3),c(rep(1,20),rep(2,20),rep(3,20)))

D = D[sample(60),]
classes = D[,51]
D = D[,-51]
pr.out = prcomp(D,scale=TRUE)
plot(pr.out$x[,1],pr.out$x[,2], col= classes)

km.out = kmeans(D,3,nstart=30)
table(km.out$cluster, classes)

km.out = kmeans(D,2,nstart=30)
table(km.out$cluster, classes)

km.out = kmeans(D,4,nstart=30)
table(km.out$cluster, classes)

km.out = kmeans(scale(D),3,nstart=30)
table(km.out$cluster, classes)

plot(pr.out$x[,1],pr.out$x[,2], col= km.out$cluster)






