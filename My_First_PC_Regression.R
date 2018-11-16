# Function first PCA
set.seed(1)
x1 <- runif(100)
x2 <- .5*x1+rnorm(100)
y <- 2+2*x1+0.3*x2+rnorm(100)

cor.fit <- lm(x2 ~ x1)
int <- summary(cor.fit)$coefficients[1,1]
slope <- summary(cor.fit)$coefficients[2,1]

pc.x <- (x2-int+x1/slope)/(slope+1/slope)
pc.y <- pc.x*slope + int

avg.x <- mean(pc.x); avg.y <- mean(pc.y)
pc.values <- sqrt((pc.x-avg.x)^2+(pc.y-avg.y)^2)*(-1)^(pc.x < avg.x)

lm.pc <- lm(y~pc.values)
lm.x1 <- lm(y~x1)
lm.x2 <- lm(y~x2)


# graphics.off()
# plot(x1,x2,xlim=c(-3,3),ylim=c(-3,3))
# abline(cor.fit,col='blue')
# points(pc.x,pc.y,col='red')

# for(i in 1:length(x1)){
# abline(x1[i]/slope+x2[i],-1/slope)
# }




