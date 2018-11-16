# Attempt the forwards model and the backwards model and then the mixed selection model
# for the Boston data set. It is likely that all predictors have some effect...nvm
library(MASS)
attach(Boston)

n <- ncol(Boston) - 1
B.names <- colnames(Boston)

# Start with one iteration first and then figure it out.
remaining.cols <- 1:n
model.f <- "medv~"

# cycle through to add 6 variables?
for (j in 1:6){
  rsq <- matrix(nrow=length(remaining.cols),ncol=1)
  
  for (i in 1:length(remaining.cols)){
    if (j==1){f <- as.formula(paste(model.f,B.names[remaining.cols[i]],sep=""))}else{
      f <- update(model.f,paste("~ . +",B.names[remaining.cols[i]]))
    }
    rsq[i] <- summary(lm(f))$r.sq
  }
  # Update model
  new.var <- names(Boston)[remaining.cols[which.max(rsq)]]
  
  if (j == 1) {model.f <- as.formula(paste(model.f,new.var,sep=""))} else{
     # model.f <- update(model.f, ~.+new.var)
    model.f <- update(model.f,paste("~ . +",new.var))
  }
  # Update remaining columns
  remaining.cols <- remaining.cols[-which.max(rsq)]
}

