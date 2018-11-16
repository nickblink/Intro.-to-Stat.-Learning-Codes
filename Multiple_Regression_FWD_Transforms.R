# Attempt the forwards model and the backwards model and then the mixed selection model
# for the Boston data set. It is likely that all predictors have some effect...nvm
library(MASS)
attach(Boston)

n <- ncol(Boston) - 1
B.names <- colnames(Boston)

# Start with one iteration first and then figure it out.
remaining.cols <- list(1:n,1:n,1:n,1:n)
names(remaining.cols) <- c('X','X^2','sqrt(X)','log(X+1)')
model.f <- "medv~"

# cycle through to add 6 variables?
for (j in 1:6){
  rsq <- remaining.cols
  
  # Calculate all X.
  for (i in 1:length(remaining.cols[[1]])){
    if (j==1){f <- as.formula(paste(model.f,B.names[remaining.cols[[1]][i]],sep=""))}else{
      f <- update(model.f,paste("~ . +",B.names[remaining.cols[[1]][i]],sep=""))
    }
    rsq[[1]][i] <- summary(lm(f))$r.sq
  }
  
  # X^2
  for (i in 1:length(remaining.cols[[2]])){
    if (j==1){f <- as.formula(paste(model.f,"I(",B.names[remaining.cols[[2]][i]],"^2)",sep=""))
     }else{
      f <- update(model.f,paste("~ . + I(",B.names[remaining.cols[[2]][i]],"^2)",sep=""))
    }
    rsq[[2]][i] <- summary(lm(f))$r.sq
  }
  
  for (i in 1:length(remaining.cols[[3]])){
    if (j==1){f <- as.formula(paste(model.f,"sqrt(",B.names[remaining.cols[[3]][i]],")",sep=""))
    }else{
      f <- update(model.f,paste("~ . + sqrt(",B.names[remaining.cols[[3]][i]],")",sep=""))
    }
    rsq[[3]][i] <- summary(lm(f))$r.sq
  }
  
  for (i in 1:length(remaining.cols[[4]])){
    if (j==1){f <- as.formula(paste(model.f,"log(",B.names[remaining.cols[[4]][i]],"+1)",sep=""))
    }else{
      f <- update(model.f,paste("~ . + log(",B.names[remaining.cols[[4]][i]],"+1)",sep=""))
    }
    rsq[[4]][i] <- summary(lm(f))$r.sq
  }

  rsq.index <- which.max(sapply(rsq,max))
  new.var <- names(Boston)[remaining.cols[[rsq.index]][which.max(rsq[[rsq.index]])]]
  remaining.cols[[rsq.index]] <- remaining.cols[[rsq.index]][-which.max(rsq[[rsq.index]])]
  
  if (rsq.index==1){new.var.form <- new.var
  }else if (rsq.index == 2){new.var.form <- paste('I(',new.var,'^2)',sep="")
  }else if (rsq.index == 3){new.var.form <- paste('sqrt(',new.var,')',sep="")
  }else {new.var.form <- paste('log(',new.var,')',sep="")}
  
  if (j == 1) {model.f <- as.formula(paste(model.f,new.var.form,sep=""))} else{
     # model.f <- update(model.f, ~.+new.var)
    model.f <- update(model.f,paste("~ . +",new.var.form))
  }
}

