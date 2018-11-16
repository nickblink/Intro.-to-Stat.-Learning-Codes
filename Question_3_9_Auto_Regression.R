# First, try all interaction terms

interactions <- as.data.frame(matrix(,nrow=21,ncol=4))
names(interactions) <- c('var1','var2','r.squared','f.stat')

iter <- 0
for (i in 2:7){
   for (j in (i+1):8){
     iter <- iter + 1
     f <- as.formula(paste('mpg~.-name+',names(Auto)[i],
                           ':',names(Auto)[j],sep=""))
      print(f)
     lm.fit <- lm(f,data=Auto)
     interactions[iter,1] <- names(Auto)[i]
     interactions[iter,2] <- names(Auto)[j]
     interactions[iter,3] <- summary(lm.fit)$r.squared
     interactions[iter,4] <- summary(lm.fit)$fstatistic[1]
   }
}

interactions[sort.list(interactions$r.squared, decreasing=TRUE),]

# Next try all log, sqrt, and squared?
transforms <- as.data.frame(matrix(nrow = 7,ncol = 4))
names(transforms) <- c('variable','r(log)','r(squared)','r(sqrt)')
for (i in 2:8){
  f.log1 <- as.formula(paste('mpg~log(',names(Auto)[i],'+1)',sep=''))
  f.squared <- as.formula(paste('mpg~I(',names(Auto)[i],'^2)',sep=''))
  f.sqrt <- as.formula(paste('mpg~sqrt(',names(Auto)[i],')',sep=''))
  
  transforms[(i-1),1] <- names(Auto)[i]
  transforms[(i-1),2] <- summary(lm(f.log1, data=Auto))$r.sq
  transforms[(i-1),3] <- summary(lm(f.squared, data=Auto))$r.sq
  transforms[(i-1),4] <- summary(lm(f.sqrt, data=Auto))$r.sq
}

