## I think this is ISL ch.3 #9
library(MASS)

str(Boston)
signif.vars = c()


alpha= .05
single.var = c()
for (var in colnames(Boston)[-1]){
  print(var)
  model = as.formula(paste0('crim ~ ', var))
  lm.fit = lm(model, data=Boston)
  #print(summary(lm.fit))
  single.var <- c(single.var,summary(lm.fit)$coefficients[2,1])
  if (summary(lm.fit)$coefficients[2,4]<alpha){
    signif.vars <- c(signif.vars,var)
  }
}
signif.vars
plot(Boston$medv, Boston$crim)

lm.full = lm(crim~.,data=Boston)
summary(lm.full)
multi.var = summary(lm.full)$coefficients[,1]

for (var in colnames(Boston)[-1]){
  print(var)
  model = as.formula(paste0('crim ~ I(',var,'^3) + I(',var,'^2) + ',var))
  lm.poly = lm(model,data=Boston)
  print(summary(lm.poly)$coefficients)
}