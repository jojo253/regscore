library(lars)
library(fastDummies)

data0=read.csv('score.csv')
data=na.omit(data0)
y=data$level
X=data[,c(-1,-3)]
X=dummy_cols(X,select_columns = c('class','term'))[,c(-2,-4)]
X=as.matrix(X)
fit=lm(y~X)
summary(fit)

fit1=lars(as.matrix(X),as.matrix(y),type = "lasso")
plot(fit1)
# term and weight are not important
fit=lm(level~.,data=data[,3:5])
summary(fit)

predict(fit,data0)
View(cbind(data0,predict(fit,data0),predict(fit,data0)<data0$level))