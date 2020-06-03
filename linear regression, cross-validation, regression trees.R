# Regression analysis of the Boston housing data

rm(list=ls())
library("MASS")
library(ggplot2)
library(gridExtra)

# load the Boston housing dataset
help("Boston")
data_house = Boston

# let's look at the response variable
plot(data_house$medv, pch=16)
summary(data_house$medv)

# a little bit of EDA (scatterplots)
# the response variable should always be in the y-axis
ggplot(data_house, aes (y=medv,x=crim)) + geom_point() +
  stat_smooth(method="lm")
data_house1 = subset(data_house, crim < 25)
ggplot(data_house1, aes (y=medv,x=crim)) + geom_point() +
  stat_smooth(method="lm")
logcrim = log(data_house1$crim)
data_house1 = cbind(data_house1, logcrim)
ggplot(data_house1, aes (y=medv,x=logcrim)) + geom_point() +
  stat_smooth(method="lm")

ggplot(data_house, aes (y=medv,x=nox)) + geom_point() +
  stat_smooth(method="lm")

ggplot(data_house, aes (y=medv,x=age)) + geom_point() +
  stat_smooth(method="lm")

ggplot(data_house, aes (y=medv,x=tax)) + geom_point() +
  stat_smooth(method="lm")
data_house2 = subset(data_house, tax < 450)
ggplot(data_house2, aes (y=medv,x=tax)) + geom_point() +
  stat_smooth(method="lm")

# Let's look at correlations
cormat = round(cor(data_house),2)

# let's move to actually fitting the regression model
fit1 = lm(medv~lstat, data = data_house)
summary(fit1)

# R^2 is approximately cor(x,y)^2
cor(data_house$medv, data_house$lstat)
(cor(data_house$medv, data_house$lstat))^2

# fit model with age only
fit2 = lm(medv~age, data = data_house)
summary(fit2)

# fit model with age only
fit3 = lm(medv~nox, data = data_house)
summary(fit3)

# fit model with both age and nox
fit23 = lm(medv~age+nox, data = data_house)
summary(fit23)

# how to find the best possible model?
fit.all = lm(medv~.,data=data_house)
summary(fit.all)

# forward and backward feature selection
?stepAIC
fit0 = lm(medv~1, data = data_house)
summary(fit0)

foo = stepAIC(fit0, scope = list(lower=fit0, upper = fit.all),
              direction = "both")
fit.best = lm(formula(foo), data=data_house)
summary(fit.best)

# Boston Housing data
rm(list=ls())
library("MASS")
help("Boston")

# how to fit the best possible model?
data_house = Boston
fit0 = lm(medv~1,data=data_house)
fit.all = lm(medv~.,data=data_house)

# model selection via forward and backward variable selection
foo = stepAIC(fit0, scope = list(lower=fit0, upper=fit.all),
              direction = "both")

fit.best = lm(formula(foo), data = data_house)
summary(fit.best)

# Linear Regression diagnostics

# 1. Scatter plots of y vs X1, X2, ...
plot(data_house$lstat, data_house$medv, pch=16)

# 2. Are residuals normally distributed?
# Q-Q plots
res=fit.best$residuals
qqnorm(res)
qqline(res)

#3. Are residuals unrelated to fitted values?
fitted = predict(fit.best)
plot(fitted,res, pch=16)

#4. Leverage plot (influential observations)
lev = hat(model.matrix(fit.best))
plot(fitted, lev, pch=16)

#5. Cook's distance (influential observations)
cooks = cooks.distance(fit.best)
plot(fitted, cooks, pch=16)

# Decision Trees
install.packages("tree")
library(tree)

# Cross-validation error
# Split our data into training set and test set
trainpct = 0.75
n = nrow(data_house)
train = sample(1:n, round(n*trainpct))
data_train = data_house[train, ]
data_test = data_house[-train, ]

# regression tree
tree.boston = tree(medv~.,data_train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty=0)

# cross-validation

# let's look at linear regression first

# how to fit the best possible model?
fit0 = lm(medv~1,data=data_train)
fit.all = lm(medv~.,data=data_train)
# model selection via forward and backward variable selection
foo = stepAIC(fit0, scope = list(lower=fit0, upper=fit.all),
              direction = "both")
fit.best = lm(formula(foo), data = data_train)

yhat1 = predict(fit.best, newdata = data_test)
y.test = data_test$medv
plot(yhat1,y.test, pch=16)
abline(0,1)


# regression tree
yhat2 = predict(tree.boston, newdata = data_test)
plot(yhat2,y.test, pch=16)
abline(0,1)

# let's compare the cross-validation errors
mean((yhat1-y.test)^2) # linear regression
mean((yhat2-y.test)^2) # regression tree

# 
















