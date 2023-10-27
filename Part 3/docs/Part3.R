#########################################################
# R script: Part 3.R
# Project: Part 3
#
# Date: 09/09/2023
# Author: Harshath Muruganantham
#
# Purpose: Answer script for Part 3
#########################################################


rm(list=ls())

library(pROC)
library(rpart)
library(boot)
library(kknn)

source("wrappers.R")
source("my.prediction.stats.R")


#Question 1
housing = read.csv("housing.2023.csv", header = TRUE)

#1.1
fit = lm(medv ~., housing)
summary(fit)
# all with a *, **, and ** are associated with * being only borderline associated
# lstat, ptratio, and rm seem to be the 3 most strongest predictors.

#1.2
bonferroni_correction = 0.05/(length(housing)-1)
cat("Bonferroni Correction is:", bonferroni_correction)
#Only p values less than or equal to bonferroni_correction should be accepted now

#1.3
fit$coefficients["crim"]
fit$coefficients["chas"]

#1.4 
fit_bic = step(fit, k = log(length(housing$medv)), direction = "both")
summary(fit_bic)

#1.5
fit_bic$coefficients

#1.6
table2 <- data.frame(crim=0.04741, 
                 zn=0, 
                 indus=11.93, 
                 chas=0, 
                 nox=0.573, 
                 rm=6.03, 
                 age=80.8, 
                 dis=2.505,
                 rad=1, 
                 tax=273,
                 ptratio=21, 
                 lstat=7.88
                 )

yhat_1.6 = predict(fit_bic, table2)
yhat_1.6
yhat_1.6_CI = predict(fit_bic, table2, interval="confidence")
yhat_1.6_CI

#1.7
fit_interaction = lm( medv ~ chas + nox + rm + dis + ptratio + lstat + dis*rm, data=housing)
summary(fit_interaction)
summary(fit_bic)



#Question 2
heart_train = read.csv("heart.train.2023.csv", stringsAsFactors = T)
heart_test = read.csv("heart.test.2023.csv", stringsAsFactors = T)

#2.1
tree.heart = rpart(HD ~ ., heart_train)
tree.heart
cv = learn.tree.cv(HD ~.,data=heart_train, nfolds = 10, m = 5000)
cv$best.tree

#2.2
plot(cv$best.tree)
text(cv$best.tree,pretty=12)

#2.3
plot(cv$best.tree)
text(cv$best.tree,pretty=12)
cv$best.tree

#2.5
fullmod=glm(HD ~ ., data = heart_train, family = binomial)
step.fit.bic = step(fullmod, k = log(nrow(heart_train)), direction = "both")
summary(step.fit.bic)

#2.7
my.pred.stats(predict(cv$best.tree,heart_test)[, 2], heart_test$HD)
my.pred.stats(predict(step.fit.bic,heart_test,type="response"), heart_test$HD)

#2.8
predict(cv$best.tree,heart_test[69,])
predict(step.fit.bic,heart_test[69,],type="response")
heart_test[69,]

#2.9
boot.prob = function(formula, data, indices, test_data)
{
  # Create a bootstrapped version of our data
  d = data[indices,]
  
  # Fit a logistic regression to the bootstrapped data
  fit = glm(formula, d, family=binomial)
  
  # Compute the prediction and return it
  rv = predict(fit,heart_test[69,],type="response")
  return(rv)
}
bs = boot(data=heart_test, statistic=boot.prob, R=5000, formula=step.fit.bic$formula)
boot.ci(bs,conf=0.95,type="bca")


#Question 3
ms.measured = read.csv("ms.measured.2023.csv", header = TRUE)
ms.truth = read.csv("ms.truth.2023.csv", header = TRUE)

#3.1
MSE = c()
for (k in 1:25) {
  ytest.hat = fitted( kknn(intensity ~ ., ms.measured, ms.truth, k = k, kernel = "optimal") )
  MSE = append(MSE, mean( (ytest.hat - ms.truth$intensity)^2))
}
plot(c(1:25), MSE, xlab = "k", ylab = "Mean-Squared Error", pch = 20)
title("Mean Squared Error against various values of k")

#3.2
k = 2
fitted_2 = fitted( kknn(intensity ~ ., ms.measured, ms.truth, k = k, kernel = "optimal") )
plot(ms.measured$MZ, ms.measured$intensity, pch = 20, cex = 0.5, xlab = "Mass/Charge (MZ)", ylab = "Relative Intensity")
lines(ms.truth$MZ, ms.truth$intensity, col = "red", cex = 0.5) 
lines(ms.truth$MZ, fitted_2, col = "blue", cex = 0.5)
title("Relative Intensity against Mass/Charge (MZ) for k = 2")
legend("topright", c("Training Data Points", "True Spectrum", "Estimated Spectrum"), lty = c(0, 1, 1), pch = c(20, NA, NA), col=c("black","red", "blue"))

k = 5
fitted_5 = fitted( kknn(intensity ~ ., ms.measured, ms.truth, k = k, kernel = "optimal") )
plot(ms.measured$MZ, ms.measured$intensity, pch = 20, cex = 0.5, xlab = "Mass/Charge (MZ)", ylab = "Relative Intensity")
lines(ms.truth$MZ, ms.truth$intensity, col = "red", cex = 0.5) 
lines(ms.truth$MZ, fitted_5, col = "blue", cex = 0.5)
title("Relative Intensity against Mass/Charge (MZ) for k = 5")
legend("topright", c("Training Data Points", "True Spectrum", "Estimated Spectrum"), lty = c(0, 1, 1), pch = c(20, NA, NA), col=c("black","red", "blue"))

k = 10
fitted_10 = fitted( kknn(intensity ~ ., ms.measured, ms.truth, k = k, kernel = "optimal") )
plot(ms.measured$MZ, ms.measured$intensity, pch = 20, cex = 0.5, xlab = "Mass/Charge (MZ)", ylab = "Relative Intensity")
lines(ms.truth$MZ, ms.truth$intensity, col = "red", cex = 0.5) 
lines(ms.truth$MZ, fitted_10, col = "blue", cex = 0.5)
title("Relative Intensity against Mass/Charge (MZ) for k = 10")
legend("topright", c("Training Data Points", "True Spectrum", "Estimated Spectrum"), lty = c(0, 1, 1), pch = c(20, NA, NA), col=c("black","red", "blue"))

k = 25
fitted_25 = fitted( kknn(intensity ~ ., ms.measured, ms.truth, k = k, kernel = "optimal") )
plot(ms.measured$MZ, ms.measured$intensity, pch = 20, cex = 0.5, xlab = "Mass/Charge (MZ)", ylab = "Relative Intensity")
lines(ms.truth$MZ, ms.truth$intensity, col = "red", cex = 0.5) 
lines(ms.truth$MZ, fitted_25, col = "blue", cex = 0.5)
title("Relative Intensity against Mass/Charge (MZ) for k = 25")
legend("topright", c("Training Data Points", "True Spectrum", "Estimated Spectrum"), lty = c(0, 1, 1), pch = c(20, NA, NA), col=c("black","red", "blue"))

#3.3
MSE_2 = mean( (fitted_2 - ms.truth$intensity)^2 )
cat("MSE for k = 2 is: ", MSE_2)

MSE_5 = mean( (fitted_5 - ms.truth$intensity)^2 )
cat("MSE for k = 5 is: ", MSE_5)

MSE_10 = mean( (fitted_10 - ms.truth$intensity)^2 )
cat("MSE for k = 10 is: ", MSE_10)

MSE_25 = mean( (fitted_25 - ms.truth$intensity)^2 )
cat("MSE for k = 25 is: ", MSE_25)

#3.5
knn = train.kknn(intensity ~ ., data = ms.measured, kmax=25, kernel="optimal")
knn$best.parameters$k
min(MSE)
which.min(MSE)

#3.6
fitted_3.6 = fitted( kknn(intensity ~ ., ms.measured, ms.truth, k = knn$best.parameters$k, kernel = "optimal") )
error = abs(ms.measured$intensity - fitted_3.6)
sd(error)

#3.7
fitted_3.7 = fitted( kknn(intensity ~ ., ms.measured, ms.truth, k = knn$best.parameters$k, kernel = "optimal") )
ms.truth[which.max(fitted_3.7),]

#3.8
boot.kknn = function(data, indices, k, mz)
{
  # Create a bootstrapped version of our data
  d = data[indices,]
  
  # Compute the prediction and return it
  rv = fitted( kknn(intensity ~ ., d, ms.truth[ms.truth$MZ == mz,], k = k, kernel = "optimal") )
  return(rv)
  
}
bs_bestk = boot(data = ms.measured, statistic=boot.kknn, R=5000, k=knn$best.parameters$k, mz = ms.truth[which.max(fitted_3.7),]$MZ)
boot.ci(bs_bestk,conf=0.95, type = "bca")

bs_3 = boot(data = ms.measured, statistic=boot.kknn, R=5000, k= 3, mz = ms.truth[which.max(fitted_3.7),]$MZ)
boot.ci(bs_3,conf=0.95, type = "bca")

bs_20 = boot(data = ms.measured, statistic=boot.kknn, R=5000, k=20, mz = ms.truth[which.max(fitted_3.7),]$MZ)
boot.ci(bs_20,conf=0.95, type = "bca")


