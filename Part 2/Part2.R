#########################################################
# R script: Part2.R
# Project: Part 2
#
# Date: 09/09/2023
# Author: Harshath Muruganantham
#
# Purpose: Answer script for Part 2
#########################################################
setwd("~/Documents/University/FIT2086/Assignment 2/")
rm(list=ls())

print("Question 1:")
covid = read.csv("covid.19.ass2.2023.csv", header = TRUE)

covid_mean = mean(covid$Recovery.Time)
print("Mean of NSW covid is:")
print(covid_mean)
covid_variance = var(covid$Recovery.Time)
print(var(covid$Recovery.Time))

t_value = qt(p=1-0.05/2, df=length(covid$Recovery.Time) - 1)
print(qt(p=1-0.05/2, df=length(covid$Recovery.Time) - 1))

print("Low CI of NSW COVID:")
print(covid_mean - t_value * (sqrt(covid_variance) / sqrt(length(covid$Recovery.Time))))
print("High CI of NSW COVID:")
print(covid_mean + t_value * (sqrt(covid_variance) / sqrt(length(covid$Recovery.Time))))

israeli_covid = read.csv("israeli.covid.19.ass2.2023.csv", header = TRUE)


iscovid_mean = mean(israeli_covid$Recovery.Time)
print(mean(israeli_covid$Recovery.Time))
iscovid_var = var(israeli_covid$Recovery.Time)
print(var(israeli_covid$Recovery.Time))

diff_mean = mean(israeli_covid$Recovery.Time) - mean(covid$Recovery.Time)
print("Difference of mean is:")
print(diff_mean)

z_value = qnorm(1-0.05/2)

print("Low CI of Difference of Mean is:")
print(diff_mean - z_value * (sqrt( (covid_variance/length(covid$Recovery.Time)) + (iscovid_var/length(israeli_covid$Recovery.Time)) )))
print("High CI of Difference of Mean is:")
print(diff_mean + z_value * (sqrt( (covid_variance/length(covid$Recovery.Time)) + (iscovid_var/length(israeli_covid$Recovery.Time)) )))

z_hypothesisTesting = (iscovid_mean - covid_mean)/(sqrt( (covid_variance/length(covid$Recovery.Time)) + (iscovid_var/length(israeli_covid$Recovery.Time)) ))
print((sqrt( (covid_variance/length(covid$Recovery.Time)) + (iscovid_var/length(israeli_covid$Recovery.Time)) )))
print((iscovid_mean - covid_mean)/(sqrt( (covid_variance/length(covid$Recovery.Time)) + (iscovid_var/length(israeli_covid$Recovery.Time)) )))

print("Hypothesis Question p-value:")
print(2 * pnorm(-z_hypothesisTesting))
2 * pnorm(-1.3814)


#Question 2
y= seq(0,10, 0.01)

v=0.5
plot(y, exp(-exp(-v)*y -v), xlab = "y", ylab = "Exponential Probability Density Function - P(y|v)", type = 'l')
v=1
lines(y, exp(-exp(-v)*y -v), col = "red")
v=2
lines(y, exp(-exp(-v)*y -v), col = "blue")
title("Exponential Probability Density Function of y under various v values")
legend("topright",c("Exp pdf of y given v = 0.5","Exp pdf of y given v = 1", "Exp pdf of y given v = 2"), lty=1, col=c("black","red", "blue"))





#Question 3
print("")
print("Question 3:")
print("Preference is:")
p_bernoulli = 80/124
print(p_bernoulli)
v_p_bernoulli = p_bernoulli*(1-p_bernoulli)
print(p_bernoulli*(1-p_bernoulli))

print("Low CI of Preference is:")
print(p_bernoulli - 1.96 * sqrt(v_p_bernoulli/124))
print("High CI of Preference is:")
print(p_bernoulli + 1.96 * sqrt(v_p_bernoulli/124))

z_value_bernoulli = (p_bernoulli - 0.5)/sqrt(0.5 * (1-0.5)/124)
(p_bernoulli - 0.5)
sqrt(0.5 * (1-0.5)/124)
(p_bernoulli - 0.5)/sqrt(0.5 * (1-0.5)/124)

print("p-value of Preference is:")
print(2 * pnorm(-abs(z_value_bernoulli)))
2*pnorm(-3.2329)

binom.test(x = 80, n = 124, p = 0.5)

thetahat_p = (80 + 83)/(124 + 100)
(80 + 83)/(124 + 100)

p_bernoulli_two = 83/100

z_twoBpop = (p_bernoulli - p_bernoulli_two)/sqrt(thetahat_p * (1 - thetahat_p) * (1/124 + 1/100))
(0.83 - 0.6452)/sqrt(0.7277 * (1 - 0.7277) * (1/124 + 1/100))


print('p-value for the two Bernoulli populations is:')
print(2 * pnorm(-abs(z_twoBpop)))

2 * pnorm(-3.0888)

