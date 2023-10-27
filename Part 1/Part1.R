#########################################################
# R script: Part1.R
# Project: Part 1
#
# Date: 09/08/2023
# Author: Harshath Muruganantham
#
#########################################################

# 2.1
0.235+0.117+0.058+0.178

#2.2
(0.235+0.058)/(0.176+0.235+0.117+0.058)

#2.3
(0.117+0.178)/(0.060+0.117+0.059+0.178)

#2.5
(0.178*0.0600) + (0.059+0.117) + (0.178 * 0.117)

#3.1
expectedValue = 2 * (sum(1,2,3,4,5,6) * 1/6) - (sum(1,2,3,4)*1/4)

#3.2
variance = 0
for (i in 1:6) {
  variance = variance + ((i - (sum(1,2,3,4,5,6) * 1/6))^2 * 1/6)
}
variance = variance * 4

for (i in 1:4) {
  variance = variance + ((i - (sum(1,2,3,4)*1/4))^2 * 1/4)
}
print(variance)

#3.3
retval = list(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for (x in 1:6){
  for (y in 1:4){
    retval[2*x-y + 3] = retval[2*x-y + 3][[1]] + 1/6*1/4
  }
}
print(retval)

#3.4
eVal = 0
for (i in -2:11){
  eVal = eVal + (i^3 * retval[i+3][[1]])
}

print(eVal)

#3.5
taylorExpansion = (expectedValue)^3 + (variance/2)*(6*expectedValue)

#3.6
finalOut = list(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for (x in 1:6){
  for (y in 1:4){
    for (z in 1:4){
      finalOut[2*x-y + 2*z+1] = finalOut[2*x-y + 2*z+1][[1]] + 1/6*1/4*1/4
    }
  }
}

eVal_Final = 0
for (i in 0:19){
  eVal_Final = eVal_Final + (i^2 * finalOut[i+1][[1]])
}

print(eVal_Final)


#4.1

p = function(x) {
  (x >= 0 & x <= s) * (2*(s-x))/(s^2) + (x<0 & x > s)
}

s = 1
curve(p(x), from = 0, to = 3)
title("Probability Density Function of X when s = 1")
legend("topright","Probability Density Function of X when s = 1", cex = 0.65)

s = 2
curve(p(x), from = 0, to = 3, ylim = c(0,2))
title("Probability Density Function of X when s = 2")
legend("topright","Probability Density Function of X when s = 2", cex = 0.65)


#5.1
covid = read.csv("covid.2023.csv", header = TRUE)
ml = sum(covid$Days)/length(covid$Days)
print(ml)

#5.2
prob52 = ppois(10,ml)
print(prob52)



x = min(covid$Days):max(covid$Days)
y = dpois(x,ml)
likelydays = data.frame(days = x, likelihood = y)
likelydays = likelydays[order(likelydays$likelihood,decreasing = TRUE),]
likelydays[1:3,]

#Recall that ml is the variable created in part 2a.
#The value of ml is 15.556
ppois(80,ml*5) - ppois(59,ml*5)

#Model this question as a binomial distribution question where the 
#probability of success is that 'One Person recovers on or after day 14'.
#Then find the probability for the probability of 3, 4 and 5 successes in 5 trials.

#Finding the probability for one person to recover on or after day 14.
PronePersonToRecover = 1- ppois(13, ml)

#Finding the probability that 3 or more of 5 patients will recover on or after day 14
sum(dbinom(c(3,4,5), 5, PronePersonToRecover))





#5.3
proportions = c()
for (days in 0:40){
 proportions = append(proportions, (length(which(covid$Days == days)))/length(covid$Days))
}
dataSet_Proportions = data.frame(Days = 0:40, Proportions = proportions)
plot(dataSet_Proportions$Days, dataSet_Proportions$Proportion, xlim = c(0,40), ylim = c(0,dpois(round(ml),ml)), xlab = "Days", ylab = "Proportion or Probability")
lines(dataSet_Proportions$Days, dpois(dataSet_Proportions$Days,ml), col = "red")
title("Comparison of the modelled poisson distribution and the proportion of days-to-recover")
legend("topright",c("Proportion","Poisson Distribution"), lty=c(0,1), pch=c("o",""), col=c("black","red"), lwd=c(1,2.5))


