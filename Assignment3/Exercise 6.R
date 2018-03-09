###EXERCISE 6

pollution = read.table("airpollution.txt",header=TRUE)

##T1
pairs(pollution)

#There seem to be correlation between wind and humidity, Temperature and oxidant.
#There seem to be negative correlation between wind and temperature, temperature and humidity, wind and oxidant.


##T2

#First we perform the test on each of the explanatory variables to get the best.
poplm = lm(oxidant~wind,data=pollution)
summary(poplm)
qqnorm(residuals(poplm))
qqline(residuals(poplm))
plot(fitted(poplm),residuals(poplm))
#p-value 8.20e-07 and the assumptions seem true according to qqplot and plot

poplm = lm(oxidant~temperature,data=pollution)
summary(poplm)
qqnorm(residuals(poplm))
qqline(residuals(poplm))
plot(fitted(poplm),residuals(poplm))
#p-value 1.17e-06 and the assumptions seem true according to qqplot and plot

poplm = lm(oxidant~humidity,data=pollution)
summary(poplm)
qqnorm(residuals(poplm))
qqline(residuals(poplm))
plot(fitted(poplm),residuals(poplm))
#p-value 0.056 and the assumptions are doubtful.

poplm = lm(oxidant~insolation,data=pollution)
summary(poplm)
qqnorm(residuals(poplm))
qqline(residuals(poplm))
plot(fitted(poplm),residuals(poplm))
#p-value 0.004 and assumptions are doubtful

#The variable with lower p-value is wind
#The linear regression model could be: Yn = 45.31 - 0.63*wind


poplm = lm(oxidant~wind+temperature,data=pollution)
summary(poplm)
qqnorm(residuals(poplm))
qqline(residuals(poplm))
plot(fitted(poplm),residuals(poplm))
#The assumptions seem true
#The linear regression model could be: Yn = -5.2 - 0.43*wind + 0.52*temperature

poplm = lm(oxidant~wind+temperature+insolation,data=pollution)
summary(poplm)
qqnorm(residuals(poplm))
qqline(residuals(poplm))
plot(fitted(poplm),residuals(poplm))
#Insolation doesn't seem to be correlated. so this extension is not useful

##T3
poplm = lm(oxidant~wind+temperature+humidity+insolation,data=pollution)
summary(poplm)
qqnorm(residuals(poplm))
qqline(residuals(poplm))
plot(fitted(poplm),residuals(poplm))
#According to the p-value, we should decrease the full model not considering humidity and insolation.
#The result linear model would be: Yn = -15.49 - 0.44*wind + 0.56*temperature




##T4
#Estimates: wind = 0.43, temperature = 0.52, humidity = 0, insolation = 0;


##T5
poplm = lm(oxidant~wind+temperature,data=pollution)
qqnorm(residuals(poplm))
qqline(residuals(poplm))

#According to the qqplot the residuals seem normal.