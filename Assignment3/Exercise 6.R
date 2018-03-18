###EXERCISE 6

pollution = read.table("airpollution.txt",header=TRUE)

##T1
pairs(pollution)

#There seem to be correlation between wind and humidity, Temperature and oxidant.
#There seem to be negative correlation between wind and temperature, temperature and humidity, wind and oxidant.

round(cor(pollution[,2:6]),2)

##T2

#First we perform the test on each of the explanatory variables to get the best.
poplm = lm(oxidant~wind,data=pollution)
summary(poplm)
#R2 = 0.59

poplm = lm(oxidant~temperature,data=pollution)
summary(poplm)
#R2 = 0.58

poplm = lm(oxidant~humidity,data=pollution)
summary(poplm)
#R2 = 0.12

poplm = lm(oxidant~insolation,data=pollution)
summary(poplm)
#R2 = 0.26

#The variable that gives higher R2 is wind 
#We add another variable

poplm = lm(oxidant~wind+temperature,data=pollution)
summary(poplm)
#R2 = 0.78

poplm = lm(oxidant~wind+humidity,data=pollution)
summary(poplm)
#R2 = 0.59

poplm = lm(oxidant~wind+insolation,data=pollution)
summary(poplm)
#R2 = 0.66

#The one with higer value is wind + temperature with R2 of 0.78
poplm = lm(oxidant~wind+temperature+humidity,data=pollution)
summary(poplm)
#R2 = 0.79

poplm = lm(oxidant~wind+temperature+insolation,data=pollution)
summary(poplm)
#R2 = 0.78

#None of these are relevant for hte linear regression.
#The final linear regression model could be: Yn = -5.0 -0.43*wind + 0.52*temperature


##T3
poplm = lm(oxidant~wind+temperature+humidity+insolation,data=pollution)
summary(poplm)
#The one with highest p-value is insolation
#We delete insolation from the model

poplm = lm(oxidant~wind+temperature+humidity,data=pollution)
summary(poplm)
#The one with highest p-value is humidity.
#We delete it from the model

poplm = lm(oxidant~wind+temperature,data=pollution)
summary(poplm)

#The final model could be Yn = -5.0 -0.43*wind + 0.52*temperature

##T4
#Estimates: wind = 0.43, temperature = 0.52, humidity = 0, insolation = 0;


##T5
poplm = lm(oxidant~wind+temperature,data=pollution)
qqnorm(residuals(poplm))
qqline(residuals(poplm))

#According to the qqplot the residuals seem normal.