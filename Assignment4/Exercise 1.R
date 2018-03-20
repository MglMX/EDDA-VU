#Exercise 1
flies = read.table("fruitflies.txt",header=TRUE);

#T1

fliesframe = data.frame(thorax=flies$thorax,longevity=flies$longevity,activity=flies$activity,loglongevity=log(flies$longevity))
fliesframe

#T2
hist(fliesframe$thorax)
qqnorm(fliesframe$thorax)
qqline(fliesframe$thorax)
shapiro.test(fliesframe$thorax)
#Not normal

hist(fliesframe$loglongevity)
qqnorm(fliesframe$loglongevity)
qqline(fliesframe$loglongevity)
shapiro.test(fliesframe$longevity)
#Doubtful normality. Probably not normal but shapiro says could be normal

hist(fliesframe$loglongevity)
qqnorm(fliesframe$loglongevity)
qqline(fliesframe$loglongevity)
shapiro.test(fliesframe$loglongevity)
#Not normal


#T3
#Performing a 1-way anova Lecture 4 (24)
#I don't know if randomization has to be done first
fliesanova = lm(longevity~activity,data=fliesframe)
anova(fliesanova)

#We get a p-value < 0.05 therefore H0(mu0==mu1==mu2) is reject. We can say that the sexual activity affects longevity


#T4
summary(fliesanova)

#According to the estimates longevity increases with sexual activity. 
# High activity 38.72
# Low activity 38.72 - 24.84 = 3.94
# Isolated 38.72 - 18 = 20.72

#T5
fliesframe$activity = as.factor(fliesframe$activity)
fliesframe$thorax = as.factor(fliesframe$thorax)

fliesanova2 = lm(loglongevity~activity+thorax,data = fliesframe) #Should I use * or +
anova(fliesanova2)

#According to the p-value < 0.05 activity it has a maiun effect on longevity

#The same using *
fliesanova2 = lm(loglongevity~activity*thorax,data = fliesframe) #Should I use * or +
anova(fliesanova2)

#According to the p-value < 0.05 activity it has a maiun effect on longevity. And there is no interaction between activity and thorax

#T6
contrasts(fliesframe$thorax)=contr.sum
contrasts(fliesframe$activity)=contr.sum
summary(fliesanova2)

#According to the estimates, too much sexual activity (high) doesn't increase longevity as much as low sexual activity. Isolated flies die faster.
#Estimates:
#Activity1(isolated) = -0.25
#Activity2(low) =  0.23
#Activity3(high)= -(-0.25+0.23) = 0.02
#! Not sure that activity1 correspond to isolated, activity2 to low and activity3 to high


#Average thorax
fliesframe$thorax = as.numeric(fliesframe$thorax)
average_thorax = mean(fliesframe$thorax) #Obatining mean for the thorax
average_thorax #8.87
fliesframe$thorax = as.factor(fliesframe$thorax)


#Estimates:
#Activity1(isolated):thorax8 = -0.13
#Activity2(low):thorax8 = -0.09
#Activity3(high):thorax8 = -(-0.13-0.09) = 0.22
#! Not sure that activity1 correspond to isolated, activity2 to low and activity3 to high
#! Also not sure thorax8 means that those are the flies with thorax length 8 (which is the average)

#Smallest thorax
fliesframe$thorax = as.numeric(fliesframe$thorax)
min_thorax = min(fliesframe$thorax) #Obatining mean for the thorax
min_thorax #1
fliesframe$thorax = as.factor(fliesframe$thorax)

#Estimates:
# Activity1(isolated):thorax1 = -0.35
# Activity2(low):thorax1 = NA
# Activity3(high):thorax1 = 0.35


#T7
contrasts(fliesframe$thorax)=contr.sum
contrasts(fliesframe$activity)=contr.sum
fliesanova2 = lm(loglongevity~thorax+activity,data = fliesframe)
summary(fliesanova2)


#According to the estimates, longevity increases with thorax length from 1 to 11. From 12 to 13 it decreases a bit the longevity

fliesframe$thorax = as.numeric(fliesframe$thorax)
flieshigh = fliesframe[which(fliesframe$activity=="high"),]
flieshigh

fliesisolated = fliesframe[which(fliesframe$activity=="isolated"),]
fliesisolated

flieslow = fliesframe[which(fliesframe$activity=="low"),]
flieslow

#Probably not graphically relevant...
  
plot(flieshigh$thorax,flieshigh$loglongevity,main="High activity with loglongevity")
plot(fliesisolated$thorax,fliesisolated$loglongevity,main="Isolated activity with loglongevity")
plot(flieslow$thorax,flieslow$loglongevity,main="Low activity with loglongevity")

fliesframe$thorax = as.factor(fliesframe$thorax)
boxplot(loglongevity~thorax+activity,data=fliesframe)

interaction.plot(fliesframe$thorax,fliesframe$activity,fliesframe$loglongevity)

plot(fliesframe$loglongevity~fliesframe$thorax,pch=as.character(fliesframe$activity))
#T8

#Not sure about this answer

#The analysis with the thorax length should not be included because there should not be interaction between the sexual activity and the thorax length. This is because the experimenters randomly chose the sexual activity that the flies were going to have.


#T9
qqnorm(residuals(fliesanova2))
qqline(residuals(fliesanova2))
#The normality s doubtful

plot(fitted(fliesanova2),residuals(fliesanova2))
#The spread in the residuals seem to be bigger with bigger fitted values.


#T10
fliesframe$activity=as.factor(fliesframe$longevity)
fliesaov = lm(longevity~thorax+activity,data=fliesframe)
drop1(fliesaov,type="F")

qqnorm(residuals(fliesanova))
qqline(residuals(fliesanova))
#The normality s doubtful. But it could be normal. It seems more normal than when using the logaritmic value of longevity

plot(fitted(fliesanova),residuals(fliesanova))
#We cannot judge becasue the values are not spread in the x axis.
