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

hist(fliesframe$longevity)
qqnorm(fliesframe$longevity)
qqline(fliesframe$longevity)
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

fliesanova2 = lm(longevity~activity+thorax,data = fliesframe) #Should I use * or +
anova(fliesanova2)

#According to the p-value < 0.05 activity it has a maiun effect on longevity

#The same using *
fliesanova2 = lm(longevity~activity*thorax,data = fliesframe) #Should I use * or +
anova(fliesanova2)

#According to the p-value < 0.05 activity it has a maiun effect on longevity

#T6
contrasts(fliesframe$thorax)=contr.sum
contrasts(fliesframe$activity)=contr.sum
summary(fliesanova2)

#According to the estimates, too much sexual activity (high) doesn't increase longevity as much as low sexual activity. Isolated flies die faster.
#Estimates:
#Activity1(isolated) = -14.05
#Activity2(low) =  14.11
#Activity3(high)= -(-14.05+14.11) = -0.6
#! Not sure that activity1 correspond to isolated, activity2 to low and activity3 to high

fliesframe$thorax = as.numeric(fliesframe$thorax)
average_thorax = mean(fliesframe$thorax) #Obatining mean for the thorax
average_thorax #8.87
fliesframe$thorax = as.factor(fliesframe$thorax)


#Estimates:
#Activity1(isolated):thorax8 = -0.28
#Activity2(low):thorax8 = -9.44
#Activity3(high):thorax8 = -(-0.28-9.44) = 9.72
#! Not sure that activity1 correspond to isolated, activity2 to low and activity3 to high
#! Also not sure thorax8 means that those are the flies with thorax length 8 (which is the average)

#T7



#T8



#T9



#T10

