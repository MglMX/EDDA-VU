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



#T6



#T7



#T8



#T9



#T10

