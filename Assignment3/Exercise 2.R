###EXERCISE 2
search=read.table("search.txt",header = TRUE);
attach(search)
##T1

N=1
I=5
B=3

for (i in 1:B) print(sample(1:(N*I)))
#gotta do the randomization again but still don't get how that works 
#like in the last exercise



##T2
interaction.plot(skill,interface,time)
interaction.plot(interface,skill,time)

#The lines seems unparallel which indicates that there might be interaction

##T3

search$skill = as.factor(search$skill)
search$interface = as.factor(search$interface)

searchaov = lm(time~interface+skill,data=search)
anova(searchaov)

anova(searchaov)[[5]][1]
#H0 mu0=mu1=mu2=mu3=mu4 can be rejected iwht a p-value of 0.01310.
#So the time is not the same for all the interfaces.


##T4
#Lecture 6 (3)

library(multcomp)
searchaov2 = lm(time~interface+skill,data=search)

searchmult = glht(searchaov2,linfct=mcp(skill="Tukey"))
summary(searchmult)

summary(searchmult)

#According to the test 4 -3 == 0 has an estimate of 2.267. That's the estimte time for a user of skill 4 with the interface 3.

##T5
#Lecture 5  (26-27)

qqnorm(residuals(searchaov2))
qqline(residuals(searchaov2))
#The QQ-plot seems a bit deviated in the extremes but it could be normal

plot(fitted(searchaov2),residuals(searchaov2))

#We see that the residuals don't change systematically with the fitted values.
#So we can assume that the population comes from where the data comes from has equal variances.

##T6
#Lecture 6 (31)
friedman.test(search$time,search$interface,search$skill)
#We reject H0 (Interface doesn't have an effect) so we can say that the interface makes an effect.

##T7
search$interface = as.factor(search$interface)

searchaov = lm(time~interface,data=search)

anova(searchaov)

#According to p-value we cannot reject that the means are the same for the different interfaces.
#It is not useful because the variable skill should also be considered

#1-way-anova assumes that the data come from normal population and the variances are equal.
qqnorm(residuals(searchaov))
qqline(residuals(searchaov))
#The population doesn't seem normal therefore the assumption is not met.