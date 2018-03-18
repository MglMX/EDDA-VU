###EXERCISE 1
library(multcomp)
library(lme4)
##T1

bread = read.table("bread.txt", header=TRUE)
attach(bread)

N=3
I=2
B=3

for (i in 1:B) print(sample(1:(N*I)))

#This way we can see hwo to assign to the slices of bread the different combinations number from 1 to 6. 

##T2

boxplot(hours~environment,data=bread)
boxplot(hours~humidity,data=bread)

interaction.plot(humidity,environment,hours)
interaction.plot(environment,humidity,hours)

##T3

bread$humidity=as.factor(bread$humidity)
bread$environment=as.factor(bread$environment)
breadaov=lm(hours~humidity*environment,data=bread)
anova(breadaov)

anova(breadaov)[[5]][1] #p-value humidity
anova(breadaov)[[5]][2] #p-value environment
anova(breadaov)[[5]][3] #p-value humidity:environment

#Environment and humidity have main effect according to the p-value. 
#Looking at the p-value of humidity:environment we see that there is interaction between the two factors.
#So it means that the hours it takes to get the bread to decay is influence by the combination of this two factor and its combinations.



##T4

contrasts(bread$humidity)=contr.sum
contrasts(bread$environment)=contr.sum
breadaov2=lm(hours~humidity*environment,data=bread)
summary(breadaov2)


##T5

qqnorm(residuals(breadaov2))
qqline(residuals(breadaov2))
plot(fitted(breadaov2),residuals(breadaov2))

#The normality according to the qq-plot could be doubtful because of the extreme values.
#In the plot we see that the values don't change systematically. We can see some outliers between 200 and 300.

