###EXERCISE 1

##T1

bread = read.table("bread.txt", header=TRUE)
attach(bread)

N=3
I=3
J=2

rbind(rep(1:I,each=N*J),rep(1:J,N*I),sample(1:(N*I*J)))

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

#Environment and humidity have main effect according to the p-value. 
#Looking at the p-value of humidity:environment we see that there is interaction between the two factors



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

