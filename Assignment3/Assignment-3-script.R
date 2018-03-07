library(multcomp)
library(lme4)
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


###EXERCISE 2

##T1

search = read.table("search.txt", header=TRUE)
attach(search)

#gotta do the randomization again but still don't get how that works 
#like in the last exercise


##T2
interaction.plot(skill,interface,time)
interaction.plot(interface,skill,time)

#The lines seems unparallel which indicates that there might be interaction

##T3

search$skill = as.factor(search$skill)
search$interface = as.factor(search$interface)

searchaov = lm(time~interface,data=search)
anova(searchaov)

#H0 mu0=mu1=mu2=mu3=mu4 cannot be rejected.
#I used 1 way anova because searchaov = lm(time~interface*skill,data=search) was not returning p-value.


##T4


##T5


##T6


##T7



###EXERCISE 3

##T1


##T2


##T3


##T4



###EXERCISE 4

##T1


##T2


##T3


##T4



###EXERCISE 5

##T1


##T2


##T3


##T4




###EXERCISE 6

##T1


##T2


##T3


##T4


##T5



###EXERCISE 7