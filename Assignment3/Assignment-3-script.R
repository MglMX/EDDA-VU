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

#qq datos en los extremos son demasiado extremos outliers
#plot todo datos similares y algun outlier en el 200


###EXERCISE 2

##T1

search = read.table("search.txt", header=TRUE)
attach(search)

#gotta do the randomization again but still don't get how that works 
#like in the last exercise


##T2



##T3


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