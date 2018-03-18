library(multcomp)
library(lme4)

###EXERCISE 4

cows = read.table("cow.txt", header = TRUE)
boxplot(cows)
stripchart(cows, vertical=TRUE)


##T1

#cowframe=data.frame(treatment=as.factor(cows$treatment), milk=as.vector(cows$milk))
#cowframe[1:18,]
#cowsaov=lm(milk~treatment,data=cowframe)
#summary(cowsaov)

#cows$id=factor(cows$id)
#cowsaov=lm(milk~treatment+id,data=cows)
#anova(cowsaov)
#summary(cowsaov)

cows$id=factor(cows$id)
cows$per=factor(cows$per)
cowslm=lm(milk~treatment+per+id,data=cows)
summary(cowslm)

## Findings check lecture 7 slide: 11-12


cowsanova=lm(milk~id+per+treatment,data=cows)
anova(cowsanova) # IDK whether we should include it or not! Check lec 7 slide:12
consider yhis
##T2

# Estimates of treatment B = -0.5100
# Estimates of perid 2 = -2.3900

##T3

#still don't really know how this works, we have to specify a random effect with | somehow but I still have o idea how to manage that
cowsaover=lmer(milk|treatment,data=cowframe) ## This isn't required maybe!
summary(cowsaover)

cowslmer=lmer(milk~treatment+order+per+(1|id),data=cows,REML=FALSE)
summary(cowslmer) ## For explanation.... [Check lecture 7 slide: 13]

cowslmer1 = lmer(milk~order+per+(1|id), data=cows, REML=FALSE)
anova(cowslmer1, cowslmer) ## p-value = 0.446 for cowslmer [Check lecture 7 slide: 14]

##T4

attach(cows)
t.test(milk[treatment=="A"],milk[treatment=="B"],paired=TRUE) ## p-value = 0.8281

#we can't reject that the difference is = to 0
#is this a good test? just show difference of means


