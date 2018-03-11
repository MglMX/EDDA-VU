###EXERCISE 7

library(multcomp)
library(lme4)

crime =  read.table("expensescrime.txt", header=TRUE)
crime
plot(crime[,c(1,4,7)])
##stepup method
#step1
crimelm=lm(expend~bad,data=crime)
summary(crimelm)

crimelm=lm(expend~crime,data=crime)
summary(crimelm)

crimelm=lm(expend~lawyers,data=crime)
summary(crimelm)

#we choose this one
crimelm=lm(expend~employ,data=crime)
summary(crimelm)

crimelm=lm(expend~pop,data=crime)
summary(crimelm)

#step2
crimelm=lm(expend~employ+bad,data=crime)
summary(crimelm)

crimelm=lm(expend~employ+crime,data=crime)
summary(crimelm)
#choose this
crimelm=lm(expend~employ+lawyers,data=crime)
summary(crimelm)

crimelm=lm(expend~employ+pop,data=crime)
summary(crimelm)

#step3

crimelm=lm(expend~employ+lawyers+bad,data=crime)
summary(crimelm)

crimelm=lm(expend~employ+lawyers+pop,data=crime)
summary(crimelm)

crimelm=lm(expend~employ+lawyers+crime,data=crime)
summary(crimelm)

##adding any of these = significant explanatory variables only +0.001

#The resulting model of the step-up method is
#total = 993.8317 + 12.2865*expend - 2.8509*takers + error