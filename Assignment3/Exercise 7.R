###EXERCISE 7

library(multcomp)
library(lme4)

expcrime =  read.table("expensescrime.txt", header=TRUE)
expcrime

###STEP-UP method


##Step1 - we choose the most significant variable - MODEL: expend

#V:BAD
crimelm=lm(expend~bad,data=expcrime) #R2: 0.6964
summary(crimelm)

#V:CRIME
crimelm=lm(expend~crime,data=expcrime) #R2: 0.1119
summary(crimelm)

#V:LAWYERS
crimelm=lm(expend~lawyers,data=expcrime) #R2: 0.9373
summary(crimelm)

#V:EMPLOY
#Highest R2 = Most significant (We choose this one)
crimelm=lm(expend~employ,data=expcrime) #R2: 0.954
summary(crimelm)

#V:POP
crimelm=lm(expend~pop,data=expcrime) #R2: 0.9073
summary(crimelm)


##Step2 - Same coming from R2=0.954 - MODEL: expend~employ

#V:BAD
crimelm=lm(expend~employ+bad,data=expcrime) #R2: 0.9551
summary(crimelm)

#V:CRIME
crimelm=lm(expend~employ+crime,data=expcrime) #R2: 0.9551
summary(crimelm)

#V:LAWYERS
#Highest R2 = Most significant (We choose this one)
crimelm=lm(expend~employ+lawyers,data=expcrime) #R2: 0.9632
summary(crimelm)

#V:POP
crimelm=lm(expend~employ+pop,data=expcrime) #R2: 0.9543
summary(crimelm)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
## Should we stop after step 1? Because when we do step 2 we can see there aren't any significnt difference

## Adding either bad, crime, lawyers or pop yields insignificant explanatory variables.
## Therefore, we should stop at the previous step. [See Lecture 9 slide: 12]


### The resulting model of the step-up method is

crimelm=lm(expend~employ,data=expcrime) #R2: 0.954
summary(crimelm)
# expend = -1.167e+02 + 4.681e-02*employ  + error

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #

##Step3 - Same coming from R2=0.9632 - MODEL:expend~employ+lawyers

#V:BAD
crimelm=lm(expend~employ+lawyers+bad,data=expcrime) #R2: 0.9639
summary(crimelm)

#V:POP
crimelm=lm(expend~employ+lawyers+pop,data=expcrime) #R2: 0.9637
summary(crimelm)

#V:CRIME
crimelm=lm(expend~employ+lawyers+crime,data=expcrime) #R2: 0.9632
summary(crimelm)

##Adding any of these variables = unsignificant explanatory variables only +0.001 change


###The resulting model of the step-up method is

crimelm=lm(expend~employ+lawyers,data=expcrime) #R2: 0.9632
summary(crimelm)
#total = -1.107e+02 + 2.971e-02*employ + 2.686e-02*lawyers + error


###STEP-DOWN method


#Step1: We choose the one with highest p-value. 
##If p-value>0.05 it's unsignificant and we get it out

crimelm=lm(expend~employ+bad+crime+lawyers+pop,data=expcrime) 
summary(crimelm)
#crime has the highest p-value=0.255534 -- p-val>0.05 -- out


#Step2: Same

crimelm=lm(expend~employ+bad+lawyers+pop,data=expcrime) 
summary(crimelm)

#pop has the highest p-value=0.06012 -- p-val>0.05 -- out


#Step3: Same

crimelm=lm(expend~employ+bad+lawyers,data=expcrime) 
summary(crimelm)

#bad has the highest p-value=0.34496 -- p-val>0.05 -- out


#Step4: Same

crimelm=lm(expend~employ+lawyers,data=expcrime) 
summary(crimelm)

#All p-values<0.05 = all variables are significant


###The resulting model of the step-down method is

crimelm=lm(expend~employ+lawyers,data=expcrime) # R2 = 0.9632
summary(crimelm)
#expend = -1.107e+02 + 2.971e-02*employ + 2.686e-02*lawyers + error

#WE GET THE SAME MODEL USING BOTH METHODS

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #

# We have found two different models by using two different strategies namely, step-up and step-down.

# Model 1: R2: 0.954
# expend = -1.167e+02 + 4.681e-02*employ  + error

# Model 2:  R2 = 0.9632
# expend = -1.107e+02 + 2.971e-02*employ + 2.686e-02*lawyers + error

# So, Model 1 is preferred, because it has less variables
# and an only slightly lower value of R2. [Check Lecture 9 slide: 25]


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #

###DIAGNOSTICS

##scatter plot of Y against each X separately 
##(this yields overall picture, and shows outlying values.)

expcrime = read.table("expensescrime.txt", header=TRUE); 
pairs(expcrime[,c(2:7)])


##scatter plot of residuals against each Xk in the model separately 
##(look at pattern(curved?) and spread.)

attach(expcrime)
crimelm=lm(expend~employ)
plot(residuals(crimelm),employ)

crimelm=lm(expend~lawyers)
plot(residuals(crimelm),lawyers) ## This isn't required I guess!!


##scatter plot of residuals against each Xk not in the model separately 
##(look at pattern — linear? then include!)


plot(residuals(crimelm),lawyers)
plot(residuals(crimelm),pop)
plot(residuals(crimelm),bad)
plot(residuals(crimelm),crime) # For this why I'm getting Error saying 'x' and 'y' lengths differ??


##scatter plot of residuals against Y 
##(look at spread.)

plot(residuals(crimelm),expend)
plot(residuals(crimelm),fitted(crimelm))


##normal QQ-plot of the residuals 
##(check normality assumption.)

qqnorm(residuals(crimelm))
shapiro.test(residuals(crimelm))



###influence points(outliers)
round(cooks.distance(crimelm), 2)
plot(cooks.distance(crimelm))


###collinearity

##Graphical way to investigate collinearity:
# scatter plot of Xj against Xk for all combinations j, k 
#(check pairwise collinearity)

pairs(expcrime[, c(2:7)])

##Numerical way to investigate collinearity:
#pairwise linear correlation of Xj and Xk for all combinations j, k 
#(check whether these are far from 0)

round(cor(expcrime[,5:6]),2) # Why we are checking only for 5:6 ??
# they are collinear with 0.97
# we can't have both collinear variables in our model so we get lawyers out


#pairwise linear correlation of Xj and Xk for all combinations j, k 
round(cor(expcrime[,3:7]),2) 

# We see that the correlation between lawyers and employ is indeed very high
# (0.97). And the correlation between pop and employ is very high too (same 0.97). 
# This is in agreement with the scatter plots (of course!). [Check Lecture 9 slide: 38]

crimelm=lm(expend~employ,data=expcrime) 
summary(crimelm)

