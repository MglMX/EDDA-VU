###EXERCISE 3
library(multcomp)
library(lme4)

##T1

cream = read.table("cream.txt", header=TRUE)
attach(cream)
cream 

cream$batch = as.factor(cream$batch)
cream$position = as.factor(cream$position)
cream$starter = as.factor(cream$starter)

creamaov = lm(acidity~starter+batch+position, data = cream)
anova(creamaov)

# Findings:

# IDK how why they used H0: alpha,beta = 0 !!

# The p-value for testing H0: alpha_i = 0 for all i is 2.904e-05;
# for H0: beta_j = 0 for all j is 0.001632;
# for H0: gama_k = 0 for all k is 0.411191. # should I write down not-zero for gamma!!!
# Ho can't be rejected for gama_k. So , the third factor i.e. position does not have a main effect!
# But while on the other hand for the other two factors starter and batch, we can reject H0. Therefore, they have main effect.


##T2
summary(creamaov)

# The output shows the estimates of meu, alpha2,alpha3,alpha4,alpha5, beta2,beta3,beta4,beta5, gama2,gama3,gam4,gama5.
# The estimates of alpha1 (for starter 1), beta1 (for batch 1) and gama1 (for position 1) are not shown in the summary table. These are minus the sum of
# the other main effects. And The p-values are for testing H0 that the coefficient is 0.

creammult =glht(creamaov,linfct=mcp(starter="Tukey")) # For starter
summary(creammult)

creammult2 =glht(creamaov,linfct=mcp(batch="Tukey")) # Not asked in exercise just to know
summary(creammult2)

creammult3 =glht(creamaov,linfct=mcp(position="Tukey")) # Not asked in exercise just to know
summary(creammult3)

# Findings : Check lecture 6 page : 3 - 8

##T3
summary(creamaov)

# Findings : Check lecture 6 page : 3 - 8

##T4
confint(creammult) # For starter

# I don't know yet how to interpret the findings!

# The intervals[1.3208,4.2992], [1.4708, 4.4492], [2.3008, 5.2792] and [-4.7832, -1.8048] for the differences alpha4 - alpha1, alpha4 - alpha2, alpha4 - alpha3 and alpha5 - alpha4
# do not contain number 0.  
