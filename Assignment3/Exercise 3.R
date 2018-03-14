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
# The p-values less than 0.05 are: 4 -1 == 0, 4 - 2 == 0, 4 - 3 == 0, 5 - 4 == 0.
#The starter that leads to significant different acidity is number 4.

##T3
summary(creamaov)

#I cannot find H0 alpha2 == alpha 1

# Findings : Check lecture 6 page : 3 - 8

##T4
confint(creammult) # For starter

# The intervals
# 4 - 1 == 0  [1.3198,4.3002]
# 4 - 2 == 0  [1.4698 ,4.4502]
# 4 - 3 == 0  [2.2998,5.2802]
# 5 - 4 == 0 [-4.7842 -1.8038]

# Do not contain 0.   
#This indicates that starter 4 has a main effect compared to the rest of starters.

