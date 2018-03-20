#Exercise 1

africa_data = read.table("africa.txt", header = TRUE)
attach(africa_data)
africa_data

pairs(africa_data)

#T1


n = c(10, 100, 1000)
lambda = c(0.5, 1, 10, 100)

study_poisson <- function(n, lambda){
  rpois(n, lambda)
}

poisson_st = list()
k = 1
for(i in n){
  for(j in lambda){
    print(i)
    print(j)
    print(rpois(i,j))
    
    poisson_st = c(poisson_st,list(rpois(i,j)))
    
    
    
    k = k +1
  }
}

x = poisson_st[1]
hist(x)
qqnorm(x)
qqline(x)


# Need to modify. This was just for testing


#T2

# I don't understand it yet!!!




#T3

africa_glm = glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numelec+numregim, family = poisson, data = africa_data)
summary(africa_glm)


#T4

### Step-Down Method

africa_glm_sd = glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numelec+numregim, family = poisson, data = africa_data)
summary(africa_glm_sd)

summary(africa_glm_sd)[[12]][35] # p-value = 0.80605

# numelec has the highest p-value and it is > 0.05. So discard it in the next iteration.

africa_glm_sd = glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numregim, family = poisson, data = africa_data)
summary(africa_glm_sd)

summary(africa_glm_sd)[[12]][32] # p-value = 0.42639

# numregim has the highest p-value and it is > 0.05. So discard it in the next iteration.

africa_glm_sd = glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size, family = poisson, data = africa_data)
summary(africa_glm_sd)

summary(africa_glm_sd)[[12]][28] # p-value = 0.421378

# size has the highest p-value and it is > 0.05. So discard it in the next iteration.

africa_glm_sd = glm(miltcoup~oligarchy+pollib+parties+pctvote+popn, family = poisson, data = africa_data)
summary(africa_glm_sd)

summary(africa_glm_sd)[[12]][24] # p-value = 0.29883

# popn has the highest p-value and it is > 0.05. So discard it in the next iteration.

africa_glm_sd = glm(miltcoup~oligarchy+pollib+parties+pctvote, family = poisson, data = africa_data)
summary(africa_glm_sd)

summary(africa_glm_sd)[[12]][20] # p-value = 0.18031

# pctvote has the highest p-value and it is > 0.05. So discard it in the next iteration.

africa_glm_sd = glm(miltcoup~oligarchy+pollib+parties, family = poisson, data = africa_data)
summary(africa_glm_sd)

# As we can see, all the p-values are smaller than 0.05
# That means all the variables are significant for our model.

# The resulting model of the step-down method is:

miltcoup = 0.25138 + 0.09262*oligarchy - 0.57410*pollib + 0.02206*parties + error

#T5

# confint(africa_glm_sd)
# coef(africa_glm_sd)    

## Plots: Fitted vs Residuals

plot(fitted(africa_glm_sd), residuals(africa_glm_sd)) 

# No specific pattern
# It's a scatter plot not visually good as it would suppose to be in linear regression model.

## Plots: Logarithmic-Fitted values vs Residuals

plot(log(fitted(africa_glm_sd)), residuals(africa_glm_sd))

# So we took logarithm to make the x-values fitted by a linear function in the plot. 
# This plot seems OK yet still no specific pattern
# And still it looks scatter too!

## Plots: Logarithmic-Fitted values vs Residuals with type = Response

plot(log(fitted(africa_glm_sd)), residuals(africa_glm_sd, type = "response"))

## Check Lec 10 slide: 41-43

## Plots: Fitted vs Residuals in the Full Model

plot(fitted(africa_glm), residuals(africa_glm))

## Plots: Logarithmic-Fitted values vs Residuals in the Full Model

plot(log(fitted(africa_glm)), residuals(africa_glm))

## Plots: Logarithmic-Fitted values vs Residuals with type = Response in the Full Model

plot(log(fitted(africa_glm)), residuals(africa_glm, type = "response"))


# The plots looked scattered. All of them followed a certain pattern in our model from task-4
# After ploting using the full model, we found the same pattern there too.
# So, it's not because we deleted too many variables.

# Now we will check normality to make some comments for that.


shapiro.test(residuals(africa_glm_sd)) # p-value = 0.01 < 0.05 # Normality is doubtful
shapiro.test(residuals(africa_glm)) # p-value = 0.01 < 0.05 # Normaility is doubtful

qqnorm(residuals(africa_glm_sd)) # doesn't seem normal
qqline(residuals(africa_glm_sd), col="red")

qqnorm(residuals(africa_glm)) # doesn't seem normal
qqline(residuals(africa_glm), col="red")

# In both models we got the same results. 
# The plots that we generated for the model that we found in task-4 and for the full model with all explanatory variables followed same type of pattern.
# Therefore the assumption of normality(if any) is doubtful.
# Which means maybe the sample doesn't come from a normal distribution!
