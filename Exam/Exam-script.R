#EXAM!!!!!!

# Task 1
gala_data = read.table("gala.txt", header = TRUE)
gala_data
attach(gala_data)

gala_glm = glm(Species~Area+Elevation+Nearest+Scruz+Adjacent, family= poisson, data = gala_data)
summary(gala_glm)

# As we can see, all the p-values are smaller than 0.05 now
# That means all the variables are significant for our model.

# The resulting model of the step-down method is:

Species = 3.15 - 5.80e-04 * Area +   3.54e-03  * Elevation + 8.83e-03 * Nearest - 5.71e-03 * Scruz - 6.63e-04 * Adjacent + error

######################## END  Task 1 #########################################

##TASK 2


######################## END  Task 2 #########################################

## Task 3

## Plots: Fitted vs Residuals

plot(fitted(gala_glm), residuals(gala_glm)) 

# No specific pattern
# It's a scatter plot not visually good as it would suppose to be in linear regression model.


## Plots: Logarithmic-Fitted values vs Residuals

plot(log(fitted(gala_glm)), residuals(gala_glm))

# So we took logarithm to make the x-values fitted by a linear function in the plot. 
# This plot seems OK yet still no specific pattern
# And still it looks scatter too!

## Plots: Logarithmic-Fitted values vs Residuals with type = Response

plot(log(fitted(gala_glm)), residuals(gala_glm, type = "response"))

## The response residuals increase with log of the fitted values as expected from Poisson model

######################## END  Task 3 #########################################


## Task 5

par(mfrow=c(2,3))
for (i in 1:6) hist(gala_data[,i],main=colnames(gala_data)[i],xlab="",ylab="")
pairs(gala_data)
for (i in 1:6) hist(log(gala_data[,i]),main=colnames(gala_data)[i],xlab="",ylab="")
pairs(log(gala_data))

## If we look into the histograms then for the first one (without logarithm), we see that the histograms show the pattern of poisson distribution
## but when we applied logarithm in the second version we see that they are approximately equal to a normal distribution.

## If we look into the pairs then for the first one we see that, the data doesn't look significant to make any conclusion but when we applied the lograthim and cheked the pairs then we can see the data get more scattered and we can find some colinearity between Species~Area, Species~Elevation and Area~Elevation and there might be some colinearity between Nearest~Scruz but it's not obvious.
## In summary by applying the this we tried to normalize the distribution and we get more relevant information.

######################## END  Task 5 #########################################

## Task 6

modlog=lm(log(Species)~log(Area)+log(Elevation)+log(Nearest)+log(Scruz+1)+log(Adjacent),data=gala_data)
summary(modlog)

modlog1=step(modlog)
summary(modlog1)

## if we look into the summary of modlog1 then we can see that log(Scruz + 1) has a p-value =  0.17 > 0.05. So maybe this variable is not significant but we are not sure whether AIC follows the exact same criteria in selecting a model.

######################## END  Task 6 #########################################
