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