#EXAM!!!!!!

gala_data = read.table("gala.txt", header = TRUE)
gala_data
attach(gala_data)

gala_glm = glm(Species~Area+Elevation+Nearest+Scruz+Adjacent, family= poisson, data = gala_data)
summary(gala_glm)

# As we can see, all the p-values are smaller than 0.05 now
# That means all the variables are significant for our model.

# The resulting model of the step-down method is:

Species = 3.15 - 5.80e-04 * Area +   3.54e-03  * Elevation + 8.83e-03 * Nearest - 5.71e-03 * Scruz - 6.63e-04 * Adjacent + error

######################## END #########################################
## For lm
# Nearest p = 0.9932 > 0.05
# Omit it

# Step Down

gala_glm_sd = glm(Species~Area+Elevation+Scruz+Adjacent, family= poisson, data = gala_data)
summary(gala_glm_sd)

# Area p = 0.27555 > 0.05
# Omit it

gala_glm_sd = glm(Species~Elevation+Scruz+Adjacent, family= poisson, data = gala_data)
summary(gala_glm_sd)

# Scruz p = 0.19632 > 0.05
# Omit it

gala_glm_sd = glm(Species~Elevation+Adjacent, family= poisson, data = gala_data)
summary(gala_glm_sd)

# As we can see, all the p-values are smaller than 0.05 now
# That means all the variables are significant for our model.

# The resulting model of the step-down method is:

Species = 1.4329 + 0.2766 * Elevation - 0.0689 * Adjacent + error


##TASK 2

galapagos=read.table("gala.txt", header=TRUE)

galalm=lm(sqrt(Species)~Area+Elevation+Nearest+Scruz+Adjacent,data=galapagos) 
summary(galalm)[[4]]

#The variable Nearest has the highest p-value with a p-value of [`r summary(galalm)[[4]][22]`]. Since this p-value is bigger than 0.05, we discard it for our model and continue to the next iteration.

galalm=lm(sqrt(Species)~Area+Elevation+Scruz+Adjacent,data=galapagos) 
summary(galalm)[[4]]

#The variable Scruz has the highest p-value with a p-value of [`r summary(galalm)[[4]][19]`]. Since this p-value is bigger than 0.05, we discard it for our model and continue to the next iteration.

galalm=lm(sqrt(Species)~Area+Elevation+Adjacent,data=galapagos) 
summary(galalm)[[4]]

#The variable Area has the highest p-value with a p-value of [`r summary(galalm)[[4]][14]`]. Since the p-value is bigger than 0.05, we discard it for our model and continue to the next iteration.

galalm=lm(sqrt(Species)~Elevation+Adjacent,data=galapagos) 
summary(galalm)[[4]]

#As we can see, all the p-values are smaller than 0.05, thus meaning that all the variables are significant for our model.


#The resulting model of the step-down method is:
  
#sqrt(Species) = 3.5379 + 0.012942\*Elevation -0.00289\*Adjacent + error
