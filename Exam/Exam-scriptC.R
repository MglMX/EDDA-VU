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

###Task 2

galapagos=read.table("gala.txt", header=TRUE)


#With this method, we start with all of the possible variables in our model. Then, we choose the one that gives the highest p-value. If this p-value is bigger than 0.05, we will discard the variable and repeat the process without it.


galaglm=glm(sqrt(Species)~Area+Elevation+Nearest+Scruz+Adjacent,family=poisson,data=galapagos)
summary(galaglm)

#The variable Nearest has the highest p-value with a p-value of 0.411. Since this p-value is bigger than 0.05, we discard it for our model and continue to the next iteration.

galaglm=glm(sqrt(Species)~Area+Elevation+Scruz+Adjacent,family=poisson,data=galapagos) 
summary(galaglm)

#The variable Scruz has the highest p-value with a p-value of 0.2466. Since this p-value is bigger than 0.05, we discard it for our model and continue to the next iteration.

galaglm=glm(sqrt(Species)~Area+Elevation+Adjacent,family=poisson,data=galapagos) 
summary(galaglm)

#As we can see, all the p-values are smaller than 0.05, thus meaning that all the variables are significant for our model.


#The resulting model of the step-down method is:
  
#sqrt(Species) = 1.314e+00 + -3.262e-04\*Area + 2.018e-03\*Elevation -3.987e-04\*Adjacent + error