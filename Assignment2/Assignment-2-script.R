# Exercise 4

# Lecture 3 - Slide No: 11 (explanation about normality)

# Exercise 5

peruvians = read.table("peruvians.txt", header = TRUE);
peruvians
peruvians = peruvians[, - c(5, 6, 7)] # Deleting redundant columns since we don't need to use them for this exercise
peruvians

# Task 1

attach(peruvians)
pairs(peruvians)

#Plots of each pair for better understanding

plot(migration, age, main = "Migration ~ Age Plot")
abline(lm(age~migration), col="red")

plot(migration, weight, main = "Migration ~ Weight Plot")
abline(lm(weight~migration), col="red")

plot(migration, length, main = "Migration ~ Length Plot")
abline(lm(length~migration), col="red")

plot(migration, wrist, main = "Migration ~ Heart rate Plot")
abline(lm(wrist~migration), col="red")

plot(migration, systolic, main = "Migration ~ Systolic Plot")
abline(lm(systolic~migration), col="red")

plot(migration, diastolic, main = "Migration ~ Diastolic Plot")
abline(lm(diastolic~migration), col="red")

# Task 2

cor.test(migration, age) # Co-related if normal
cor.test(migration, weight) # Co-related if normal
cor.test(migration, length) # Not Co-related
cor.test(migration, wrist) # Not Co-related
cor.test(migration, systolic) # Not Co-related
cor.test(migration, diastolic) # Not Co-related

# Correlation Checking for Migration~Age

qqnorm(migration, main = "QQ - Plot of Migration")
qqnorm(age, main = "QQ - Plot of Age")

shapiro.test(migration) # Not Normal
shapiro.test(age) # Normal

# So, use the rank correlation test of Spearman.

cor.test(migration, age, method = "spearman") # There is significant rank correlation

# Correlation Checking for Migration~Weight

qqnorm(migration, main = "QQ - Plot of Migration")
qqnorm(weight, main = "QQ - Plot of Weight")

shapiro.test(migration) # Not Normal
shapiro.test(weight) # Not Normal

# So, use the rank correlation test of Spearman.

cor.test(migration, weight, method = "spearman") # There is no rank correlation!! ?? IDK how to explain

# Exercise 6

# Task 1

run = read.table("run.txt", header = TRUE)
run[,1:2]
attach(run)

# graph for before data 
hist(before)
qqnorm(before)
qqline(before) # Not Normal #

# graph for after data 
hist(after)
qqnorm(after)
qqline(after) # Normal #

# Before ~ After grah plotting 

plot(before, after, main = "Before Vs After")
abline(lm(after~before), col="red")
boxplot(before, after, names = c("before", "after"))

# Graph for the differences before-after

hist(before-after)
qqnorm(before-after)
qqline(before-after) # Normal #
boxplot(before-after, names = "before - after")

# Check for normality

shapiro.test(before-after) # proves our asumption that the difference is normal

# Task 2

lemo = run[which(drink == "lemo"),]
lemo

energy = run[which(drink == "energy"),]
energy

# Test for "Lemo"

# Paired t-test assumes the differences are form normal distribution (Lec 3 - p 25)

t.test(lemo$before, lemo$after, paired = TRUE) # We can not reject H0 i.e. the mean of the differences is 0

# t-test assumes the data are form normal distribution (Lec 3 - p 6)

t.test(lemo$before - lemo$after)


# both paired t-test and one-sample t-test has the same p value. That is they are equivalent.

# Test for "energy"

t.test(energy$before, energy$after, paired = TRUE) # We can not reject H0 i.e. the mean of the differences is 0
t.test(energy$before - energy$after)

# both paired t-test and one-sample t-test has the same p value. That is they are equivalent.

# Task 3

time_difference = run$before - run$after
time_difference

lemo_time_difference = lemo$before - lemo$after
lemo_time_difference

energy_time_difference = energy$before - energy$after
energy_time_difference

t.test(lemo_time_difference, energy_time_difference) # Paired or Not Paired!!!

t.test(lemo_time_difference)
t.test(energy_time_difference)

# We can not reject H0 i.e. the mean of the differences is 0. So, we can not say whether it is affected by the type of drink! Even if we do one sample t-test for both lemo and energy drink individually we could say that, we can't reject H0. So , we can't say that it is affected by drink type.
# Task 4

# Task 5

# Task 6

# OUr assumption for performing analysis in Exercise 3 was that the differences are taken from a random sample of normal distribution

qqnorm(lemo_time_difference)
qqline(lemo_time_difference)
shapiro.test(lemo_time_difference)
# p- value = 0.3725. Therefore, no reason to suspect that the assumption about differences are not taken from normal distribution

qqnorm(energy_time_difference)
qqline(energy_time_difference)
shapiro.test(energy_time_difference)
# p- value = 0.2788. Therefore,the differences are taken from normal distribution.

# How would you transform this vector into 24 residuals to investigate this assumption in QQ-plots? Make this QQ-plot(s).

levels <- c("lemo", "lemo", "lemo", "lemo", "lemo", "lemo", "energy", "energy", "energy", "energy", "energy", "energy")

# difference_data <- data.frame(first=lemo_time_difference, second=energy_time_difference, f=levels)
# Two types = lemo and energy. Each contains 12 differences (before - after)

difference_data <- data.frame(difference_soft =lemo_time_difference, difference_energy =energy_time_difference)
difference_data

runframe = data.frame(yeild=as.vector(as.matrix(difference_data)),  group = factor(rep(1:4, each=6)))
runframe
runaov = lm(yeild~group, data = runframe)
anova(runaov)
summary(runaov)
confint(runaov)

par(mfrow=c(1,2)); 
for (i in 1:2) qqnorm(difference_data[,i])
par(mfrow=c(1,1)); qqnorm(residuals(runaov))
qqline(residuals(runaov)) # Normal

# p-value for H0 i.e m0=m1=m2=m4 = 0.2622. Therefore, H0 is not rejected!! (or should I reject I'm confused again :D) 
# Therefore, our assumption that is the differences are taken from Normal distribution is correct!

# Exercise 7

