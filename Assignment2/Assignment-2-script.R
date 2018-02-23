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
