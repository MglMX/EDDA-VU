#Exercise 2

#First we load the data from the data source
expsi = read.table("psi.txt", header=TRUE);
expsi

#T1

#We divide the data in 2 groups, 
#the students that received psi and the ones that didn't
nousepsi = expsi[which(expsi$psi == "0"),]
nousepsi
usepsi = expsi[which(expsi$psi == "1"),]
usepsi

# We show some data summaries

hist(nousepsi$gpa, breaks = c(2.0, 2.5, 3.0, 3.5, 4.0, 4.5), xlab="GPA", ylab="N of students", main="GPA of students not receiving PSI")
hist(usepsi$gpa, breaks = c(2.0, 2.5, 3.0, 3.5, 4.0, 4.5), xlab="GPA", ylab="N of students", main="GPA of students receiving PSI")

boxplot(expsi$gpa, usepsi$gpa, nousepsi$gpa, main="GPA Boxplots", names=c("All", "PSI", "No PSI"));

qqnorm(nousepsi$gpa,  main="Q-Q Plot of Student's GPA without PSI") # doesn't look normal
qqline(nousepsi$gpa, col="red")

qqnorm(usepsi$gpa, main="Q-Q Plot of Student's GPA receiving PSI") # looks normal
qqline(usepsi$gpa, col="red")


#T2

expsiglm=glm(passed~psi+gpa,data=expsi,family=binomial)
summary(expsiglm)


#T3

#psi increases the linear predictor by 2.338 
#and increases odds of passing by e^2.338=10.3604948382




#T4

# with psi and gpa=3.0 --> -11.602+2.338+(3.063*3.0) = -0.075 =
# without psi and gpa=3.0 --> -11.602+(3.063*3.0) = -2.408 =


#T5

expsiglm2=glm(passed~psi+gpa, data=expsi,family=binomial)
summary(expsiglm2)

#exp(intercept(which is supposedly psi0) − psi1?) = 1.786932. )
  

#T6

x=matrix(c(3,15,8,6),2,2)
x
fisher.test(x)

#15 is the people that didn't show improvement from the 18 
#and 3 is the people that showed improvement
#Same with the second column out of 14 8 showed improvement and 6 didn't
#So first column is students not receiving psi and second is the students receiving it
#row 1 shows the students that improved and row 2 the rest that didn't

#conclusion: #with pval 0.02 we can reject the null hypothesis, 
#which will be that the students receiving psi and the 
#ones not receiving it have the same probablity of improvement.

#T7

#I guess it's okay? Fishers test works with this kind of experiments

#T8

#Fishers is good for small counts in each 2x2 table cell. 
#More exact than the other approach, ut it doesn't work with  big counts.