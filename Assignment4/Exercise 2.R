#Exercise 2

expsi = read.table("psi.txt", header=TRUE);
expsi

#T1

nousepsi = expsi[which(expsi$psi == "0"),]
nousepsi
usepsi = expsi[which(expsi$psi == "1"),]
usepsi

hist(nousepsi$gpa, breaks = c(2.0, 2.5, 3.0, 3.5, 4.0, 4.5), xlab="GPA", ylab="N of students", main="GPA of students not receiving PSI")
hist(usepsi$gpa, breaks = c(2.0, 2.5, 3.0, 3.5, 4.0, 4.5), xlab="GPA", ylab="N of students", main="GPA of students receiving PSI")

boxplot(expsi$gpa, usepsi$gpa, nousepsi$gpa, main="GPA Boxplots", names=c("All", "PSI", "No PSI"));


#T2

expsiglm=glm(passed~psi+gpa,data=expsi,family=binomial)
summary(expsiglm)


#T3

#psi increases the linear predictor by 2.338 
#and increases odds of passing by e^2.338=10.3604948382


#T4

expsi$passed=factor(expsi$passed)
expsi$psi=factor(expsi$psi)
expsi$gpa=factor(expsi$gpa)
expsiglm2=glm(passed~psi+gpa, data=expsi,family=binomial)
summary(expsiglm2)

#estimations shown, I think intercept+psi1 + gpa3.0 would be the first estimation 
#and intercept+gpa3.0 would be the second. 

#T5



#T6

x=matrix(c(3,15,8,6),2,2)
x
fisher.test(x)

#15 is the people that didn't show improvement from the 18 
#and 3 is the people that showed improvement
#Same with the second column out of 14 8 showed improvement and 6 didn't
#So first column is students not receiving psi and second is the students receiving it
#row 1 shows the students that improved and row 2 the rest that didn't


#T7



#T8