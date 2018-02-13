x=rnorm(30);
par(mfrow=c(1,2));
hist(x);
qqnorm(x);

hist(x*10+3)
qqnorm(10*x+3)

x=rnorm(100)
hist(x)
qqnorm(x)

x=runif(100)
hist(x)
qqnorm(x)

x=rchisq(30,5)
hist(x)
qqnorm(x)

#Exercise 1
load(file="assign1.RData")

par(mfrow=c(1,3))

hist(x1)
qqnorm(x1)
qqnorm(rnorm(length(x1)))
shapiro.test(x1)
#Not normal

hist(x2)
qqnorm(x2)
qqnorm(rnorm(length(x2)))
shapiro.test(x2)
#Not normal. QQPlot doesn't show a straight line. There are some deviations at the beggining and end of the line

hist(x3)
qqnorm(x3)
qqnorm(rnorm(length(x3)))
shapiro.test(x3)
#Normal. Both histograms and QQ-plot show a normal distribution. The qq plot shows a straight line.

hist(x4)
qqnorm(x4)
qqnorm(rnorm(length(x4)))
shapiro.test(x4)
#Normal. The QQplot shows a straight line.

hist(x5)
qqnorm(x5)
qqnorm(rnorm(length(x5)))
shapiro.test(x5)
#Not normal. The QQplot doesn't show a straight line. 

#T-test
m=30
n=30
mu=180
nu=175
sd=10
x=rnorm(m,mu,sd)
y=rnorm(n,nu,sd)
t.test(x,y,var.equal = TRUE)
t.test(x,y,var.equal = TRUE)

B=1000
p=numeric(B)
for(b in 1:B){
  x=rnorm(m,mu,sd)
  y=rnorm(n,nu,sd)
  p[b]=t.test(x,y,var.equal = TRUE)[[3]]
}
power=mean(p<0.05)
power

m=30
n=30
mu=180
nu=180
sd=10
x=rnorm(m,mu,sd)
y=rnorm(n,nu,sd)
t.test(x,y,var.equal = TRUE)
t.test(x,y,var.equal = TRUE)

B=1000
p=numeric(B)
for(b in 1:B){
  x=rnorm(m,mu,sd)
  y=rnorm(n,nu,sd)
  p[b]=t.test(x,y,var.equal = TRUE)[[3]]
}
power=mean(p<0.05)
power

#Exercise 2
#2.1
m=30
n=30
mu=180
nu=180
sd=10
x=rnorm(m,mu,sd)
y=rnorm(n,nu,sd)
t.test(x,y,var.equal = TRUE)
t.test(x,y,var.equal = TRUE)

B=1000
p=numeric(B)
for(b in 1:B){
  x=rnorm(m,mu,sd)
  y=rnorm(n,nu,sd)
  p[b]=t.test(x,y,var.equal = TRUE)[[3]]
}
power05=mean(p<0.05)
sump05=sum(p<0.05)
power05
sump05

power10=mean(p<0.10)
sump10=sum(p<0.10)
power10
sump10

hist(p)
qqnorm(p)

#The t-test shows a high p-value 0.09 so we cannot reject the null hypothesis (Means are equal mu == nu). 
#When getting the power of the t-test we get that 48 p-values are lower than 0.05 (0.048) which means that the power of the test is low so we cannot reject the hypothesis with a 95% cofindence.
#We also see that 99 p--values are lower than 0.1 (0.098) which means we cannot reject H0 with a 90% confidence because the power value is low.
#Checking the histogram we see that the p-values are uniformly distribtued. Which indicates that Ho is true.

#2.2
m=30
n=30
mu=180
nu=180
sd=1
x=rnorm(m,mu,sd)
y=rnorm(n,nu,sd)
t.test(x,y,var.equal = TRUE)
t.test(x,y,var.equal = TRUE)

B=1000
p=numeric(B)
for(b in 1:B){
  x=rnorm(m,mu,sd)
  y=rnorm(n,nu,sd)
  p[b]=t.test(x,y,var.equal = TRUE)[[3]]
}
power05=mean(p<0.05)
sump05=sum(p<0.05)
power05
sump05

power10=mean(p<0.10)
sump10=sum(p<0.10)
power10
sump10

hist(p)

#The t-test shows a high p-value 0.8716 so we cannot reject the null hypothesis (Means are equal mu == nu). 
#When getting the power of the t-test we get that 45 p-values are lower than 0.05 (0.045) which means that the power of the test is low so we cannot reject the hypothesis with a 95% cofindence.
#We also see that 99 p--values are lower than 0.1 (0.099%) which means we cannot reject H0 with a 90% confidence because the power value is low.
#Checking the histogram we see that the p-values are uniformly distribtued. Which indicates that Ho is true.

#2.3
m=30
n=30
mu=180
nu=175
sd=6
x=rnorm(m,mu,sd)
y=rnorm(n,nu,sd)
t.test(x,y,var.equal = TRUE)
t.test(x,y,var.equal = TRUE)

B=1000
p=numeric(B)
for(b in 1:B){
  x=rnorm(m,mu,sd)
  y=rnorm(n,nu,sd)
  p[b]=t.test(x,y,var.equal = TRUE)[[3]]
}
power05=mean(p<0.05)
sump05=sum(p<0.05)
power05
sump05

power10=mean(p<0.10)
sump10=sum(p<0.10)
power10
sump10

hist(p)
#We get a p-value of 0.0005615 so we can reject the null hypothesis
#Checking hte power 889 vvalues are lower than 0.05 (0.889) so 88.9% probability of rejecting the H0 is correct with 95% confidence
#Checking hte power 939 p-values are lower than 0.1 (0.939) so 93.9% probability of rejectuing H0 is correct with 90% conficned
#Checking the histogram we see that the p-values are not uniformly distributed so it indicates that the H0 hypotheisis is not true.



par(mfrow=c(1,3))
#Exercise 3
#3.1

m=n=30
mu=180

sd=5

values = seq(175,185,by=0.1)

powers = numeric(length(values))
i=0
for(nu in values){
  B=1000
  p=numeric(B)
  for(b in 1:B){
    x=rnorm(m,mu,sd)
    y=rnorm(n,nu,sd)
    p[b]=t.test(x,y,var.equal = TRUE)[[3]]
  }
  powers[i]=mean(p<0.05)
  i=i+1
}


plot(values,powers)

#3.2

m=n=100
mu=180
sd=5

values = seq(175,185,by=0.1)

powers = numeric(length(values))
i=0
for(nu in values){
  B=1000
  p=numeric(B)
  for(b in 1:B){
    x=rnorm(m,mu,sd)
    y=rnorm(n,nu,sd)
    p[b]=t.test(x,y,var.equal = TRUE)[[3]]
  }
  powers[i]=mean(p<0.05)
  i=i+1
}

plot(values,powers)

#3.3

m=n=30
mu=180
sd=100

values = seq(175,185,by=0.1)

powers = numeric(length(values))
i=0
for(nu in values){
  B=1000
  p=numeric(B)
  for(b in 1:B){
    x=rnorm(m,mu,sd)
    y=rnorm(n,nu,sd)
    p[b]=t.test(x,y,var.equal = TRUE)[[3]]
  }
  powers[i]=mean(p<0.05)
  i=i+1
}

plot(values,powers)

#Findings:
#With the first two plots we can see how the power of the test decreases when the nu values get closer to mu. This indicates that we are correctly rejecting H0 when nu values get closer to mu. We see that the bigger t
#We see that the bigger the population is, the easier it is to decide wether we should reject H0 or not by looking at the power.
#In the last plot we see that, with the same population and with a high standard deviation, we get a low power values for nu while in the other plots we were getting high power values for the same nu value. 








