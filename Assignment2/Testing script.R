par(mfrow=c(1,1));
#Exercise 1
#1.1
Data <- read.table("telephone.txt",header = TRUE);
bills = Data$Bills;

t=median(bills)
t
n=length(bills);
n

hist(bills,prob=T);

bootstrap_exp<-function(n,lambda){
  B=1000;
  tstar=numeric(B);
  for(i in 1:B){
    xstar = rexp(n,lambda); #Exercise says for lambda = [0.01,0.1]
    tstar[i] = median(xstar);
  }
  return(tstar)
}

lambda_values=seq(0.01,0.1,length=200)
lambda_values

p_array=numeric(length(lambda_values))
i=1
for(lambda in lambda_values){
  tstar=bootstrap_exp(n,lambda);
  pl=sum(tstar<t)/B;
  pr=sum(tstar>t)/B;
  p=2*min(pl,pr);
  p_array[i]=p;
  i=i+1;
}

p_array;
p_not_reject=p_array[p_array>0.05] #We take the values where the p value would not reject H0
p_not_reject
indexes = match(p_not_reject,p_array);

indexes;
lambda_not_reject=numeric(length(indexes));
count=1
for(i in indexes){
  lambda_not_reject[count]=lambda_values[i];
  count=count+1;
}

#We cannot reject that the data stems from a exponential distribution with lambda values:
lambda_not_reject


length(lambda_values)
length(p_array)

plot(lambda_values,p_array)
#Checking our graph we get that forthe values of lambda in range [0.01,0.1] all the p-values we get are 0. so we can reject the H0 that our distribution come from an exponential distribution of lambda range 0.01 0.1

#1.2

hist(bills)
boxplot(bills)
#Since half of the customers spend less than 27€ the marketing advice could be 
#Any other plot?
#Conclusion: Most of the people spend less than 20€ per month (?)

#Exercise 2
#Retrieving data

light1879 = scan("light1879.txt");
light1879;

light1882 = scan("light1882.txt")
light1882

light_time = scan("light.txt") #Holds the times saved from the experiment which are NOT the seconds taken to travel 7442Km
light_time


light_real_time = ((light_time/1000)+24.8)/1000000 #Holds the real time taken to travel 7442Km in seconds
light_real_time

light_speed = (7.442/light_real_time)-299000 #Measured speed of light minus 299000
light_speed

#2.1
#Histograms and box plots
par(mfrow=c(1,3));
hist(light1879);
boxplot(light1879);

hist(light1882);
boxplot(light1882);

hist(light_speed);
boxplot(light_speed);


boxplot(light1879,light1882,light_speed)

quantile(light1879)
quantile(light1882)
quantile(light_speed)
#Comparing the histograms and boxplots we can see that the distribution of the light1882 and light_speed are quite similar. Meanwhile, the light1879 the boxplot seems shifted upwards which means that the values are bigger.
#If we consider the median values then we get for light1879 is 850, for 1882 is 774 and light_speed is 754. We see again that light1882 and light_speed medians are close to each other.

#2.2
#Bootstrap confidence intervals lecture 2

#Defining function for bootstrap test

bootstrap<-function(data,operation="mean"){
  B=1000;
  tstar=numeric(B);
  
  for(i in 1:B){
    xstar = sample(data,replace = TRUE);
    
    if(operation=="mean"){
      tstar[i] = mean(xstar);
    }else if(operation=="median"){
      tstar[i] = median(xstar);
    }
  }
  return(tstar);
}

get_confidence_intervals<-function(data,operation="mean"){
  if(operation=="mean"){
    t=mean(data);
  }else if(operation=="median"){
    t=median(data);
  }
  tstar = bootstrap(data,operation);
  tstar25 = quantile(tstar,0.025);
  tstar975 = quantile(tstar,0.975);
  return(c(2*t-tstar975,2*t-tstar25));
}

#Light1879
add_speed=299000
add_speed=0


confidence_mean = get_confidence_intervals(light1879);
confidence_mean+add_speed;

confidence_median = get_confidence_intervals(light1879,"median");
confidence_median+add_speed;

#Light1882
confidence_mean = get_confidence_intervals(light1882);
confidence_mean+add_speed;

confidence_median = get_confidence_intervals(light1882,"median");
confidence_median+add_speed;

#Light
confidence_mean = get_confidence_intervals(light_speed);
confidence_mean+add_speed;

confidence_median = get_confidence_intervals(light_speed,"median");
confidence_median+add_speed;

#2.3 Comments
#We see that for the confidence intervals the light1882 and lightspeed they are similir and more or less in the same range. Meanwhile, the confidence interval of 1879 is not in the same range as the other two for the mean and for the median. The intesection of the intervals of 1879 and the others
#is empty.Then the experiments of 1879 maybe are not accurate enough or at least they differ from the other two experiments..
#2.4
#Speed of light according to https://physics.nist.gov/cgi-bin/cuu/Value?c
#299792.458
# For light1879 the value doesn't fall in the interval neither mean or median
# For light1882 the value doesn't fall in the interval neither mean or median
# For light it also doesn't fall in the interval neither mean or median
#It is not consistent

#We see that the currently most accurate value of speed does not fall in the confidence interval of the first data set (1879) neither does in the measures of Newcombs (light_speed) neither for the mean or median. But, it does falls in the mean and median confidence interval of Michael's 1882.
#It is consistent for Michelson measurements of the experiments made in 1882 but not in the other two.

#Exercise 3
#3.1
klm = scan("klm.txt");
klm
hist(klm)
qqnorm(klm)
qqline(klm)

#We check that the population doesn not seem normal this tell us that doing the t-test is not the best option
#We should perform the sign test Lecture 3 (13)
#Wilcoxon test doesn't seem appropiate because the population is not symetric

#Sign test
#We compute how many values are bigger than the median to test
n_smaller = sum(klm>31)
n_smaller
n = length(klm)
n
binom.test(n_smaller,n,p=0.5)

median(klm) #Checking
quantile(klm)

#
#We can reject the null hypothesus that the median of the population is == 31. But checking at the confidence interval 0.53 0.78, we see that 31 should be between quantile 0.53 and 0.78 therefore 
# How to change the null hypothesis to check if population is <= than 31?
# Checking confidence interval it gives 0.21 and 0.46. Can we say with that info the median is bigger than 31?


#3.2
quantile(klm,probs=0.8) #$Just checking

n_bigger = sum(klm>72);
binom.test(n_bigger,n,p=0.1)

#With a p value of 0.002 we can reject that at most 10% of the parts arrive later than 72 days. 
# If we check the confidence intervals 0.12 and 0.34, this means that between 12% and 34% of the pieces arrive later than 72 days (Can we say this?)

#Exercise 4
Clouds<-read.table("clouds.txt",header = TRUE);

hist(Clouds$seeded)
hist(Clouds$unseeded)

differences=Clouds$seeded-Clouds$unseeded
mean(differences)

hist(differences)
boxplot(Clouds$seeded,Clouds$unseeded)
boxplot(differences)

#4.1

#Check if the differences come from a normal distribution
qqnorm(differences)
qqline(differences)
shapiro.test(differences)
#QQplot an shapiro indicate that the differences do not follow normal distribution

#Should we do t.test eventhough the differences of the two data sets doesn't seem to be normal

#T-test
#Lecture 3 slide 25 says that paired t-test assumes that the differneces are a random sample from a normal population

plot(Clouds$seeded~Clouds$unseeded,data=Clouds); abline(0,1)

t.test(Clouds$seeded,Clouds$unseeded,paired = TRUE)

#H0 is rejected. Mean of the differences is different from 0.

#Mann-whitney-wilcoxon test. Lecture 4 slide 20
wilcox.test(Clouds$seeded,Clouds$unseeded)
#p-value indicates that H0 is rejected. The samples come from different populations
#though probably we shouldn't use this test because it is for independent samples and our data is paired

#Kolmorov-Smirnov. Lecture 4 slide 22
ks.test(Clouds$seeded,Clouds$unseeded);
#p-value indicates that H0 is rejected. The samples come from different populations
#though probably we shouldn't use this test because it is for independent samples and our data is paired

#4.2
clouds_sq=sqrt(Clouds)
clouds_sq

#T-test
t.test(clouds_sq$seeded,clouds_sq$unseeded,paired = TRUE)
#p-value rejects H0. Mean of the diiferences is different from 0.

#Mann-whitney-wilcoxon test
wilcox.test(clouds_sq$seeded,clouds_sq$unseeded)
#p-value indicates that H0 is rejected. The samples come from different populations

#Kolmorov-Smirnov.
ks.test(clouds_sq$seeded,clouds_sq$unseeded);
#p-value indicates that H0 is rejected. The samples come from different populations

#4.3
clouds_sq_sq=sqrt(clouds_sq)
#T-test
t.test(clouds_sq_sq$seeded,clouds_sq_sq$unseeded,paired = TRUE)
#p-value rejects H0. Mean of the diiferences is different from 0.

#Mann-whitney-wilcoxon test
wilcox.test(clouds_sq_sq$seeded,clouds_sq_sq$unseeded)
#p-value indicates that H0 is rejected. The samples come from different populations

#Kolmorov-Smirnov.
ks.test(clouds_sq_sq$seeded,clouds_sq_sq$unseeded);
#p-value indicates that H0 is rejected. The samples come from different populations


#Findings: applying square root to the samples makes the p-value on the t-test to decrease.
#while in the Mann-whitney-wilcoxon test and Kolmorov-sminorv test p-value remains the same after doing the test on the square rooted values.

#Exercise 5
Peruvians<-read.table("peruvians.txt",header = TRUE);
Peruvians=Peruvians #Deleting columns we don't need

#5.1
plot(Peruvians$migration~Peruvians$weight)
pairs(Peruvians)

#Looking at the graph generated we can say that migration is corelated with age and weight. 
#There could maybe be also correlation with diastolic

#5.2
cor.test(Peruvians$migration,Peruvians$age); #Correlated.
cor.test(Peruvians$migration,Peruvians$weight); #Correlated. 
cor.test(Peruvians$migration,Peruvians$length); #Not correlated. 
cor.test(Peruvians$migration,Peruvians$wrist); #Not correlated. 
cor.test(Peruvians$migration,Peruvians$systolic); #Not correlated
cor.test(Peruvians$migration,Peruvians$diastolic); #Not correlated

#Exercise 6

#6.1 

#This example looks very similar to the ones in Lecture 3 (23)
Run<-read.table("run.txt")

hist(Run$before)
qqnorm(Run$before)
qqline(Run$before)

hist(Run$after)
qqnorm(Run$after)
qqline(Run$after)

differences = Run$before-Run$after

hist(differences)
qqnorm(differences)
qqline(differences)

#The differences seem to come from a normal distribution

plot(before~after,data=Run);
abline(0,1)
boxplot(Run$before,Run$after)
boxplot(differences)


#6.2

Run_lemo = Run[which(Run$drink=="lemo"),]
Run_energy = Run[which(Run$drink=="energy"),]

t.test(Run_lemo$before,Run_lemo$after,paired = TRUE)
#We cannot reject that the mean of the differences is 0. Therefore, we cannot say that the running speed is affected for the people who drink lemo

t.test(Run_energy$before,Run_lemo$after,paired = TRUE)
#We cannot reject that the mean of the differences is 0. Therefore, we cannot say that the running speed is affected for the people who drink energy drink

# Why you are doing the test with energy_before and lemo_after!

#6.3

differences_lemo = Run_lemo$before-Run_lemo$after
differences_energy = Run_energy$before - Run_energy$after

t.test(differences_lemo,differences_energy)

#We cannot reject that the mean of the diffences is 0. Therefore, we cannot say that the time differences are affected by the type of drink

#6.4 Lecture 3 (24)
#A possible objection could be that the time taken between the two runs are not enough. According to http://www.sciencefocus.com/article/human-body/how-long-does-caffeine-take-kick
#Cafeine takes around 45 minutes to kick in. So maybe half an hour was not enough to see the effects of the drinks in the running speed.

#6.5 Lecture 3 (24)
#The possible objection could be that 30 minutes is nor enough time to rest and do the second run in the same conditions as the first run.
#(?)

#6.6
#The test assumes that the distribution of the differences is normal
#Second question??? (Maybe something realted with one-way ANOVA?)

qqnorm(differences_lemo);
qqline(differences_lemo);

qqnorm(differences_energy);
qqline(differences_energy);

#We can see that the distribution is normal

#Exercise 7
Dogs<-read.table("dogs.txt",header = TRUE);

#7.1
boxplot(Dogs$isofluorane,Dogs$halothane,Dogs$cyclopropane)

qqnorm(Dogs$isofluorane);
qqline(Dogs$isofluorane);
#Doesn't seem normal

qqnorm(Dogs$halothane);
qqline(Dogs$halothane);
#Seems normal

qqnorm(Dogs$cyclopropane);
qqline(Dogs$cyclopropane);
#Seems normal

#It is reasonable to say that halothane and cyclopropane come from normal populatino but nor isofluorane

#7.2
#One-way nova (?) Lecture 4 (26)
#Creating data frame Lecture 4 (30)
dogs_frame=data.frame(plasma=as.vector(as.matrix(Dogs)),anesthesia=factor(rep(1:3,each=10)));
dogs_frame

dogs_aov=lm(plasma~anesthesia,data=dogs_frame)
anova(dogs_aov)

#We get a p-value of 0.011. So H0 (mu1=mu2=mu3) is reject.

summary(dogs_aov)
confint(dogs_aov)

#Estimated conecntration ?? 

#7.3 Lecutre 4 (37)
attach(dogs_frame)
kruskal.test(plasma,anesthesia)

#With a p-value of 0.06 we cannot (?) reject the null hypothesis mu1=mu2=mu3. 
#In previous example we could reject the hypothesis but not in this one


### Task 2

First, we check the normality of variable migration.After finding the normality then we can decide which test should we perform for corelation checking.

```{r, echo=FALSE}

qqnorm(migration, main = "QQ - Plot of Migration")

```
```{r, results="hide"}

shapiro.test(migration)
```
The shapiro test gives a p-value of `r shapiro.test(migration)[[3]]` which rejects the null hypothesis that the distribution is normal.
From the above test result and from QQ-Plot, we found that migration does not belong to normal distribution. Therefore, we will use the rank correlation test of Spearman.

```{r, results="hide"}
cor.test(migration, age, method = "spearman")
```
The p-value `r cor.test(migration, age, method = "spearman")[[3]]` indicates that the null hypothesis that the correlation is 0 should be rejected. Therefore, migration and age are correlated.

```{r, results="hide"}
cor.test(migration, weight, method = "spearman")
```
The p-value `r cor.test(migration, weight, method = "spearman")[[3]]` indicates that the null hypothesis that the correlation is 0 should be rejected. Therefore, migration and weight are correlated.

```{r, results="hide"}
cor.test(migration, length, method = "spearman")
```
The p-value `r cor.test(migration, length, method = "spearman")[[3]]` indicates that  we cannot reject that the null hypothesis that the correlation is 0. Therefore, migration and length are not correlated.

```{r, results="hide"}
cor.test(migration, wrist, method = "spearman")
```
The p-value `r cor.test(migration, wrist, method = "spearman")[[3]]` indicates that  we cannot reject that the null hypothesis that the correlation is 0. Therefore, migration and heart rate are not correlated.

```{r, results="hide"}
cor.test(migration, systolic, method = "spearman")
```
The p-value `r cor.test(migration, systolic, method = "spearman")[[3]]` indicates that  we cannot reject that the null hypothesis that the correlation is 0. Therefore, migration and systolic are not correlated.

```{r, results="hide"}
cor.test(migration, diastolic, method = "spearman")
```
The p-value `r cor.test(migration, diastolic, method = "spearman")[[3]]` indicates that  we cannot reject that the null hypothesis that the correlation is 0. Therefore, migration and diastolic are not correlated.
