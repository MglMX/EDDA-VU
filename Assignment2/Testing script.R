#Exercise 1
#1.1
Data <- read.table("telephone.txt",header = TRUE);
bills = Data$Bills;

t=median(bills)
t

hist(bills,prob=T);

B=10000;
tstar=numeric(B);
n=length(bills);
n

for(i in 1:B){
  xstar = rexp(n,0.1); #Exercise says for lambda = [0.01,0.1]
  tstar[i] = median(xstar);
}

hist(tstar,prob=T);

pl=sum(tstar<t)/B;
pr=sum(tstar>t)/B;
p=2*min(pl,pr);
p;pl;pr;

#With a p value of 0 we reject the hypothesis that the distribution is exponential
#1.2
hist(bills)
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
par(mfrow=c(1,2));
hist(light1879);
boxplot(light1879);

hist(light1882);
boxplot(light1882);

hist(light_speed);
boxplot(light_speed);

#Obersvations: ??

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
  return(c(2*T1-tstar975,2*T1-tstar25));
}

#Light1879
confidence_mean = get_confidence_intervals(light1879);
confidence_mean+299000;

confidence_median = get_confidence_intervals(light1879,"median");
confidence_median+299000;

#Light1882
confidence_mean = get_confidence_intervals(light1882);
confidence_mean+299000;

confidence_median = get_confidence_intervals(light1882,"median");
confidence_median+299000;

#Light
confidence_mean = get_confidence_intervals(light_speed);
confidence_mean+299000;

confidence_median = get_confidence_intervals(light_speed,"median");
confidence_median+299000;

#2.3 Comments
#??

#2.4
#Speed of light according to https://physics.nist.gov/cgi-bin/cuu/Value?c
#299792.458
# For light1879 the value doesn't fall in the interval neither mean or median
# For light1882 the value doesn't fall in the interval neither mean or median
# For light it also doesn't fall in the interval neither mean or median
#It is not consistent

#Exercise 3
#3.1
klm = scan("klm.txt");
klm
hist(klm)
qqnorm(klm)

#We check that the population doesn not seem normal this tell us that doing the t-test is not the best option
#We should perform the sign test Lecture 3 (13)
#Wilcoxon test doesn't seem appropiate because the population is not symetric

#Sign test
#We compute how many values are bigger than the median to test
n_bigger = sum(31>=klm)
n = length(klm)
binom.test(n_bigger,n,p=0.5)

median(klm) #Checking


#We can reject the null hypothesus that the median of the population is == 31. 
# How to change the null hypothesis to check if population is <= than 31?
# Checking confidence interval it gives 0.21 and 0.46. Can we say with that info the median is bigger than 31?

#3.2
quantile(klm,probs=0.9) #$Just checking

n_bigger = sum(72>klm);
binom.test(n_bigger,n,p=0.9)

#With a p value of 0.002 we can reject that at most 10% of the parts arrive later than 72 days. 
# If we check the confidence intervals 0.64 and 0.87, this means that between 13% and 36% of the pieces arrive later than 72 days (Can we say this?)




