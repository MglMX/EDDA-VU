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
Light1879<-read.table("light1879.txt");
Light1882<-read.table("light1882.txt",fill = TRUE);
Light<-read.table("light.txt");


