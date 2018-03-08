###EXERCISE 2

##T1

search = read.table("search.txt", header=TRUE)
attach(search)

#gotta do the randomization again but still don't get how that works 
#like in the last exercise


##T2
interaction.plot(skill,interface,time)
interaction.plot(interface,skill,time)

#The lines seems unparallel which indicates that there might be interaction

##T3

search$skill = as.factor(search$skill)
search$interface = as.factor(search$interface)

searchaov = lm(time~interface,data=search)
anova(searchaov)

#H0 mu0=mu1=mu2=mu3=mu4 cannot be rejected.
#I used 1 way anova because searchaov = lm(time~interface*skill,data=search) was not returning p-value.


##T4


##T5


##T6


##T7


