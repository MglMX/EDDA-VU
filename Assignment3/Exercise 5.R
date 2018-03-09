###EXERCISE 5
library(multcomp)
library(lme4)

##T1
nauseadata = read.table("nauseatable.txt", header = TRUE)
attach(nauseadata)
nauseadata

nausea = as.vector(rep(0:1, 152)) # 304 or 152 or ??
nausea
medicin = as.vector(rep(1:2, each = 152)) # I have no idea about how to define medicin labels!!
medicin

nausea.frame = data.frame(nausea, medicin)
# nausea.frame If I try to print all data from that frame I'm getting more than 304 rows why??
nausea.frame[1:304,]


##T2
xtabs(~medicin+nausea, data = nausea.frame)

##T3


##T4