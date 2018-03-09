###EXERCISE 5
library(multcomp)
library(lme4)

##T1
nauseadata = read.table("nauseatable.txt", header = TRUE)
attach(nauseadata)
nauseadata

nonausea_detected = nauseadata[, 1]
nonausea_detected

nausea_detected = nauseadata[, 2]

#nausea = as.vector(rep(0:1, 152)) # 304 or 152 or ??
nausea_detected

nausea = as.vector(rep(1, each = nausea_detected[1]+nausea_detected[2]+nausea_detected[3]))
nausea

nonausea = as.vector(rep(0, each = nonausea_detected[1]+nonausea_detected[2]+nonausea_detected[3]))
nonausea

nausea_vector = c(nonausea, nausea)
nausea_vector

Chlorpromazine_nonausea = as.vector(rep("C", 100))
Chlorpromazine_nonausea

Pentobarbital_nonausea_1 = as.vector(rep("P1", 32))
Pentobarbital_nonausea_1

Pentobarbital_nonausea_2 = as.vector(rep("P2", 48))
Pentobarbital_nonausea_2

Chlorpromazine_nausea = as.vector(rep("C", 52))
Chlorpromazine_nausea

Pentobarbital_nausea_1 = as.vector(rep("P1", 35))
Pentobarbital_nausea_1

Pentobarbital_nausea_2 = as.vector(rep("P2", 37))
Pentobarbital_nausea_2

medicin_vector = c(Chlorpromazine_nonausea, Pentobarbital_nonausea_1, Pentobarbital_nonausea_2, Chlorpromazine_nausea, Pentobarbital_nausea_1, Pentobarbital_nausea_2)
medicin_vector


# medicin = as.vector(rep(1:2, each = 152)) # I have no idea about how to define medicin labels!!
# medicin

nausea.frame = data.frame(nausea_vector, medicin_vector)
# nausea.frame If I try to print all data from that frame I'm getting more than 304 rows why??

nausea.frame


##T2
xtabs(~medicin_vector+nausea_vector, data = nausea.frame)
chisq.test(xtabs(~medicin_vector+nausea_vector, data = nausea.frame))[[1]]

# In this case R does not give the warning, since the approximation is reliable for
# nausea_vector and medicin_vector in nausea.frame

##T3
# Check Lecture 5 page : 1-12

##T4