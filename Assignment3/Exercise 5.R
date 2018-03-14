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

label_medicin <- function(label, count){
  
   med_label <- as.vector(rep(label, count))
   
   return(med_label)
  
}

# Total count of medicines for No nausea
C_nonausea = 100
P1_nonausea = 32
P2_nonausea = 48

# Total count of medicines for Nausea
C_nausea =  52
P1_nausea = 35
P2_nausea = 37


medicin_vector = c(label_medicin("C", C_nonausea), label_medicin("P1", P1_nonausea), label_medicin("P2", P2_nonausea), label_medicin("C", C_nausea), label_medicin("P1", P1_nausea), label_medicin("P2", P2_nausea))
medicin_vector


nausea.frame = data.frame(nausea_vector, medicin_vector)

# We assigned labels "C" for Chlorpromazine, "P1" for Pentobarbital(100mg) and "P2" for Pentobarbital(150mg). 
# We could have assigned same label for both doses of Pentobarbital. But we want to be specific so we used different labels for each one.

nausea.frame
# head(nausea.frame)
# tail(nausea.frame)


##T2
xtabs(~medicin_vector+nausea_vector, data = nausea.frame)


# In this case R does not give the warning, since the approximation is reliable for
# nausea_vector and medicin_vector in nausea.frame

##T3
# Check Lecture 5 page : 1-12

mystat = function(x) sum(residuals(x)^2)
B = 1000
tstar =  numeric(B)
for (b in 1:B) {
 medicinstar = sample(medicin_vector)  # Permutting the medicin labels
 tstar[b] = mystat(lm(nausea_vector~medicinstar))
}

myt = mystat(lm(nausea_vector~medicin_vector))

hist(tstar)
myt # value = 71.82106

pl = sum(tstar<myt)/B
pl # p-value = 0.032
pr = sum(tstar>myt)/B
pr # p-value = 0.967
2*pl # 2*p-value = 0.064

chisq.test(xtabs(~medicin_vector+nausea_vector, data = nausea.frame))

# p-value = 0.03643

chisq.test(xtabs(~medicin_vector+nausea_vector, data = nausea.frame))[[1]]

# x-squared = 6.624765

### Findings: I don't know what should we say/conclude here for this task!

##T4

## If we look at the p-values for both permutation test and chisq test we can seee that,
## for both of the tests we have same p-values that is 0.03

