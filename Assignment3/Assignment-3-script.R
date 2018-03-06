bread = read.table("bread.txt", header=TRUE)
attach(bread)

N=3
I=3
J=2

rbind(rep(1:I,each=N*J),rep(1:J,N*I),sample(1:(N*I*J)))

boxplot(hours~environment,data=bread)
boxplot(hours~humidity,data=bread)

interaction.plot(humidity,environment,hours)
interaction.plot(environment,humidity,hours)

bread$humidity=as.factor(bread$humidity)
bread$environment=as.factor(bread$environment)
breadaov=lm(hours~humidity*environment,data=bread)
anova(breadaov)

#environment y humidity tienen main effect y hay evidencias de interaccion. 
#porque p value es muy pequeño y h0 es que no hay interaccion.

contrasts(bread$humidity)=contr.sum
contrasts(bread$environment)=contr.sum
breadaov2=lm(hours~humidity*environment,data=bread)
summary(breadaov2)

qqnorm(residuals(breadaov2))
qqline(residuals(breadaov2))
plot(fitted(breadaov2),residuals(breadaov2))

#qq datos en los extremos son demasiado extremos outliers
#plot todo datos similares y algun outlier en el 200

