install.packages("e1071")
install.packages("MASS")
install.packages("lmtest")
install.packages("stats")

library(stats)
library(lmtest)
library(e1071)
library(MASS)

Data <- read.csv ("C:\\Users\\yihui\\Desktop\\University resources\\SECOND YEAR\\2º Cuatrimestre\\PYE2\\PYE2DataSet31.csv", header=TRUE)
head(Data)

#-----------------------------------------------------------------------------------------------------------------

#PARTE 4 : Contrastes (parametricos y no parametricos)

# 4.1 Contrastes Parametricos


set.seed(2021)
M1<-sample(Data$IMC,size=200,replace=FALSE)  # Muestra 1
M2<-sample(Data$IMC,size=200,replace=FALSE)  # Muestra 2
um1<-mean(M1)        # media de la muestra 1 (24.44653)
dtm1<-sd(M1)         # desv tipica de la muestra 1
vm1<-var(M1)         # varianza de la muestra 1
um2<-mean(M2)        # media de la muestra 2 (24.37199)
cuant<-quantile(M1)  # vector de cuantiles

# EJ1: H0: Q1 <= um1

q1<-cuant[2]         # Cuantil 1 (25%)

sol.test=t.test(M1,mu=q1,alternative="less",conf.level = 0.95) # calculamos T con nivel de confianza del 5%
sol.test$statistic   # valor estadistico del contraste
sol.test$p.value     # valor del p
sol.test$conf.int[1] # extremo izqd del intervalo de confianza 
sol.test$conf.int[2] # extremo dcho del intervalo de confianza
print(t.test(M1,mu=q1,alternative="less",conf.level = 0.95))

# INTERVALO = (-INF, 24.53514)
# CONCLUSION: como um1 se encuentra DENTRO del intervalo de confianza, 
# Q1<=um1 es cierto

# EJ2: H0: um1 <= Q3

q3<-cuant[4]
sol.test=t.test(M1,mu=q3,alternative="greater",conf.level = 0.95)
sol.test$statistic   # valor estadistico del contraste
sol.test$p.value     # valor del p
sol.test$conf.int[1] # extremo izqd del intervalo de confianza 
sol.test$conf.int[2] # extremo dcho del intervalo de confianza
print(t.test(M1,mu=q3,alternative="greater",conf.level = 0.95))
# INTERVALO = (-24.35791 , Inf )

# CONCLUSION: como um1 se encuentra FUERA del intervalo de confianza, 
# H0: um1 <= Q3 NO es cierto, se acepta en su lugar um1 > Q3

# EJ3: H0: varm1 > 1, que es equivalente a decir H1: varm1 <= 1; con que probamos que la varianza es igual a 1 podemos rechazar H0

aux<-c(1, 2, 3)      #vector auxiliar con varianza = 1
var(aux)
sol.var.test=var.test(M1,aux,ratio=1,alternative="two.sided",conf.level=0.95)
sol.var.test$statistic
sol.var.test$p.value
print(var.test(M1,aux,ratio=1,alternative="two.sided",conf.level=0.95))

# Como el valor P es 0.357 mayor que 0.05 (nivel de conf), se da la conclusión de que varm1 es igual que 1
# H0: varm1 > 1 es falso

# EJ4: H0: um1 = um2

sol.test<-t.test(M1,M2,alternative="two.side",var.equal=TRUE,conf.level=0.95)
sol.test$statistic
sol.test$p.value
sol.test$conf.int[1]
sol.test$conf.int[2]
print(t.test(M1,M2,alternative="two.side",var.equal=TRUE,conf.level=0.95))
# INTERVALO = (-0.0688 , 0.2178)

# Como el valor P es 0.0307, que se encuentra fuera del intervalo de confianza,
# H0: um1 = um2 NO es cierto y es rechazado consecuentemente

# EJ5: H0: vm1 = vm2

sol.var.test=var.test(M1,M2,ratio=1,alternative="two.sided",conf.level=0.95)
sol.var.test$statistic
sol.var.test$p.value
print(var.test(M1,M2,ratio=1,alternative="two.sided",conf.level=0.95))

# Como el valor P es 0.2465 mayor que 0.05 (nivel de conf), se da la conclusión de que varm1 es igual que 1
# H0: varm1 = varm2 NO es cierto y es rechazado consecuentemente


print(q1[2])         # cuartil 1
print(um1)           # media de la muestra 1
print(dtm1)
print(um2)           

# 4.2 Contrastes NO Parametricos

#Contraste de la normalidad con Pearson

chisq.test(M1)

#Como el valor de p es mucho mayor que a=0.05, por lo que no rechazamos la h0

#Contraste de la normalidad con Kolmogorov-Smirnov

ks.test(M1, pnorm, mean(M1), sd(M1))

#Como el valor de p es mucho mayor que a=0.05, por lo que no rechazamos la h0

#Contraste de la independencia con Durbin-Watson

set.seed(2021)
m200 <- sample(1:nrow(Data),size=200,replace=FALSE)
m200df <- Data[m200,]
dwtest(IMC ~weight, alternative = "two.sided", data = m200df)

dwtest(IMC ~height, alternative = "two.sided", data = m200df)
dwtest(IMC ~height + weight, alternative = "two.sided", data = m200df)
dwtest(IMC ~height / weight^2, alternative = "two.sided", data=Data200)

#En conclusión, la independencia de la variable IMC junto con las otras está justificada
set.seed(2021)
m200a <- sample(1:nrow(Data),size=200,replace=FALSE)
m200b <- sample(1:nrow(Data),size=200,replace=FALSE)
m200adf <- Data[m200a,]
m200bdf <- Data[m200b,]
wilcox.test(m200adf$IMC,m200bdf$IMC,conf.level = 0.95)
