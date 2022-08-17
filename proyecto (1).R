install.packages("e1071")
install.packages("MASS")

library(e1071)
library(MASS)

Data <- read.csv ("D:\\Documentos\\Universidad\\2?carrera\\Probabilidad y estadistica 2\\Proyecto\\PYE2DataSet31.csv", header=TRUE)
head(Data)

#Parte 1: Identificacion de Modelo y Muestreo 

#-----------------------------------------------------------------------------------------------------------
#summary
summary(Data$sleeptime)
summary(Data$steps)


#hist
hist(Data$sleeptime, main="Histograma de sleeptime", xlab="Sleeptime", ylab="Frecuencia")
hist(Data$steps, main="Histograma de steps", xlab="Steps", ylab="Frecuencia")


#boxplot
boxplot(Data$sleeptime, main="Diagrama de caja de sleeptime", xlab="Sleeptime", ylab="Frecuencia")
boxplot(Data$steps, main="Diagrama de caja de steps", xlab="Steps", ylab="Frecuencia")


#skewness
skewness(Data$sleeptime)
skewness(Data$steps)


#kurtosis
kurtosis(Data$sleeptime)
kurtosis(Data$steps)


#fitdistr
x.sleeptime<-fitdistr(Data$sleeptime, c("normal"))
y.sleeptime<-fitdistr(Data$sleeptime, c("gamma"))
z.sleeptime<-fitdistr(Data$sleeptime, c("exponential"))


x.steps<-fitdistr(Data$steps , c("normal"))
y.steps<-fitdistr(Data$steps , c("gamma"))
z.steps<-fitdistr(Data$steps , c("exponential"))


#kstest
sleeptime.kstest <- ks.test(Data$sleeptime,"pnorm",as.numeric(x.sleeptime$estimate[1]),as.numeric(x.sleeptime$estimate[2]))
print(sleeptime.kstest)

sleeptime.kstest <- ks.test(Data$sleeptime,"pgamma",as.numeric(y.sleeptime$estimate[1]),as.numeric(y.sleeptime$estimate[2]))
print(sleeptime.kstest)

sleeptime.kstest <- ks.test(Data$sleeptime,"pexp",as.numeric(z.sleeptime$estimate[1]))
print(sleeptime.kstest)


steps.kstest <- ks.test(Data$steps,"pnorm",as.numeric(x.steps$estimate[1]),as.numeric(x.steps$estimate[2]))
print(steps.kstest)

steps.kstest <- ks.test(Data$steps,"pgamma",as.numeric(y.steps$estimate[1]),as.numeric(y.steps$estimate[2]))
print(steps.kstest)

steps.kstest <- ks.test(Data$steps,"pexp",as.numeric(z.steps$estimate[1]))
print(steps.kstest)


#Dens
hist(Data$sleeptime,main="Densidad de sleeptime", xlab="Sleeptime", ylab="Frecuencia",probability=TRUE)
hist(Data$steps,main="Densidad de steps", xlab="Steps", ylab="Frecuencia",probability=TRUE)

#-----------------------------------------------------------------------------------------------------------
#Muestreo Medias


#Muestreo30
set.seed(2021)
muestreo30<-replicate(30,sample(Data$Age,200))

x <- seq(1, dim(muestreo30)[1], by = 1)
medias <-x

for (fila in x) {
  medias[fila] <- mean(muestreo30[fila,]) 
}
print(medias)
hist(medias,main="Histograma de 30 muestras Medias", xlab="Medias", ylab="Frecuencia")
boxplot(medias,main="Diagrama de caja de 30 muestras Medias", xlab="Medias", ylab="Frecuencia")
fitdistr(medias , c("normal"))


#Muestreo50
set.seed(2021)
muestreo50<-replicate(50,sample(Data$Age,200))

x <- seq(1, dim(muestreo50)[1], by = 1)
medias <-x

for (fila in x) {
  medias[fila] <- mean(muestreo50[fila,]) 
}
print(medias)
hist(medias,main="Histograma de 50 muestras Medias", xlab="Medias", ylab="Frecuencia")
boxplot(medias,main="Diagrama de caja de 50 muestras Medias", xlab="Medias", ylab="Frecuencia")
fitdistr(medias , c("normal"))


#Muestreo100
set.seed(2021)
muestreo100<-replicate(100,sample(Data$Age,200))

x <- seq(1, dim(muestreo100)[1], by = 1)
medias <-x

for (fila in x) {
  medias[fila] <- mean(muestreo100[fila,]) 
}
print(medias)
hist(medias,main="Histograma de 100 muestras Medias", xlab="Medias", ylab="Frecuencia")
boxplot(medias,main="Diagrama de caja de 100 muestras Medias", xlab="Medias", ylab="Frecuencia")
fitdistr(medias , c("normal"))

#-------------------------------------------------------------------------------------------------------
#Muestreo con Varianza muestral


#Muestreo30
set.seed(2021)
muestreo30<-replicate(30,sample(Data$Age,200))

x <- seq(1, dim(muestreo30)[1], by = 1)
varianza <-x

for (fila in x) {
  varianza[fila] <- var(muestreo30[fila,]) 
}
print(varianza)
hist(varianza,main="Histograma de 30 muestras Varianza muestral", xlab="Varianza muestral", ylab="Frecuencia")
boxplot(varianza,main="Diagrama de caja de 30 muestras Varianza muestral", xlab="Varianza muestral", ylab="Frecuencia")
fitdistr(varianza , c("normal"))


#Muestreo50
set.seed(2021)
muestreo50<-replicate(50,sample(Data$Age,200))

x <- seq(1, dim(muestreo50)[1], by = 1)
varianza <-x

for (fila in x) {
  varianza[fila] <- var(muestreo50[fila,]) 
}
print(varianza)
hist(varianza,main="Histograma de 50 muestras Varianza muestral", xlab="Varianza muestral", ylab="Frecuencia")
boxplot(varianza,main="Diagrama de caja de 50 muestras Varianza muestral", xlab="Varianza muestral", ylab="Frecuencia")
fitdistr(varianza , c("normal"))


#Muestreo100
set.seed(2021)
muestreo100<-replicate(100,sample(Data$Age,200))

x <- seq(1, dim(muestreo100)[1], by = 1)
varianza <-x

for (fila in x) {
  varianza[fila] <- var(muestreo100[fila,]) 
}
print(varianza)
hist(varianza,main="Histograma de 100 muestras Varianza muestral", xlab="Varianza muestral", ylab="Frecuencia")
boxplot(varianza,main="Diagrama de caja de 100 muestras Varianza muestral", xlab="Varianza muestral", ylab="Frecuencia")
fitdistr(varianza , c("normal"))


#------------------------------------------------------------------------------------------------------

#Muestreo con Varones/Mujeres


#Muestreo30
set.seed(2021)
muestreo30<-replicate(30,sample(Data$Sex,200))

x <- seq(1, dim(muestreo30)[1], by = 1)
proporcion <-x

for (fila in x) {
  proporcion[fila] <- mean(muestreo30[fila,]=="V") 
}
print(proporcion)
hist(proporcion,main="Histograma de 30 muestras Proporcion Varones/Mujeres", xlab="Proporcion", ylab="Frecuencia")
boxplot(proporcion,main="Diagrama de caja de 30 muestras Proporcion Varones/Mujeres", xlab="Proporcion", ylab="Frecuencia")
fitdistr(proporcion , c("normal"))


#Muestreo50
set.seed(2021)
muestreo50<-replicate(50,sample(Data$Sex,200))

x <- seq(1, dim(muestreo50)[1], by = 1)
proporcion <-x

for (fila in x) {
  proporcion[fila] <- mean(muestreo50[fila,]=="V") 
}
print(proporcion)
hist(proporcion,main="Histograma de 50 muestras Proporcion Varones/Mujeres", xlab="Proporcion", ylab="Frecuencia")
boxplot(proporcion,main="Diagrama de caja de 50 muestras Proporcion Varones/Mujeres", xlab="Proporcion", ylab="Frecuencia")
fitdistr(proporcion , c("normal"))


#Muestreo100
set.seed(2021)
muestreo100<-replicate(100,sample(Data$Sex,200))

x <- seq(1, dim(muestreo100)[1], by = 1)
proporcion <-x

for (fila in x) {
  proporcion[fila] <- mean(muestreo100[fila,]=="V") 
}
print(proporcion)
hist(proporcion,main="Histograma de 100 muestras Proporcion Varones/Mujeres", xlab="Proporcion", ylab="Frecuencia")
boxplot(proporcion,main="Diagrama de caja de 100 muestras Proporcion Varones/Mujeres", xlab="Proporcion", ylab="Frecuencia")
fitdistr(proporcion , c("normal"))

#-------------------------------------------------
#Parte 2: Estimacion Clasica (puntual, intervalos)
#-------------------------------------------------

#Muestra completa
mean(Data$sleeptime)
var(Data$sleeptime)
mean(Data$steps)
var(Data$steps)

#Muestra de 200
set.seed(2021)
muestraSleep <- sample(Data$sleeptime,200)
muestraStep <- sample(Data$steps,200)


mean(muestraSleep)
var(muestraStep)
mean(muestraStep)
var(muestraSleep)

#Muestra de Mujeres completa
mean(subset(Data,Data$Sex=='M')$sleeptime)
var(subset(Data,Data$Sex=='M')$sleeptime)
mean(subset(Data,Data$Sex=='M')$steps)
var(subset(Data,Data$Sex=='M')$steps)

#Muestra de Mujeres de 200
set.seed(2021)
muestraMujerSleep <- sample(subset(Data,Data$Sex=='M')$sleeptime,200)
muestraMujerStep <- sample(subset(Data,Data$Sex=='M')$steps,200)

mean(muestraMujerSleep)
var(muestraMujerSleep)
mean(muestraMujerStep)
var(muestraMujerStep)

#Muestra de Hombres completa
mean(subset(Data,Data$Sex=='V')$sleeptime)
var(subset(Data,Data$Sex=='V')$sleeptime)
mean(subset(Data,Data$Sex=='V')$steps)
var(subset(Data,Data$Sex=='V')$steps)

#Muestra de Hombres de 200
set.seed(2021)
muestraHombreSleep <- sample(subset(Data,Data$Sex=='V')$sleeptime,200)
muestraHombreStep <- sample(subset(Data,Data$Sex=='M')$steps,200)

mean(muestraHombreSleep)
var(muestraHombreSleep)
mean(muestraHombreStep)
var(muestraHombreStep)

#Estimacion por intervalos, una población con muestras de tamaño 200

#Intervalos de confianza para la media con varianza desconocida

aux_sleep_90 <- t.test(muestraSleep, conf.level = 0.90)$conf.int
IC_Sleep_Mean_90 <- sprintf("%s %s",aux_sleep_90[1],aux_sleep_90[2])
IC_Sleep_Mean_90

aux_step_90 <- t.test(muestraStep, conf.level = 0.90)$conf.int
IC_Step_Mean_90 <- sprintf("%s %s",aux_step_90[1],aux_step_90[2])
IC_Step_Mean_90


aux_sleep_95 <- t.test(muestraSleep, conf.level = 0.95)$conf.int
IC_Sleep_Mean_95 <- sprintf("%s %s",aux_sleep_95[1],aux_sleep_95[2])
IC_Sleep_Mean_95

aux_step_95 <- t.test(muestraStep, conf.level = 0.95)$conf.int
IC_Step_Mean_95 <- sprintf("%s %s",aux_step_95[1],aux_step_95[2])
IC_Step_Mean_95

aux_sleep_99 <- t.test(muestraSleep, conf.level = 0.99)$conf.int
IC_Sleep_Mean_99 <- sprintf("%s %s",aux_sleep_99[1],aux_sleep_99[2])
IC_Sleep_Mean_99

aux_step_99 <- t.test(muestraStep, conf.level = 0.99)$conf.int
IC_Step_Mean_99 <- sprintf("%s %s",aux_step_99[1],aux_step_99[2])
IC_Step_Mean_99

#Intervalos de confianza para la media con varianza conocida

n <- 200

alfa <- 0.1
media <- mean(muestraSleep)
cuantil <- qnorm(1-alfa/2)
desv_tipica <- sqrt(var(muestraSleep))
lim_inf <- media - cuantil * desv_tipica / sqrt(n)
lim_sup <- media + cuantil * desv_tipica /sqrt(n)
IC_Sleep_Mean_conVar_90 <- sprintf("%s %s",lim_inf,lim_sup)
IC_Sleep_Mean_conVar_90

alfa <- 0.1
media <- mean(muestraStep)
cuantil <- qnorm(1-alfa/2)
desv_tipica <- sqrt(var(muestraStep))
lim_inf <- media - cuantil * desv_tipica / sqrt(n)
lim_sup <- media + cuantil * desv_tipica /sqrt(n)
IC_Step_Mean_conVar_90 <- sprintf("%s %s",lim_inf,lim_sup)
IC_Step_Mean_conVar_90

alfa <- 0.05
media <- mean(muestraSleep)
cuantil <- qnorm(1-alfa/2)
desv_tipica <- sqrt(var(muestraSleep))
lim_inf <- media - cuantil * desv_tipica / sqrt(n)
lim_sup <- media + cuantil * desv_tipica /sqrt(n)
IC_Sleep_Mean_conVar_95 <- sprintf("%s %s",lim_inf,lim_sup)
IC_Sleep_Mean_conVar_95

alfa <- 0.05
media <- mean(muestraStep)
cuantil <- qnorm(1-alfa/2)
desv_tipica <- sqrt(var(muestraStep))
lim_inf <- media - cuantil * desv_tipica / sqrt(n)
lim_sup <- media + cuantil * desv_tipica /sqrt(n)
IC_Step_Mean_conVar_95 <- sprintf("%s %s",lim_inf,lim_sup)
IC_Step_Mean_conVar_95

alfa <- 0.01
media <- mean(muestraSleep)
cuantil <- qnorm(1-alfa/2)
desv_tipica <- sqrt(var(muestraSleep))
lim_inf <- media - cuantil * desv_tipica / sqrt(n)
lim_sup <- media + cuantil * desv_tipica /sqrt(n)
IC_Sleep_Mean_conVar_99 <- sprintf("%s %s",lim_inf,lim_sup)
IC_Sleep_Mean_conVar_99

alfa <- 0.01
media <- mean(muestraStep)
cuantil <- qnorm(1-alfa/2)
desv_tipica <- sqrt(var(muestraStep))
lim_inf <- media - cuantil * desv_tipica / sqrt(n)
lim_sup <- media + cuantil * desv_tipica /sqrt(n)
IC_Step_Mean_conVar_99 <- sprintf("%s %s",lim_inf,lim_sup)
IC_Step_Mean_conVar_99


#Intervalos de confianza para la varianza

n <- length(muestraSleep)
alfa <- 0.1
L1 <- (n-1) * var(muestraSleep) / qchisq(1-alfa / 2,n-1)
L2 <- (n-1) * var(muestraSleep) / qchisq(alfa /2,n-1)
IC_Sleep_Var_90 <- c(L1,L2)
IC_Sleep_Var_90

n <- length(muestraStep)
alfa <- 1-0.9
L1 <- (n-1) * var(muestraStep) / qchisq(1-alfa / 2,n-1)
L2 <- (n-1) * var(muestraStep) / qchisq(alfa /2,n-1)
IC_Step_Var_90 <- c(L1,L2)
IC_Step_Var_90

n <- length(muestraSleep)
alfa <- 1-0.95
L1 <- (n-1) * var(muestraSleep) / qchisq(1-alfa / 2,n-1)
L2 <- (n-1) * var(muestraSleep) / qchisq(alfa /2,n-1)
IC_Sleep_Var_95 <- c(L1,L2)
IC_Sleep_Var_95

n <- length(muestraStep)
alfa <- 1-0.95
L1 <- (n-1) * var(muestraStep) / qchisq(1-alfa / 2,n-1)
L2 <- (n-1) * var(muestraStep) / qchisq(alfa /2,n-1)
IC_Step_Var_95 <- c(L1,L2)
IC_Step_Var_95

n <- length(muestraSleep)
alfa <- 1-0.99
L1 <- (n-1) * var(muestraSleep) / qchisq(1-alfa / 2,n-1)
L2 <- (n-1) * var(muestraSleep) / qchisq(alfa /2,n-1)
IC_Sleep_Var_99 <- c(L1,L2)
IC_Sleep_Var_99

n <- length(muestraStep)
alfa <- 1-0.99
L1 <- (n-1) * var(muestraStep) / qchisq(1-alfa / 2,n-1)
L2 <- (n-1) * var(muestraStep) / qchisq(alfa /2,n-1)
IC_Step_Var_99 <- c(L1,L2)
IC_Step_Var_99



#Estimación por intervalos, dos poblaciones

#Intervalos de confianza para la diferencia de medias sin conocer la varianza

lim_inf <- t.test(muestraMujerSleep,muestraHombreSleep,var.equal = TRUE,conf.level = 0.90)$conf.int[1]
lim_sup <- t.test(muestraMujerSleep,muestraHombreSleep,var.equal = TRUE,conf.level = 0.90)$conf.int[2]
IC_2_Sleep_Mean_noVar_90 = sprintf("%s %s",lim_inf,lim_sup)
IC_2_Sleep_Mean_noVar_90

lim_inf <- t.test(muestraMujerStep,muestraHombreStep,var.equal = TRUE,conf.level = 0.90)$conf.int[1]
lim_sup <- t.test(muestraMujerStep,muestraHombreStep,var.equal = TRUE,conf.level = 0.90)$conf.int[2]
IC_2_Step_Mean_noVar_90 = sprintf("%s %s",lim_inf,lim_sup)
IC_2_Step_Mean_noVar_90

lim_inf <- t.test(muestraMujerSleep,muestraHombreSleep,var.equal = TRUE,conf.level = 0.95)$conf.int[1]
lim_sup <- t.test(muestraMujerSleep,muestraHombreSleep,var.equal = TRUE,conf.level = 0.95)$conf.int[2]
IC_2_Sleep_Mean_noVar_95 = sprintf("%s %s",lim_inf,lim_sup)
IC_2_Sleep_Mean_noVar_95

lim_inf <- t.test(muestraMujerStep,muestraHombreStep,var.equal = TRUE,conf.level = 0.95)$conf.int[1]
lim_sup <- t.test(muestraMujerStep,muestraHombreStep,var.equal = TRUE,conf.level = 0.95)$conf.int[2]
IC_2_Step_Mean_noVar_95 = sprintf("%s %s",lim_inf,lim_sup)
IC_2_Step_Mean_noVar_95

lim_inf <- t.test(muestraMujerSleep,muestraHombreSleep,var.equal = TRUE,conf.level = 0.99)$conf.int[1]
lim_sup <- t.test(muestraMujerSleep,muestraHombreSleep,var.equal = TRUE,conf.level = 0.99)$conf.int[2]
IC_2_Sleep_Mean_noVar_99 = sprintf("%s %s",lim_inf,lim_sup)
IC_2_Sleep_Mean_noVar_99

lim_inf <- t.test(muestraMujerStep,muestraHombreStep,var.equal = TRUE,conf.level = 0.99)$conf.int[1]
lim_sup <- t.test(muestraMujerStep,muestraHombreStep,var.equal = TRUE,conf.level = 0.99)$conf.int[2]
IC_2_Step_Mean_noVar_99 = sprintf("%s %s",lim_inf,lim_sup)
IC_2_Step_Mean_noVar_99


#Intervalos de confianza para la diferencia de medias con varianza conocida

muestra1 <- muestraMujerSleep
muestra2 <- muestraHombreSleep
lim_inf <- zsum.test(mean(muestra1),var(muestra1),200,mean(muestra2),var(muestra2),200,conf.level = 0.90)$conf.int[1]
lim_sup <- zsum.test(mean(muestra1),var(muestra1),200,mean(muestra2),var(muestra2),200,conf.level = 0.90)$conf.int[2]
IC_2_Sleep_Mean_conVar_90 <- sprintf("%s %s",lim_inf,lim_sup)
IC_2_Sleep_Mean_conVar_90

muestra1 <- muestraMujerStep
muestra2 <- muestraHombreStep
lim_inf <- zsum.test(mean(muestra1),var(muestra1),200,mean(muestra2),var(muestra2),200,conf.level = 0.90)$conf.int[1]
lim_sup <- zsum.test(mean(muestra1),var(muestra1),200,mean(muestra2),var(muestra2),200,conf.level = 0.90)$conf.int[2]
IC_2_Step_Mean_conVar_90 <- sprintf("%s %s",lim_inf,lim_sup)
IC_2_Step_Mean_conVar_90

muestra1 <- muestraMujerSleep
muestra2 <- muestraHombreSleep
lim_inf <- zsum.test(mean(muestra1),var(muestra1),200,mean(muestra2),var(muestra2),200,conf.level = 0.95)$conf.int[1]
lim_sup <- zsum.test(mean(muestra1),var(muestra1),200,mean(muestra2),var(muestra2),200,conf.level = 0.95)$conf.int[2]
IC_2_Sleep_Mean_conVar_95 <- sprintf("%s %s",lim_inf,lim_sup)
IC_2_Sleep_Mean_conVar_95

muestra1 <- muestraMujerStep
muestra2 <- muestraHombreStep
lim_inf <- zsum.test(mean(muestra1),var(muestra1),200,mean(muestra2),var(muestra2),200,conf.level = 0.95)$conf.int[1]
lim_sup <- zsum.test(mean(muestra1),var(muestra1),200,mean(muestra2),var(muestra2),200,conf.level = 0.95)$conf.int[2]
IC_2_Step_Mean_conVar_95 <- sprintf("%s %s",lim_inf,lim_sup)
IC_2_Step_Mean_conVar_95

muestra1 <- muestraMujerSleep
muestra2 <- muestraHombreSleep
lim_inf <- zsum.test(mean(muestra1),var(muestra1),200,mean(muestra2),var(muestra2),200,conf.level = 0.99)$conf.int[1]
lim_sup <- zsum.test(mean(muestra1),var(muestra1),200,mean(muestra2),var(muestra2),200,conf.level = 0.99)$conf.int[2]
IC_2_Sleep_Mean_conVar_99 <- sprintf("%s %s",lim_inf,lim_sup)
IC_2_Sleep_Mean_conVar_99

muestra1 <- muestraMujerStep
muestra2 <- muestraHombreStep
lim_inf <- zsum.test(mean(muestra1),var(muestra1),200,mean(muestra2),var(muestra2),200,conf.level = 0.99)$conf.int[1]
lim_sup <- zsum.test(mean(muestra1),var(muestra1),200,mean(muestra2),var(muestra2),200,conf.level = 0.99)$conf.int[2]
IC_2_Step_Mean_conVar_99 <- sprintf("%s %s",lim_inf,lim_sup)
IC_2_Step_Mean_conVar_99


#Intervalo de confianza para la diferencia de varianzas

lim_inf <- var.test(muestraMujerSleep,muestraHombreSleep,conf.level = 0.90)$conf.int[1]
lim_sup <- var.test(muestraMujerSleep,muestraHombreSleep,conf.level = 0.90)$conf.int[2]
IC_2_Sleep_Var_90 <- sprintf("%s %s",lim_inf,lim_sup)
IC_2_Sleep_Var_90

lim_inf <- var.test(muestraMujerStep,muestraHombreStep,conf.level = 0.90)$conf.int[1]
lim_sup <- var.test(muestraMujerStep,muestraHombreStep,conf.level = 0.90)$conf.int[2]
IC_2_Step_Var_90 <- sprintf("%s %s",lim_inf,lim_sup)
IC_2_Step_Var_90

lim_inf <- var.test(muestraMujerSleep,muestraHombreSleep,conf.level = 0.95)$conf.int[1]
lim_sup <- var.test(muestraMujerSleep,muestraHombreSleep,conf.level = 0.95)$conf.int[2]
IC_2_Sleep_Var_95 <- sprintf("%s %s",lim_inf,lim_sup)
IC_2_Sleep_Var_95

lim_inf <- var.test(muestraMujerStep,muestraHombreStep,conf.level = 0.95)$conf.int[1]
lim_sup <- var.test(muestraMujerStep,muestraHombreStep,conf.level = 0.95)$conf.int[2]
IC_2_Step_Var_95 <- sprintf("%s %s",lim_inf,lim_sup)
IC_2_Step_Var_95

lim_inf <- var.test(muestraMujerSleep,muestraHombreSleep,conf.level = 0.99)$conf.int[1]
lim_sup <- var.test(muestraMujerSleep,muestraHombreSleep,conf.level = 0.99)$conf.int[2]
IC_2_Sleep_Var_99 <- sprintf("%s %s",lim_inf,lim_sup)
IC_2_Sleep_Var_99

lim_inf <- var.test(muestraMujerStep,muestraHombreStep,conf.level = 0.99)$conf.int[1]
lim_sup <- var.test(muestraMujerStep,muestraHombreStep,conf.level = 0.99)$conf.int[2]
IC_2_Step_Var_99 <- sprintf("%s %s",lim_inf,lim_sup)
IC_2_Step_Var_99






