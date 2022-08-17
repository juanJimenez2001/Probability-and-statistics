install.packages("e1071")
install.packages("MASS")

library(e1071)
library(MASS)

Data <- read.csv ("D:\\Documentos\\Universidad\\2ºcarrera\\Probabilidad y estadistica 2\\Proyecto\\PYE2DataSet31.csv", header=TRUE)
head(Data)

#Parte 1: Identificacion de Modelo y Muestreo 

#-----------------------------------------------------------------------------------------------------------
#summary
summary(Data$sleeptime)
summary(Data$steps)
help(summary)


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
muestreo30<-replicate(30,sample(Data$Sex,200))
print(muestreo30)
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

