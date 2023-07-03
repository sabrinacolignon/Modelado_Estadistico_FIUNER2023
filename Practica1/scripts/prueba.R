#tipos de datos numericos
a <- 2
b = 2.5
c <- 2L
class(a) #saber tipo de dato, en este casu numeric
class(b)
class(c)
d <- a +b-c
d
class(d)

#cadenas de texto
aa <- "hola"
bb <- "pruebas de texto"
cc <- paste(aa, ',', bb) #cocatenar
dd <- paste0(aa, ',', bb) #cocatenar sin espacio

verdadero <- T #o TRUE
falso <- FALSE

#NA - NULL: Valor faltante(na), dato que no existe(null)
#Valor faltante
e <- c(1,0,4,NA,7.5)#c=cocatenacion para armar vector
nrow(d)#dato no disponible o nulo

#vectores
v1 <- c(1,0,4,NA,7.5)
v1[2]
lenght(v1)
v2 <- c("hola", "eee")
v3 <- 1:10#vector de num consecutivos
v4 <- sec(from:0, to:100,b:=2)#secuencia desde donde hasta donde y el paso
v5 <- rep(x=0, times=100)#es un vector de valores repetidos, la repeticion es por el parametro times

#factor
m <- factor(x=c(1,2,1,4,5,5,1,2), levels=c(2,7,4,5))
#x=vector, los niveles estan dados por levels
plot(m)#graticar vector

#Matrices
m0 <- matrix(1:15)#matriz columna
m1 <- matrix(1:15, nrow=3, ncol=5)
m2 <- matrix(1:15, nrow=3, ncol=5, byrow=T)#orden creciente por renglones
dim(m2)

#data frames
nombre <- c("sabrina", "fran","mica", "enzo")
edad <- c(23,25,25,24,26)
cohorte <- factor(c(3,3,2,1), levels=c(1,2,3))
alumnos <- data.frame("nombre" <- nombre,
                      "edad" <- edad,
                      "cohorte" <- cohorte,
                      stringsAsFactors = F) #f porque el vector de nombre al ser char me lo va a cambair a factor entonces le tengo que decir que no lo haga
alumnos

#acceso a elementos
alumnos$nombre
alumnos[[1]]#columna 1
alumnos$nombre[2]
alumnos[,1]
alumnos[,"nombre"]
alumnos[1,1]
aux <- alumnos[,c(1,3)]
dim(alumnos)
aux <- alumnos[, 1:3]

#agregar columna nueva
ciudad <- c("pna", "dte", "stafe")
alumnos <- cbind(alumnos[,1:2], "ciudad"=ciudad, cohorte)

#agregar alumno
nuevo <- data.frame("huan", 45, "oro verde", as.factor(2), stringsAsFactors = F)
colnames(nuevo) <- c("Nombre", "edad", "ciudad", "cohorte")
alumnos <- rbind(alumnos, nuevo)

dim(alumnos)

#Listas
l1 <- list(v1,v2,m0,m2,alumnos)
l1
l1[[1]]
l1[[1]][2]
l1[[4]]
l1[[5]][,c(1,3)]
l1[[5]]$nombre[7]
l1[[5]][[1]][7]

l2 <- list(l1, 1:15)
l2[[1]][[5]]
l3 <- list("v1"=v1, "DF"=alumnos)
l3$v1
l3$DF$nombre

#eliminar elementos
rm(list=ls()) #rm=remove


#funciones
#suma va en un script aparte
suma <- funcion(a,b){
  c <- a+b
  result <- c }# es el return
#en el script donde estoy trabajando debo llamar a la funcion
source("ruta de la funcion")
a <- 3
b <- 3
c <- suma(a.b)

#graticos de paquete base
f <- 5
ini <- 0
fin <- 10
x <- seq(from=ini, to=(fin-(1/f)), by=(1/f))
senial <- sin(2*pi*x)
senial2 <- cos(2*pi*x)
plot(x,senial, type="l", col="red")
lines(x, senial2, col="blue")
abline(h=0)
abline(v=c(1,2,5), color="green")
abline(a=-1, b=2, col="red")
#grafico valores de la funciones mayores o iguales a 0.95
pos <- wich(senial>=0.99)
points(x[pos], senial[pos], pch=21, col="black")

#cargo data set iris
data(iris)
summary(iris)
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species,
      xlab = "sepal len", ylab = "sepal width")
title("iris")
plot(iris[,1:4], col=iris$Species)

#USAR DATOS DESDE ARCHIVO:
datos <- read.csv(file="C:/Users/Sabrina/Downloads/Modelado estadistico/bank.csv", sep = ";", stringsAsFactors = T)
datos <- read.csv(file="C:./datos/bank.csv", sep = ";", stringsAsFactors = T)

library(data.table)
installed.packages("data.table")
datos2=fread(file="C:/Users/Sabrina/Downloads/Modelado estadistico/bank.csv", sep=";")

head(datos)
tail(datos)

#histograma
hist(x=datos$age)
hist(x=datos$duration,
     xlab="duracion",
     ylab="frecuencia",
     main="histograma",
     color="blue")

#barras
plot(datos$month)
datos$month <- factor(datos$month)

#barplot
tabla <- table(datos$month)
barplot(tabla)
barplot(c(2,4,12))

#bastones
anio <- 2010:2019
ventas <- c()
plot(anio, ventas, type="h")

#boxplot
plot(datos$marital, datos$age)
boxplot(formula=age ~ marital,
        data=datos,
        col=c("red", "blue", "green3"))
boxplot(datos$age)

#guaradr archivo csv
datos.exp <- data.frame(tiempo=x, mV=senial)
fwrite(x=datos.exp, file="./salidas/senial.csv", sep=";")

#exportar graficos
png(filename="./salidas/senial01.png", width = 800, height = 600)
plot(x, senial, type="l", color="red")
abline(h=0)
points(x[pos], senial[pos], pch=19, color="blue")
dev.off()



