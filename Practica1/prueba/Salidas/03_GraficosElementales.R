# Borramos la memoria
rm(list = ls())
# Creamos una señal
f <- 50
ini <- 0
fin <- 10
x <- seq(from = ini, to = (fin - (1/f)), by = (1/f))
senial <- sin(2 * pi * x)
senial2 <- cos(2 * pi * x)
# Gráfica base tipo línea
plot(x, senial, type = "l", col = "red")
# Agregamos a la misma gráfica
lines(x, senial2, col = "blue")
# Líneas horizontales, verticales u oblicuas
abline(h = 0)
abline(v = c(1,3,5), col = "green")
abline(a = -1, b = 2, col = "red")
# Vamos a encontrar las posiciones en la señal que son 
# mayores o igual a 0.95 con la función which
pos <- which(senial >= 0.99)
# Agregamos puntos
points(x[pos],senial[pos], pch =  21, col = "black")
# Scatterplot
data(iris) #Cargamos el dataset Iris del paquete base
# Resumen de medidas estadísticas de los datos
summary(iris)
# Ancho del sepalo vs. largo del sepalo
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, 
     xlab = "Sepal length", ylab = "Sepal width")
title("Iris")
# Scatterplot solo de los datos numéricos
plot(iris[,1:4], col = iris$Species) # Coloreamos por clase
#------------------------------------------------------------------------------------
# Usar datos desde un archivo
# Datos: Bank Marketing Data Set
# Fuente: https://archive.ics.uci.edu/ml/datasets/Bank+Marketing
# Con el paquete base
datos <- read.csv(file = "./datos/bank.csv", 
                  sep = ";",
                  stringsAsFactors = T)
# Con data.table
library("data.table")
# Para instalar paquetes:
# install.packages("data.table")
datos2 <- fread(file = "./datos/bank.csv", sep = ";")
# Vemos los primeros valores
head(datos)
tail(datos)
# Graficamos un campo: Edad y duración
hist(x = datos$age)
# Agregamos parámetros
hist(x = datos$duration, 
     xlab = "Duración", 
     ylab = "Frecuencia", 
     main = "Histograma de Duración",
     col = "blue")
# Gráfica de barras
plot(datos$month)

datos$month <- factor(datos$month)
# Con barplot
tabla <- table(datos$month)
barplot(tabla)
barplot(c(2,4,12))
# Bastones
anio <- 2010:2019
ventas <- c(1.6,2.1,3,5.2,7,3.7,2.9,4,5.8,6.3)
plot(anio, ventas, type = "h", lwd = 3, col = 2:11)
plot(tabla)
#------------------------------------------------------------------------------------
# Grafico de cajas
# Distribución de la edad según el estado civil
plot(datos$marital, datos$age)
# Con boxplot()
boxplot(formula = age ~ marital, 
        data = datos, 
        col = c("red","blue","green3"))
boxplot(datos$age)
#------------------------------------------------------------------------------------
# Exportar resultados
# Archivo CSV
datos.exp <- data.frame(tiempo = x, mV = senial)
# Con fwrite del paquete data.table
fwrite(x = datos.exp, file = "./salidas/senial.csv", sep = ";")
# Exportar gráficos
# Pueden ser varias, las más usadas son:
# jpg() y png()
png(filename = "./salidas/senial01.png", width = 800, height = 600)
plot(x, senial, type = "l", col = "red")
abline(h = 0)
points(x[pos],senial[pos], pch =  19, col = "blue")
dev.off()
