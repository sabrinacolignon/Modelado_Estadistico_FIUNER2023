#Para encontrar la correlación entre dos variables, vamos a utilizar las funciones
#de la instalación base de R: cor() y cor.test().
#Los parámetros que vamos a utilizar para las dos funciones son:
#x: vector, matriz o data.frame numérico.
#y: vector, matriz o data.frame numérico de la misma dimensión que x (opcional).
#method: donde le indicamos que coeficiente de correlación
#obtendremos, por defecto el valor es “pearson”, pero se pueden utilizar
#“spearman” y “kendall”

#EJERCICIO 1:
peso <- c(74, 92, 63, 72,58 , 76, 85, 78, 67, 91, 85, 73, 62, 80, 72)
altura <- c(168, 196, 170, 175, 162, 183, 169, 190, 172, 188, 186, 176, 170, 176, 179)


plot(altura~peso, pch=2, col="red")
plot(peso,alura, col="red")

#supuesto de normalidad
shapiro.test(peso)
shapiro.test(altura)

# si los puntos no estan muy separados de la recta de qline es de distribucion normal
qqnorm(peso)
qqline(peso)
qqnorm(altura)
qqline(altura)

#supuesto de homocedasticidad
var.test(peso, altura) #compara las varianzas de las dos variables, el cociente, si es 1 las variables son iguales encontces hay homeostacidad7

#compruebo correlacion
cor(peso, altura)
cor.test(peso, altura) #-> devuelve el pvalor para comprobar la hipotesis nula y el cor es el calor de la correlacion
#cor.test(peso, altura, method="spearman")-> rho es la correlacion

#como el valor de correlacion es 0.75 y pvalue=0.0011 -> hay correlacion lineal

#-------------------------------------------------------------------------------------------------------------------
#EJERCICIO 2:
datos <- read.csv(file="C:./datos/bank.csv", sep = ";", stringsAsFactors = T)
is.na(datos)
table(is.na(datos))


#Encuentre si existe una relación lineal entre las variables “age” y “balance”. 

edad <- c(datos$age)
balance <- c(datos$balance)

plot(balance~edad)

qqnorm(edad)
qqline(edad)
qqnorm(balance)
qqline(balance)

shapiro.test(edad)
shapiro.test(balance)

var.test(balance, edad)

cor.test(balance, edad) 

#Hipotesis nula: es normal
#al ser pvalor muy cercano a cero, la distribucion de edad y balance no es normal

#-------------------------------------------------------------------------------------------------------------------
#EJERCICIO 3:
library (MASS)
data(mammals)

#a. Explore los datos. ¿Puede utilizar el coeficiente de Pearson?
p_cuerpo <- c(mammals$body)
p_cerebro <- c(mammals$brain)

#supuesto de normalidad
shapiro.test(p_cuerpo)
shapiro.test(p_cerebro)

#supuesto de homocedasticidad
var.test(p_cuerpo, p_cerebro)

#b. Si los supuestos se cumplen, encuentre el coeficiente de correlación. Si
#no, utilice la rho de Spearman. ¿Qué tipo de correlación existe?

#al ser pvalue muy chico, no se cumple el supuesto de normalidad
cor.test( p_cerebro,p_cuerpo, method="spearman")

#rho = 0.9534986 
  
#c. Realice un escalamiento de los datos con el objetivo de “mejorar” la
#distribución de los datos y repita el análisis de correlación.

p_cuerpo_escala <- log(p_cuerpo)
p_cerebro_escala <- log(p_cerebro)

plot(p_cerebro_escala ~ p_cuerpo_escala)

#repito analisis de correlacion
#supuesto de normalidad
shapiro.test(p_cuerpo_escala)
shapiro.test(p_cerebro_escala)
#supuesto de homocedasticidad
var.test(p_cuerpo_escala, p_cerebro_escala)

cor.test(balance, edad) #cor = 0.9534

#conclusion: existe la relacion lineal positiva fuerte despues de ser estandarizada

#-------------------------------------------------------------------------------------------------------------------
#EJERCICIO 4:
library (datasets)
data(Orange)
#plot(circumference~age)

#a. Explore los datos. 
edad <- c(Orange$age)
circunferencia <- c(Orange$circumference)

#supuesto de normalidad
shapiro.test(edad) #-> p-value = 0.007118 no esta taaaaaan lejos de la normalidad
shapiro.test(circunferencia)

#supuesto de homocedasticidad
var.test(edad, circunferencia)

#Encuentre la correlación entre las variables:

plot(edad ~ circunferencia)

cor.test(edad, circunferencia) 
#cor = 0.9135189
#conclusion: existe correlacion lineal Positiva fuerte

