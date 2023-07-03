#------------------------EJERCICIO 1 --------------------------

peso <- c(74, 92, 63, 72,58 , 76, 85, 78, 67, 91, 85, 73, 62, 80, 72)
altura <- c(168, 196, 170, 175, 162, 183, 169, 190, 172, 188, 186, 176, 170, 176, 179)

datos <- data.frame(peso, altura)

plot(peso~altura, pch=1, col="red")

modelo <- lm(peso~altura, data=datos)

class(modelo)

modelo$coefficients
modelo$model

abline(modelo, col="blue")

summary(modelo)

shapiro.test(modelo$residuals)

hist(modelo$residuals)

#predecimos la altura:
altura.new <- data.frame(altura=177)
peso.new <- predict(object=modelo, newdata=altura.new)

points(peso.new~altura.new$altura, pch=17, col="green")

#---------------- EJERCICIO 2 -----------------------------

datos_2 <- read.table (file="C:/Users/Sabrina/Downloads/Modelado estadistico/clase4/datos/datos.txt", header = TRUE)

#a. Encuentre un modelo de regresión que permita predecir el perímetro de
#la cintura. El modelo es significativo?

Peso <- datos_2$Peso
Cintura <- datos_2$Cintura


plot(Peso~Cintura, pch=1, col="red")

datos_dataframe <- data.frame(Peso, Cintura)

modelo_dataframe <- lm(Peso~Cintura, data=datos_2)

class(modelo_dataframe)

modelo_dataframe$coefficients
modelo_dataframe$model

abline(modelo_dataframe, col="blue")

summary(modelo_dataframe)

shapiro.test(modelo_dataframe$residuals)

hist(modelo_dataframe$residuals)


#d. ¿Cómo cambia la variable de respuesta respecto a la explicativa?
#deberia ser lineal

#------------------- EJERCICIO 3 -------------------------------

library (datasets)
data(faithful)

#Encuentre un modelo para predecir la duración de la próxima erupción
#del geyser según el tiempo de espera.

Erupciones <- faithful$eruptions
Espera <- faithful$waiting


plot(Erupciones~Espera, pch=1, col="red")


faithful_dataframe <- data.frame(Erupciones, Espera)

faithful_dataframe <- lm(Erupciones~Espera, data=faithful_dataframe) #MODELO

abline(faithful_dataframe, col="blue")

summary(faithful_dataframe)


#c. Use el modelo obtenido para predecir la duración de la erupción, si el
#tiempo de espera fue:

Espera <- c(58, 67,2, 75, 92)

tiempo.new <- data.frame(Espera)

erupciones.new <- predict(object=faithful_dataframe, newdata=tiempo.new)

points(erupciones.new~tiempo.new$Espera, pch=17, col="green")

#d. ¿Cómo describiría el comportamiento de las erupciones del geyser de
#acuerdo con el tiempo de espera?
#segun el tiempo de espera, las erupciones del geyser aumentan 

#--------------------------- EJERCICIO 4 ---------------------------------

datos_supermercado <- read.csv(file="C:/Users/Sabrina/Downloads/Modelado estadistico/clase4/datos/supermercados.csv", sep = ";", stringsAsFactors = T)

summary(datos_supermercado)

#MODELO TV-VENTAS
ventas_tv_dataframe <- data.frame( ventas=datos_supermercado$ventas, tv=datos_supermercado$TV)

plot(ventas_tv_dataframe$ventas~ventas_tv_dataframe$TV, pch=1, col="red")

ventas_tv_modelo <- lm(ventas ~ tv, data=ventas_tv_dataframe) #MODELO

abline(ventas_tv_modelo, col="blue")

#prediccion:
tv.new <- data.frame(tv=c(125.55, 234.00))
ventas.new <- predict(object = ventas_tv_modelo, newdata = tv.new)
points(ventas.new ~ tv.new$tv, pch=17, col="green")

#MODELO RADIO-VENTAS:

ventas_radios_dataframe <- data.frame( ventas=datos_supermercado$ventas, radios=datos_supermercado$radios)

plot(ventas_radios_dataframe $ ventas~ventas_radios_dataframe$radios, pch=1, col="red")

ventas_radios_modelo <- lm(ventas ~ radios, data=ventas_radios_dataframe) #MODELO

abline(ventas_radios_modelo, col="blue")

#prediccion:
radios.new <- data.frame(radios=c(23.40, 89.70))
ventas.new <- predict(object = ventas_radios_modelo, newdata = radios.new)
points(ventas.new ~ radios.new$radio, pch=17, col="green")

#MODELO DIARIOS-VENTAS:

ventas_diarios_dataframe <- data.frame( ventas=datos_supermercado$ventas, diarios=datos_supermercado$diarios)

plot(ventas_diarios_dataframe $ ventas~ventas_diarios_dataframe$diarios, pch=1, col="red")

ventas_diarios_modelo <- lm(ventas ~ diarios, data=ventas_diarios_dataframe) #MODELO

abline(ventas_diarios_modelo, col="blue")

#prediccion:
diarios.new <- data.frame(diarios=c(23.40, 89.70))
ventas.new <- predict(object = ventas_diarios_modelo, newdata = diarios.new)
points(ventas.new ~ diarios.new$diarios, pch=17, col="green")





