#ejercicio 1
library("data.table")
library("ppcor")
library("corrplot")

#cargo los datos
datos <- fread("./datos/supermercados.csv")

#a)
datos.cor <- cor(datos[, 2:5])
datos.cor
#correlaciones parciales
pcor(x=datos[, 2:5])
corrplot(datos.cor, method="number", bg="grey")

#b)
m4 <- lm(ventas~TV + radios + diarios, data=datos)
summary(m4) #r ajustado: 0.8956 

#c)
ventas_tv_modelo <- lm(ventas ~ TV, data=datos) #MODELO
ventas_radio_modelo <- lm(ventas~radios, data=datos)
ventas_diarios_modelo <- lm(ventas~diarios, data=datos)

summary(ventas_tv_modelo) #r ajustado: 0.6099
summary(ventas_radio_modelo) #r ajustado: 0.3287
summary(ventas_diarios_modelo) #r ajustado: 0.04733

#usando mas variables explicativas, el modelo es mucho mejor que usando una sola, 
#por eso comparo el rajustado del modelo de todas las variables con el de modelo de variables
#simples.

#d)
#prediccion:

m4.new <- data.frame(TV=c(125.55, 234.00), radios=c(23.40, 85.5), diarios=c(89.70, 65))
ventas.new <- predict(object = m4, newdata = m4.new)
#veo los resultados
ventas.new

#------------------------------------- EJERCICIO 2 -----------------------------------------
#cargo los datos
datos_tortugas <- fread ("./datos/tortugas.csv")

#b)
datos_tortugas.cor <- cor(datos_tortugas[, 2:6])
datos_tortugas.cor

#correlaciones parciales
pcor(x=datos_tortugas[, 2:6])
corrplot(datos_tortugas.cor, method="number", bg="grey")

# modelo de regresión para predecir el número de especies endémicas de tortuga
tortugas_modelo <- lm(Endemics~Area + Elevation + Nearest + Scruz + Adjacent, data=datos_tortugas)

summary(tortugas_modelo)

#variables más significativas que explican el modelo: elevation y adjacent
pcor(datos_tortugas)

#------------------------------------- EJERCICIO 3 ------------------------------------------
datos_cosecha <- fread ("./datos/produccion.csv")

datos_cosecha.cor <- cor(datos_cosecha[, 2:10])
datos_cosecha.cor

#correlaciones parciales
pcor(x=datos_cosecha[, 2:10])
corrplot(datos_cosecha.cor, method="circle", bg="grey") #bg=background color

#modelo de regresión utilizando los datos de cada mes
#(temperatura y precipitaciones) para predecir la producción:

cosecha_may_modelo <- lm(produccion~premay + tempmay, data=datos_cosecha)
cosecha_jun_modelo <- lm(produccion~prejun + tempjun, data=datos_cosecha)
cosecha_jul_modelo <- lm(produccion~prejul + tempjul, data=datos_cosecha)
cosecha_ago_modelo <- lm(produccion~preago + tempago, data=datos_cosecha)

summary(cosecha_may_modelo) #Adjusted R-squared:  -0.02086
summary(cosecha_jun_modelo) #Adjusted R-squared:  0.00336
summary(cosecha_jul_modelo) #Adjusted R-squared:  0.4085 
summary(cosecha_ago_modelo) #Adjusted R-squared:  0.07108

#modelo de regresión lineal que explique la producción de
#cereales utilizando los datos de todos los meses registrados

modelo_total <- lm (produccion~premay + tempmay + prejun + tempjun + prejul + tempjul + preago + tempago, data=datos_cosecha)
summary(modelo_total)


#------------------------------------- EJERCICIO 4 ------------------------------------------
data(state)
datos_state <- as.data.frame(state.x77)

colnames(datos_state) <- c("habitantes","ingresos", "analfabetismo", "esp_vida", "asesinatos", "universitarios", "heladas", "area")

str(datos_state)
datos_state.cor <- cor(datos_state)
corrplot(datos_state.cor, method="circle", bg="grey")

#modelo para predecir la expectativa de vida
modelo_vida <- lm(esp_vida~habitantes+ingresos+analfabetismo+asesinatos+universitarios+heladas+area, data=datos_state)
summary(modelo_vida) #Adjusted R-squared:  0.6922

#el modelo es significativo y las variables que mas lo explican son 

#nuevo modelo con las variables más significativas:
nuevo_modelo <- lm(esp_vida~habitantes+asesinatos+universitarios+heladas, data=datos_state)
summary(nuevo_modelo)#Adjusted R-squared:  0.7126 
