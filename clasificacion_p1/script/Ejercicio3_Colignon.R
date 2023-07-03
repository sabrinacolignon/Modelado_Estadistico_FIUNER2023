#Ejercicio 3 – Según datos relevados de la UNER (archivo “c_consulta.csv”), se
#supone que existe una relación entre el hecho de que un estudiante asista a
#clases de consulta de estadística (Si = 1, No = 0), la nota obtenida en el primer
#parcial de la materia y el sexo del estudiante.
  #a. Explore los datos. De qué tipo son las variables?
  #b. Genere un modelo con las variables adecuadas que prediga la
  #probabilidad de que el estudiante tenga que asistir a clases de consulta.
#Evalúe el modelo encontrado y encuentre el porcentaje de datos bien clasificados.

#cargo librerias:
library(data.table)
library(caret)
library(pROC)
library(dplyr)

#fijo semilla: 
set.seed(1234)

#cargo los datos:
datos <- fread("C:/Users/Sabrina/Downloads/Modelado estadistico/clasificacion_p1/datos/c_consulta.csv")

#a. Explore los datos. De qué tipo son las variables?

str(datos) #las variables son de tipo: chr, num e int

table(datos$clases_consulta) #obtengo que hay 130 "no" y 59 "si"
table(datos$sexo) #obtengo que tengo 93 hombres y 96 mujeres

barplot(table(datos$clases_consulta), ylim = c(0, 140), col = 2:8)
barplot(table(datos$sexo), ylim = c(0, 100), col = 2:8)

#b. Genere un modelo con las variables adecuadas que prediga la probabilidad de que el estudiante tenga que asistir a clases de consulta.

#proporcion 70-30
porc <- 0.7

#encuentro el numero de observaciones de los datos
N <- nrow(datos)

#tamaño de la particion de entenamiento
tamanio <- floor(porc*N)

#encuentro el conjunto de indices de entrenamiento
train.ind <- sample(seq_len(N), size=tamanio)

#separo los datos
datos.train <- datos[train.ind,]
datos.test <- datos[-train.ind,]

#encuentro el modelo de regresion logistica:
modelo <- glm(clases_consulta~examen_estadistica, datos.train, family="binomial")
summary(modelo)

#grafico el modelo
plot(clases_consulta~examen_estadistica, datos.train, col=factor(datos.train$clases_consulta))

#realizo prediccion:
prediccion <- predict(modelo, datos.test, type="response")
pred.modelo <- ifelse(prediccion > 0.5, 1, 0)

#evaluo el modelo:
plot.roc(pred.modelo, datos.test$clases_consulta)

#area bajo la curva:
auc(pred.modelo , datos.test$clases_consulta)
#Area under the curve: 0.8839

#realizo la matriz de confusion:
matriz_conf <- confusionMatrix(factor(pred.modelo), factor(datos.test$clases_consulta))
matriz_conf$table

#             Reference
#Prediction     0  1
#          0    43 13
#          1    0  1

#Luego de realizar los análisis correspondientes, obtuve que mi modelo es muy sensible pero poco específico, ya que la sensitividad es 1
#y la especificidad es de 0,07, por ende podemos concluir que se clasifican bien los valores positivos pero no los negativos