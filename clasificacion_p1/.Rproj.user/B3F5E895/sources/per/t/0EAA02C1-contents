#Ejercicio 1 – Un estudio quiere establecer un modelo que permita calcular la
#probabilidad de obtener una beca para la universidad, a alumnos en el último
#año de la escuela secundaria, en función de la nota que obtuvieron en
#matemática. La variable beca está codificada como 0 si no aplica a la beca y 1
#si aplica. Los datos del estudio se encuentran disponibles en el archivo
#“beca.csv”.
#Utilice validación simple del tipo 70%-30%, 80%-20% y 90%-10% para
#encontrar el modelo. Grafique y evalúe los modelos con los datos de test
#mediante la curva ROC y el AUC. ¿Con que porcentaje de validación obtiene el
#mejor resultado?

library("data.table")
install.packages("caret")
library("caret")
install.packages("pROC")
library("pROC")
becas <- fread("./datos/beca.csv", sep=";")

#validacion simple
#fijo semilla aleatoria
set.seed(1234)

#proporcion 70-30
porc <- 0.7
#encuentro el n de observaciones de los datos
N <- nrow(becas)
#tamaño de la particion de entenamiento
tamanio <- floor(porc*N)
#encuentro el conjunto de indices de entrenamiento
train.ind <- sample(seq_len(N), size=tamanio)

#separo los datos
becas.train <- becas[train.ind,]
becas.test <- becas[-train.ind,]

#veo proporcion de la clase
table(becas.train$beca)
table(becas.test$beca)

#encuentro modelo de regresion logistica
modelo.becas <- glm(beca~matematica, becas.train, family="binomial")
summary(modelo.becas)

#grfico modelo
plot(beca~matematica, becas.train, col=factor(becas.train$beca))

#agrego funcion ajustada
curve(1/(1+exp(-modelo.becas$coefficients[1]-modelo.becas$coefficients[2]*x)), add=TRUE, col="gray20")

#predecimos:
becas.pred <- predict(modelo.becas, becas.test, type="response")

pred.modelo -- ifelse(becas.pred>0.5,1,0)
#evaluo el modelo
plot.roc(pred.modelo,becas.test$beca)

#area bajo la curva
auc(pred.modelo,becas.test$beca)

#----------------------------------------
#proporcion 80-20
porc2 <- 0.8
tamanio2 <- floor(porc2*N)
train.ind2 <- sample(seq_len(N), size=tamanio2)

becas2.train <- becas[train.ind2,]
becas2.test <- becas[-train.ind2,]

modelo2.becas <- glm(beca~matematica, becas2.train, family="binomial")
plot(beca~matematica, becas2.train, col=factor(becas2.train$beca))

becas2.pred <- predict(modelo2.becas, becas2.test, type="response")
pred2.modelo <- ifelse(becas2.pred>0.5,1,0)
plot.roc(pred2.modelo, becas2.test$beca)
auc(pred2.modelo, becas2.test$beca)

#---------------------------------------
#proporcion 90-10
porc3 <- 0.9
tamanio3 <- floor(porc3*N)
train.ind3 <- sample(seq_len(N), size=tamanio3)

becas3.train <- becas[train.ind3,]
becas3.test <- becas[-train.ind3,]

modelo3.becas <- glm(beca~matematica, becas3.train, family="binomial")
plot(beca~matematica, becas3.train, col=factor(becas3.train$beca))

becas3.pred <- predict(modelo3.becas, becas3.test, type="response")
pred3.modelo <- ifelse(becas3.pred>0.5,1,0)
plot.roc(pred3.modelo, becas3.test$beca)
auc(pred3.modelo, becas3.test$beca)

#------------------------------- EJERCICIO 2 -----------------------------------
rm(list=ls())
gc()

install.packages("ISLR")
library("ISLR")
datos <- Default
datos$default <- ifelse(datos$default=="Yes",1,0)
set.seed(1234)

N2 <- nrow(datos)

porc4 <- 0.8
tamanio4 <- floor(porc4*N2)

train.ind4 <- sample(seq_len(N2), size=tamanio4)

datos.train <- datos[train.ind4,]
datos.test <- datos[-train.ind4,]

#datos de entrenamiento
modelo.datos <- glm(default~income, datos.train, family="binomial")
summary(modelo.datos)
plot(default~income, datos.train, col=factor(datos.train$default))

datos.pred <- predict(modelo.datos, datos.test, type="response")
pred.modelo <- ifelse(datos.pred>0.5,1,0)
plot.roc(pred.modelo, datos.test$default)
auc(pred.modelo, datos.test$default)
#no se puede graficar porque la preduccion devuelve una sola categoria y necesito dos

#cambio el modelo:
modelo2.datos <- glm(default~balance, datos.train, family="binomial")
summary(modelo2.datos)
plot(default~balance, datos.test, col=datos.test$default)

datos.pred2 <- predict(modelo2.datos, datos.test, type="response")
pred2.modelo <- ifelse(datos.pred>0.5,1,0)
summary(pred2.)
plot.roc(pred2.modelo, datos.test$default)
auc(pred2.modelo, datos.test$default)



