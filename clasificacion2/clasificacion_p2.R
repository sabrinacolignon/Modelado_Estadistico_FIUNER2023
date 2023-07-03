library("data.table")
library("caret")
install.packages("fastDummies")
library("fastDummies")
install.packages("caTools")
library("caTools")
library("MASS")
install.packages("e1071")
library("e1071")
install.packages ("naivebayes")
library("naivebayes")

library("pROC")

install.packages("readxl")
library("readxl")

#--------------------------------------- EJERCICIO 2 ---------------------------------------------

datos <- read_excel("./datos/demencia.xls")
datos2 <- dummy_cols(datos, select_columns = c("WINE","HIGHBP"),
                     remove_selected_columns = T
                     )

datos2$T3DEMEN <- as.factor(datos2$T3DEMEN)
str(datos2)
set.seed(1234)
porc <- 0.8
N <- nrow(datos2)
tamanio <- floor(porc*N)
train.ind <- sample(seq_len(N), size=tamanio)
datos2.train <- datos2[train.ind,2:9]
datos2.test <- datos2[-train.ind,2:9]
#encuentro modelo de regresion logistica
modelo.datos <- glm(T3DEMEN~., datos2.train, family="binomial")
summary(modelo.datos)

demen.pred <- predict(modelo.datos, datos2.test, type="response")
demen.pred <-  ifelse(demen.pred>0.5,1,0)


plot.roc(demen.pred,datos2.test$T3DEMEN)

matriz_conf <- confusionMatrix(factor(demen.pred), factor(datos2.test$T3DEMEN))
matriz_conf$table

#asigna a un nuevo caso con los siguientes datos: AGE= 70; WINE= 1; MMSE= 25; HIGHBP= 1?

dato_nuevo <- data.frame(
                         AGE= 70,
                         MMSE= 25,
                         WINE_0=0,
                         WINE_1= 1, 
                         WINE_2=0,
                         HIGHBP_0= 0,
                         HIGHBP_1= 1)
caso_nuevo <- predict(modelo.datos, newdata=dato_nuevo, type="response")
#0,043

#--------------------------------------- EJERCICIO 8  ---------------------------------------------

datos3 <- fread("./datos/trenes.txt")

set.seed(1234)

datos3.train <- datos3[train.ind,]

datos3.test <- datos3[-train.ind,]

modelo.trenes <- naiveBayes(puntualidad~., data=datos3.train)

trenes.predict <- predict(modelo.trenes, datos3.test)

matriz_conf2 <- confusionMatrix(factor(trenes.predict), factor(datos3.test$puntualidad))
matriz_conf2$table
