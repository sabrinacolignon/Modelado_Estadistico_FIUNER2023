library(data.table)
library(fastDummies)
library(caret)
library(pROC)
library(dplyr)
library(scales)
# Cargamos el dataset que vamos a utilizar

Ac_Cardiaco <- fread("./heart_failure_clinical_records_dataset.csv")

Ac_Cardiaco$DEATH_EVENT <- as.factor(Ac_Cardiaco$DEATH_EVENT)

#EXPLORACION DE LOS DATOS
#-----------------------------------------------------------------------------------------------------------------------------------------------
#Comenzamos recorriendo un poco el dataset e investigando sobre los datos y variables 
#número de filas y columnas
dim(Ac_Cardiaco)
#nombres de las columnas
names(Ac_Cardiaco)
#primeras filas del dataset
head(Ac_Cardiaco)
#Información del dataset
str(Ac_Cardiaco)
#resumen estadístico
summary(Ac_Cardiaco)


#Tablas de frecuencia

table(Ac_Cardiaco$anaemia)
table(Ac_Cardiaco$diabetes)
table(Ac_Cardiaco$high_blood_pressure)
table(Ac_Cardiaco$sex)
table(Ac_Cardiaco$smoking)


#GRÁFICOS:

#Edad según el sexo:
ggplot(data = Ac_Cardiaco, aes(x = sex, y = age, group = sex)) +
  geom_boxplot(fill = c("violet", "green"))+
  labs(title = "Boxplot edad paciente") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  labs(x = "Sexo del paciente", y = "Edad")

#Distribución segun el sexo 
ggplot(data = Ac_Cardiaco, aes(x = sex)) + geom_bar(fill = c("violet", "green"))+
  labs(title = "Gráfico de barras según el sexo del paciente") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  labs(x = "Sexo", y = "Frecuencia absoluta")

#Fumador
ggplot(data = Ac_Cardiaco, aes(x = smoking)) + geom_bar(fill = c("#87CEFA", "#8B3A3A"))+
  labs(title = "Gráfico de barras según condición de fumador") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  labs(x = "Fumador", y = "Frecuencia absoluta")

#Hipertensión
ggplot(data = Ac_Cardiaco, aes(x = high_blood_pressure)) + geom_bar(fill =  c("#87CEFA", "#8B3A3A"))+
  labs(title = "Gráfico de barras según condición de hipertensión") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  labs(x = "Hipertensión", y = "Frecuencia absoluta")


#Tiempo internado y supervivencia
ggplot(data = Ac_Cardiaco, aes(x = DEATH_EVENT, y = time, group = DEATH_EVENT)) +
  geom_boxplot(fill = c("#87CEFA", "#8B3A3A"))+
  labs(title = "Boxplot tiempo internado vs supervivencia") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  labs(x = "Fallecimiento luego de la obs", y = "Tiempo de observación")

#Distribución segun fallecimiento
ggplot(data = Ac_Cardiaco, aes(x = DEATH_EVENT)) + geom_bar(fill = c("#87CEFA", "#8B3A3A"))+
  labs(title = "Gráfico de barras según condición de supervivencia") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  labs(x = "Fallecimiento", y = "Frecuencia absoluta")
#------------------------------------------------------------------------------------------------------------------------------------------

#DESARROLLO DEL MODELO



# Inicializamos la semilla aleatoria
set.seed(1234)

#Definimos la proporción utilizada para entrenar
prop <- 0.7 # 70/30

# Encontramos el número de observaciones de los datos
N <- nrow(Ac_Cardiaco)

#Definimos el tamaño de la partición
tamanio <- floor(prop*N) #Se utiliza la función floor para redondear decimales

#Encontramos el conjunto de indices de entrenamiento
cardio.indices<- sample(seq_len(N),size = tamanio)


#MODELO 1: TODAS LAS VARIABLES

#Asignamos el tipo de variable necesario para crear las variables DUMMIES
Ac_Cardiaco$anaemia <- as.factor(Ac_Cardiaco$anaemia)
Ac_Cardiaco$diabetes <- as.factor(Ac_Cardiaco$diabetes)
Ac_Cardiaco$high_blood_pressure <- as.factor(Ac_Cardiaco$high_blood_pressure)
Ac_Cardiaco$sex <- as.factor(Ac_Cardiaco$sex)
Ac_Cardiaco$smoking <- as.factor(Ac_Cardiaco$smoking)
Ac_Cardiaco$DEATH_EVENT <- as.factor(Ac_Cardiaco$DEATH_EVENT)


#Utilizamos variables DUMMIES para las de tipo categórica:
Ac_Cardiaco_dummies <- dummy_cols(Ac_Cardiaco, 
                               select_columns = c( "anaemia","diabetes", "high_blood_pressure","sex","smoking"),
                               remove_selected_columns = TRUE)

##Creación de los conjuntos de entrenamiento y testeo (VALIDACION SIMPLE)
cardio.train <- Ac_Cardiaco_dummies[cardio.indices,] #Entrenamiento
cardio.test <- Ac_Cardiaco_dummies[-cardio.indices,] #Testeo


#Creamos el modelo
modelo_AC_Dum <- glm(DEATH_EVENT~., cardio.train, family = "binomial")
summary(modelo_AC_Dum)

#Testeamos el modelo realizando una predicción
ACDUM.pred <- predict(modelo_AC_Dum, cardio.test, type="response")

#Gráfica de la curva ROC
#Curva ROC
pred.modelo_dum <- ifelse(ACDUM.pred > 0.5,1,0)
plot.roc(pred.modelo_dum, as.numeric(cardio.test$DEATH_EVENT))

#Area bajo la curva
auc(pred.modelo_dum,as.numeric(cardio.test$DEATH_EVENT)) #AUC = 0.754

#Matriz de confusión para corroborar la clasificación
matriz_conf_ACD <- confusionMatrix(factor(pred.modelo_dum), factor(cardio.test$DEATH_EVENT))
matriz_conf_ACD$table
#Podemos ver que el modelo tiene una Accuracy = 0.78
#Además de una Sensibilidad = 0.85 y una Especificidad = 0.64

#--------------------------------------------------------------------------

#MODELO 2: SOLO VARIABLES NUMERICAS
#Utilizando los mismos conjuntos de testeo y entrenamiento

#Creamos el modelo
modelo_AC_num <- glm(DEATH_EVENT~age+creatinine_phosphokinase+ejection_fraction+platelets+serum_creatinine+serum_sodium+time, cardio.train, family = "binomial")
summary(modelo_AC_num)

#Testeamos el modelo realizando una predicción
ACNUM.pred <- predict(modelo_AC_num, cardio.test, type="response")

#Gráfica de la curva ROC
#Curva ROC
pred.modelo_num <- ifelse(ACNUM.pred > 0.5,1,0)
plot.roc(pred.modelo_num, as.numeric(cardio.test$DEATH_EVENT))

#Area bajo la curva
auc(pred.modelo_num,as.numeric(cardio.test$DEATH_EVENT)) #AUC = 0.806

#Matriz de confusión para corroborar la clasificación
matriz_conf_ACN <- confusionMatrix(factor(pred.modelo_num), factor(cardio.test$DEATH_EVENT))
matriz_conf_ACN$table
#Podemos ver que el modelo tiene una Accuracy = 0.833
#Además de una Sensibilidad = 0.887 y una Especificidad = 0.71





