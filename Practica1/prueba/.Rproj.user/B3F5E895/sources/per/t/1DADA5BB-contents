#– Abra el archivo de datos “bank.csv”
datos <- read.csv(file="C:./datos/bank.csv", sep = ";", stringsAsFactors = T)

#A qué tipo de datos corresponde cada variable
head(datos)
class(datos$age) #de a una variable

str(datos) #todas las variables juntas

#Los parámetros estadísticos típicos de las variables numéricas.
summary(datos)
summary(datos$age)
summary(datos$balance)
summary(datos$day)
summary(datos$duration)
summary(datos$campaign)
summary(datos$pdays)
summary(datos$previous)

#Contiene datos faltantes en alguna de las variables?
is.na(datos)
table(is.na(datos))

#Realice una función que tome los datos correspondientes al archivo
#abierto, seleccione una variable numérica y una categórica
#(interesantes) y devuelva un nuevo conjunto de datos que contenga
#estas dos variables.

source("C:./Funciones/Funcion_guia1.R")
#v_num <- colnames (datos$age)
#v_cat <- datos$marital
nuevo <- conjunto_datos(datos, colnames(datos[1]), colnames(datos[4]))
summary(nuevo)

#e. Realice un gráfico completo (título, etiquetas y color) de las variables
#obtenidas antes y explique la relación.

plot(nuevo$education, nuevo$age)
boxplot(formula=age ~ education,
        data=nuevo,
        xlab="educacion",
        ylab="edad",
        main="educacion vs. edad",
        col=rainbow(4))












