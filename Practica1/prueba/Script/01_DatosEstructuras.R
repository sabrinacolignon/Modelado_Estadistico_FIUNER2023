#------------------------------------------------------------------------------------
# Tipos de datos numéricos
a <- 2
b = 2.5
c <- 2L
class(a)
class(b)
class(c)

d <- a + b + c
d
class(d)
# Cadenas de texto
aa <- "Hola!"
aa
bb <- "Esto es una prueba..."
class(aa)
cc <- paste(aa,',',bb)
dd <- paste0(aa,', ',bb)

verdadero <- F
nrow(a)
#------------------------------------------------------------------------------------
# NA y NULL
# Valor faltante
e <- c(0, 1, 4, NA, 7.5)
e
# Dato no disponible o nulo
nrow(d)
# Vectores
v1 <- c(1,0.5,3,7)
v1
v1[2]
length(v1)
v2 <- c("Hola","Modelos","Estadísticos")
v2
v2[3]
#------------------------------------------------------------------------------------
# Tamaño del vector
length(v2)
# Otra forma
v3 <- 1:10
v3
# Con funciones
v4 <- seq(from = 0,
          to = 100,
          by = 2)
v4
v5 <- rep(x = 0, 
          times = 100)
v5
#------------------------------------------------------------------------------------
# Factor
m <- factor(x = c(1,1,2,3,4,5,3,4,2,5,5,4),
            levels = c(1,2,3,4,5)
            )
m
plot(m)
#------------------------------------------------------------------------------------
# Matrices
m1 <- matrix(1:15)
m1
m2 <- matrix(1:15, nrow = 3, ncol = 5)
m2
m3 <- matrix(1:15, nrow = 3, ncol = 5, byrow = T)
m3
# Dimensión de la matríz
dim(m3)
#------------------------------------------------------------------------------------
# Data Frames
nombre <- c("Leandro","Agustín","Cristian","Carolina","Nahuel","Ariana")
edad <- c(25,26,35,31,32,27)
cohorte <- factor(c(1,2,2,1,2,2),levels = c(1,2,3))

alumnos <- data.frame("nombre" = nombre,
                      "edad" = edad,
                      "cohorte" = cohorte,
                      stringsAsFactors = F)

alumnos
# Acceso a los elementos
alumnos$nombre
alumnos[[1]]
alumnos$nombre[2]
alumnos[, 1]
alumnos[, "nombre"]
alumnos[1, 1]
aux <- alumnos[, c(1,3)]
aux
aux <- alumnos[, 1:3]
aux
dim(alumnos)
# Agregamos una columna nueva
ciudad <- c("Paraná","Paraná","Oro Verde","Santa Fe","Gualeguay","CABA")

alumnos <- cbind(alumnos[,1:2],"ciudad" = ciudad, cohorte)
alumnos
# Agregamos un alumno
nuevo <- data.frame("Juan",45,"Oro Verde",as.factor(2),
                    stringsAsFactors = F)
colnames(nuevo) <- c("nombre","edad","ciudad","cohorte")
alumnos <- rbind(alumnos,nuevo)
alumnos
# Dimensión
dim(alumnos)
#------------------------------------------------------------------------------------
# Listas
l1 <- list(v1,v2,m,m3,alumnos)
# Acceso a los valores
l1
l1[[1]]
l1[[1]][2]
l1[[4]]
l1[[5]][,c(1,3)]
l1[[5]]$nombre[7]
l1[[5]][[1]][7]
l2 <- list(l1,1:15)
l2
l2[[1]][[5]]
l2[[1]][[2]]
l2[[2]]
l3 <- list("v1" = v1, "DF" = alumnos)
l3$v1
l3$DF$nombre
#------------------------------------------------------------------------------------
# Eliminar elementos
rm(list = ls()) # Con ls() eliminamos todos los elementos del entorno

