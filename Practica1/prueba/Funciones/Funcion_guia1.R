#Realice una función que tome los datos correspondientes al archivo
#abierto, seleccione una variable numérica y una categórica
#(interesantes) y devuelva un nuevo conjunto de datos que contenga
#estas dos variables.

conjunto_datos <- function(archivo, c_num, c_cat){
  aux <- archivo[, c(c_num, c_cat)]
  result <- aux 
}
