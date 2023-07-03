rm(list = ls())
gc()
#----
#Ejercicio 1
peso <- c(74,92,63,72,58,76,85,78,67,91,85,73,62,80,72)
altura <- c(168,196,170,175,162,183,169,190,172,188,186,176,170,176,179)
datos <- data.frame(peso,altura)
#
plot(peso~altura, pch = 1, col = "red")
#a) Encontramos el modelo
modelo <- lm(peso~altura, data = datos)
# Vemos el objeto "modelo"
class(modelo)
modelo$coefficients
modelo$model
# 
abline(modelo, col="blue")
summary(modelo)
#
# peso = -68.5574 + 0.8107 * altura
# el modelo es significativo: p-valor = 0.001174
#b) Vemos los residuos
shapiro.test(modelo$residuals) # p-valor = 0.09237. No rechazamos Ho => normales

#c) Predecimos para una altura = 177
altura.new <- data.frame(altura = 177)
#Predecimos
peso.new <- predict(object = modelo, newdata = altura.new)
#Graficamos el punto nuevo
points(peso.new~altura.new$altura, pch = 17, col = "green")
