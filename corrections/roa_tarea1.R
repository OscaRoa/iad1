# library(tidyverse)
rm(list=ls())
theta <- 1/5 # Probabilidad de un exito
n <- 30 # Ensayos

# Variable aleatoria, posibles exitos en n ensayos
x <- 0:n

coef_bin <- function(a,b) {
  # Calcular coeficiente binomial
  # Devuelve ya sea un numero entero o un vector de enteros.
  return(factorial(a)/(factorial(b)*factorial(a-b)))
}
# coef_bin(3,2) Number operation
# coef_bin(n, x) Vector operation

binomial_dist <- function(n, theta, x) {
  # Funcion que calcula la distribucion binomial de un vector de datos 'X'
  # con una probabilidad 'theta' de exito en 'n' ensayos.
  # Devuelve un vector con los valores de la distribucion.
  bin_coef <- coef_bin(n, x)
  distribution <- bin_coef*((theta**x)*(1 - theta)**(n-x))
  return(distribution)
}

expected_value <- function(x, theta) {
  # Esperanza matematica distribucion binomial
  return(x*theta)
}

standard_deviation <- function(theta, e_x) {
  # Desviacion estandar distribucion binomial
  v <- e_x*(1 - theta)  # Varianza
  return(v**(1/2))
}

# Guardar el vector de la distribucion para graficarlo posteriormente
p_dist <- binomial_dist(n, theta, x)
esperanza <- expected_value(n, theta)
sd_x <- standard_deviation(theta, esperanza)

# Las soluciones numéricas son correctas, y la organización
# de todo en funciones también está chida. Tienes 3 de 3.
#
# Como sospechabas, el punto del ejercicio era enfatizar la definición
# general de esperanza, varianza etc., es decir, estaba esperando
# una suma sobre la multiplicación de dos vectores etc. 
# La que propones, en cambio, únicamente es válida para la dist. binomial 
# (estoy asumiendo que sabes por qué, en esta distribución, la esperanza
# equivale a n*theta, y que tienes bien clara la diferencia entre 'n' como
# *parámetro* de la distribución, y 'x' como *argumento* en tu función 'expected_value';
# lo mismo en la definición de la varianza), y tocará cambiar las 
# fórmulas al trabajar con otras distribuciones.
#
# Para responder a tu pregunta, la manera de hacerlo general:
E_X <- sum(x*p_dist) # Sólo necesitas el vector con los valores de la variable
# y el vector con sus valores de probabilidad, multiplicarlos e integrar esos productos.
V_X <- sum(p_dist*(E_X-x)^2) # La varianza es la esperanza de las diferencias al 
# cuadrado entre cada valor de la variable y su esperanza...
sd_X <- V_X^(1/2) # ...y la desviación es la raíz cuadrada de la varianza.
#
# Ps., no tengo instalado tidyverse y ando medio apurado, luego me 
# muestras cómo se ve ese ggplot.


tibb_data <- tibble(x, p_dist)
ggplot(data = tibb_data, aes(x = x, y = p_dist)) +
  geom_bar(fill = "#7cc992", stat = "identity", width = .5)

# Las funciones y resultados son correctos 
