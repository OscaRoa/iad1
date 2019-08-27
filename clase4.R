library(tidyverse)

theta <- 1/3 # Probabilidad de un exito
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

tibb_data <- tibble(x, p_dist)
ggplot(data = tibb_data, aes(x = x, y = p_dist)) +
  geom_bar(fill = "#7cc992", stat = "identity", width = .5)
