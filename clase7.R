# Distribucion hipergeometrica

m <- 3 # Total exitos poblacion
n <- 5 # Total fracasos poblacion
k <- 6 # Tamaño de la extraccion (de la muestra) sin remplazo

x <- max(c(0, k-n)):min(c(k, m))


coef_bin <- function(a,b) {
  # Calcular coeficiente binomial
  # Devuelve ya sea un numero entero o un vector de enteros.
  return(factorial(a)/(factorial(b)*factorial(a-b)))
}
# coef_bin(3,2) Number operation
# coef_bin(n, x) Vector operation

hipergeom_dist <- function(x, m, n, k) {
  success <- coef_bin(m, x) # Posibles combinaciones de exito
  fail <- coef_bin(n, k - x) # Posibles combinaciones de fracasos
  total <- coef_bin(m + n, k) # Total de combinaciones
  return(success * fail / total)
}

p_dist <- hipergeom_dist(x, m, n, k)

plot(x, p_dist, type = "h", lwd = 2)
