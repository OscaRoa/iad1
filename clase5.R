library(tidyverse)

theta <- 1/4 # Probabilidad de un exito
n <- 10 # Ensayos

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

prob_aprob <- sum(p_dist[x >= 6])
prob_repr <- sum(p_dist[x < 6])

# plot(x[x>=6],
#      p_dist[x>=6],
#      ylim = c(0,.35),
#      xlim = c(0,n),
#      type = 'h', lwd = 5, col = 'darkgreen')
# lines(x[x<6],
#       p_dist[x<6],
#       type = 'h', lwd = 5, col = 'red'
#       )
# text(6, .10, paste(round(prob_aprob, 4)), col = "darkgreen", cex = 1.4)
# text(5, .30, paste(round(prob_repr, 4)), col = "red", cex = 1.4)
# mtext('Distribucion binomial', 3, line=2, col='blue')
# mtext(paste('Aprobar un examen de ', n, ' aciertos con prob. exito de ', theta), 3, line=.5, col='red')


# Pendiente graficar en ggplot la grafica anterior
tibb_data <- tibble(x, p_dist)
graded_data <- mutate(
  tibb_data,
  passing = if_else(x >= (n/2 + 1), "pass", "no pass", "NA")
  )
ggplot(data = graded_data, aes(x = x, y = p_dist)) +
  geom_bar(mapping = aes(fill = passing), stat = "identity", width = .5) +
  ylim(0, max(p_dist) + .05) +
  scale_x_continuous(breaks = x) +
  labs(title = "Distribucion binomial", x = "Aciertos", y = "Probabilidad")
max(p_dist)
