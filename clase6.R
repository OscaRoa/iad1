rm(list=ls())
library(tidyverse)

theta <- 0.01 # Probabilidad de un exito
n <- 50 # Limite superior

# Variable aleatoria
x <- 0:n

geom_dist <- function(va, prob) {
  # Funcion que calcula la distribucion geometrica
  # con una probabilidad 'prob' de que aparezca el primer exito
  # en una variable aleatoria
  # Devuelve un vector con los valores de la distribucion.
  distribution <- (prob)*(1 - prob)**(va)
  return(distribution)
}

expected_value <- function(x, p_dist) {
  # Esperanza matematica
  # La suma de cada uno de los valores de la distribucion 
  # por su valor en la distribucion de probabilidad
  return(sum(x*p_dist))
}

standard_deviation <- function(x, e_x, p_dist) {
  # Desviacion estandar
  # La suma del producto de cada valor de la distribucion de probabilidad
  # por la diferencia cuadrada de la esperanza y de cada valor de la dist.
  v <- sum(p_dist*(e_x - x)**2)  # Varianza
  return(v**(1/2))
}

# Guardar el vector de la distribucion para graficarlo posteriormente
p_dist <- geom_dist(x, theta)
esperanza <- expected_value(x, p_dist)
sd_x <- standard_deviation(x, esperanza, p_dist)

tibb_data <- tibble(x, p_dist)
ggplot(data = tibb_data, aes(x = x, y = p_dist)) +
  geom_bar(fill = "#7cc992", stat = "identity", width = .5) +
  ylim(0, max(p_dist) + .001) +
  labs(title = "Distribucion geometrica", x = "Ensayos", y = "Probabilidad") +
  theme(plot.title = element_text(hjust = 0.5))

plot(NULL,
     ylim = c(0,.5),
     xlim = c(0,n),
     type = 'h', lwd = 5, col = 'darkgreen')
lines(x, geom_dist(x, .5),
      type = 'o', pch = 15, # Type overplot, pch = points choice
      col = 'red'
      )
lines(x, geom_dist(x, .01),
      type = 'o', pch = 16,
      col = 'blue'
      )
lines(x, geom_dist(x, .25),
      type = 'o', pch = 17,
      col = 'darkgreen'
      )
legend(40, .4,
       legend = c("prob = 0.5",
                  "prob = 0.01",
                  "prob = 0.25"
                  ),
       col = c('red',
               'blue',
               'darkgreen'),
       pch = c(15,
               16,
               17)
       )

text(6, .10, paste(round(prob_aprob, 4)), col = "darkgreen", cex = 1.4)
text(5, .30, paste(round(prob_repr, 4)), col = "red", cex = 1.4)
mtext('Distribucion binomial', 3, line=2, col='blue')
mtext(paste('Aprobar un examen de ', n, ' aciertos con prob. exito de ', theta), 3, line=.5, col='red')


