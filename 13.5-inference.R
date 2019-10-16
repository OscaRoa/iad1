############################
# Inferencia estadistica 2 #
#    Modelo geometrico     #
#     tarea pendiente      #
############################
# A posteriori predictiva sobre X

rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Funciones
source("distributions/pmf.R") # Funciones de masa de probabilidad

# Variable observable
x <- 0:30

# Datos/observaciones en ensayos de la clase
x_observada <- c(0,0,0,5,0,0,0,0,3,0)

for (indice in 1:length(x_observada)) {
  theta <- seq(0, 1, 0.1)
  P_theta <- theta/sum(theta)
  
  if (indice == 1) {
    P_x <- x/sum(x)
  }
  else if (indice > 1) {
    P_x <- P_x_d_theta
  }
  # Matrices a llenar por cada nueva distribucion a priori
  P_x_d_theta <- array(dim = c(length(P_theta), length(x)))
  P_X_theta <- array(dim = dim(P_x_d_theta))
  P_x <- array(dim = length(x))
  
  for (theta_hyp in theta) {
    pos = which(theta == theta_hyp)
    P_x_d_theta[pos, ] <- geom_dist(x, theta_hyp)
    P_X_theta[pos, ] <- P_x_d_theta[pos, ]*P_theta[pos]
  }
  
  for (x_hyp in x) {
    pos = which(x == x_hyp)
    P_x[pos] <- sum(P_X_theta[, pos], na.rm = TRUE)
  }
}

# Distribucion de los datos observados
dist_x <- c(8, 0, 0, 1, 0, 1)/10

plot(x[x < 6], P_x[x < 6], type = 'o', ylim = c(0, 1))
points(x[x < 6], dist_x, type = 'o', col = "darkgreen")
title("Distribucion de las observaciones en X y Distribucion marginal de X")
