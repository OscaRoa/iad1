############################
# Inferencia estadistica 2 #
#    modelo geometrico     #
############################

rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Funciones
source("distributions/pmf.R") # Funciones de masa de probabilidad

# Variable observable
x <- 0:50

# Datos/observaciones
x_observada <- c(0,0,1,2,4,3,3,1)

for (indice in 1:(length(x_observada))) {
  
  # Condicional para asignar la carga inicial de la distribucion a priori
  if(indice == 1) {
    theta <- seq(0, 1, 0.1)
    # P_theta <- c(.6,.2,.1,0,0,0,0,0,0,0,0)
    P_theta <- theta/sum(theta)
    plot(theta, P_theta, type = 'b', pch=16, ylim=c(0, 1))
  }
  # Condicional para las cargas posteriores con base en los datos observados
  else if (indice > 1) {
    P_theta <- P_theta_d_x
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
    P_x[pos] <- sum(P_X_theta[, pos], na.rm = T)
  }
  
  # posterior
  x_obs <- x_observada[indice]
  pos = which(x==x_obs)
  P_theta_d_x <- P_X_theta[, pos]/P_x[pos]
  if (indice < 10) {
    color <- paste0("#", indice-1, "f0000", indice, "f")
  }
  else {
    color <- paste0("#", indice, "0000", "ff")
  }
  points(theta, P_theta_d_x, type='o', pch=indice-1, col = color, bg = "gray")
  
  # Verosimilitud
  points(theta, P_x_d_theta[, pos], type = 'l', pch = 16, col = "#0000ff44")
}

# points(theta, P_theta_d_x, cex=1.5, col='red')
