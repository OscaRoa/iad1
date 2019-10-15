############################
# Inferencia estadistica 2 #
#    Modelo geometrico     #
#  ensayos durante clase   #
############################

rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Funciones
source("distributions/pmf.R") # Funciones de masa de probabilidad

# Variable observable
x <- 0:30

# Datos/observaciones en ensayos de la clase
x_observada <- c(0,0,0,5,0,0,0,0,3,0)

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
  if (indice < 11) {
    color <- paste0("#", indice-1, "f0000", indice-1, "f")
  }
  else {
    color <- paste0("#", indice, "0000", "ff")
  }
  points(theta, P_theta_d_x, type='o', pch=indice-1, col = color, bg = "gray")
  
  # Verosimilitud
  points(theta, P_x_d_theta[, pos], type = 'l', pch = 16, col = "#0000ff44")
  
  plot(x, P_x_d_theta[x_obs, ], type = "o")
  points(x,  P_theta_d_x[x_obs], type = "o")
}

# Graficas tridimensional de las matrices
mat_plot <- rbind(1:3,
                  c(0, 0, 4))
#layout(matrix(1:2, ncol = 4))
x11(width = 12, height = 8)
layout(mat_plot)

plot(theta, P_theta,
     type = 'h', lwd = 3, col = "darkgreen")
mtext("Distribucion marginal de Theta", 3, line = 1)

persp(x = theta, y = x, z = P_x_d_theta,
      ticktype = 'detailed',
      col = "#0088ff",
      theta = 115)
mtext("Distribucion condicional P(X | theta)", 3, line = 1)

persp(x = theta, y = x, z = P_X_theta,
      ticktype = "detailed",
      col = "#ff88ff",
      theta = 115)
mtext("Distribucion conjunta P(X, theta)", 3, line = 1)

plot(x, P_x,
     type = 'h', lwd = 3, col = "darkblue")
mtext("Distribucion marginal de X", 3, line = 1)